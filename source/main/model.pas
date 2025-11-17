program model;
uses chunkedFile, sysutils, classes, vmath, fouramdl, fgl, OGF, windows,
		 nxcform, OGFImport, LMap;

type
	TLevelFlags = set of (lfSkipMU, lfUseLMap, lfXBox, lfLastLight, lfRedux);
	TTargetVersion = (tv2033, tvBuild15102012, tvBuild03122012, tvLastLight, tvRedux, tvArktika1, tvExodus);

var
	shaderbytexture, 
	materialbytexture,
	collisionbyname : TFPGMap<String,String>;

procedure FromOGFTo4A(srcfile, destfile : String);
var
	mdl : T4AModelHierrarhy;
	w : TMemoryWriter;

	I : Longint;
	m : T4AModelSimple;
	sh, mat : Integer;
begin
	mdl := ImportOGF(srcfile);

	Writeln('surfaces: ', Length(mdl.meshes));
	for I := 0 to Length(mdl.meshes) - 1 do
	begin
		m := mdl.meshes[I];
		Write(m.texture, ' ');
		Write('vertex count: ', Length(m.vertices), ' ');
		Write('face count: ', Length(m.indices) div 3);
		Writeln;
		
		sh := shaderbytexture.IndexOf(m.texture);
		if (sh <> -1) and (shaderbytexture.Data[sh] <> '<none>') then
			m.shader := shaderbytexture.Data[sh]
		else
			m.shader := 'geometry\default';
	
		mat := materialbytexture.IndexOf(m.texture);
		if (mat <> -1) and (materialbytexture.Data[mat] <> '<none>') then
			m.material := materialbytexture.Data[mat]
		else
			m.material := 'materials\wood';		
	end;

	w := TMemoryWriter.Create;
	mdl.Save(w);
	w.SaveTo(destfile);
	w.Free;
end;

procedure CopyVertices(src : P4AVertStatic; dest : POGFVertStatic; count : Longword); overload;
begin
	while count > 0 do
	begin
		dest^.point := src^.point;
		UnpackNormal(dest^.normal, src^.normal);
		dest^.tc := src^.tc;
		Inc(src);
		Inc(dest);
		Dec(count);
	end;
end;

procedure From4AToOGF(src, dst : String);
var
	I : Integer;
	
	r : TMemoryReader;
	w : TMemoryWriter;

	mdl : T4AModelHierrarhy;
	ogf : TOGFModelHierrarhy;

	s : T4AModelSimple;
	m : TOGFModelSimple;
begin
	r := TMemoryReader.CreateFromFile(src);

	mdl := T4AModelHierrarhy.Create;
	mdl.Load(r);
	r.Free;

	ogf := TOGFModelHierrarhy.Create;
	ogf.version := 4;
	ogf.modeltype := OGF_MT_HIERRARHY;
	ogf.bbox := mdl.bbox;
	ogf.bsphere := mdl.bsphere;

	SetLength(ogf.meshes, Length(mdl.meshes));
	Writeln('surfaces: ', Length(mdl.meshes));
	for I := 0 to Length(mdl.meshes) - 1 do
	begin
		s := mdl.meshes[I];
		Write(s.texture, ' ');
		Write('vertex count: ', Length(s.vertices), ' ');
		Write('face count: ', Length(s.indices) div 3);
		Writeln;

		m := TOGFModelSimple.Create;
		m.version := 4;
		m.modeltype := OGF_MT_NORMAL;
		m.bbox := s.bbox;
		m.bsphere := s.bsphere;
		m.texture := s.texture;
		m.shader := 'default';
		m.vertexformat := OGF_VF_STATIC;
		m.vertexcount := Length(s.vertices);
		SetLength(m.vertices, m.vertexcount*Sizeof(TOGFVertStatic));
		CopyVertices(P4AVertStatic(s.vertices), POGFVertStatic(m.vertices), m.vertexcount);
		SetLength(m.indices, Length(s.indices));
		Move(s.indices[0], m.indices[0], Length(m.indices)*Sizeof(word));

		ogf.meshes[I] := m;
	end;

	w := TMemoryWriter.Create;

	ogf.Save(w);
	w.SaveTo(dst);
	w.Free;

	ogf.Free;
	mdl.Free;
end;

procedure From4AToNxCform(src, dst : String; redux : Boolean);
var
	m : T4AModelHierrarhy;
	cf : TNxCform;
	I, mat, nam : Integer;

	w : TMemoryWriter;
begin
	m := T4AModelHierrarhy.Create;
	m.LoadFromFile(src);

	cf := TNxCform.Create(False);
	for I := 0 to Length(m.meshes) - 1 do
	begin
		mat := materialbytexture.IndexOf(m.meshes[I].texture);
		if (mat = -1) or (materialbytexture.Data[mat] <> '<none>') then
		begin
			nam := collisionbyname.IndexOf(m.meshes[I].name);
			if (nam = -1) or (collisionbyname.Data[nam] <> '<none>') then
				cf.Add4AModel(m.meshes[I]);
		end
	end;
	
	w := TMemoryWriter.Create;
	if redux then
		cf.SaveRedux(w)
	else
		cf.Save(w);
	w.SaveTo(dst);
	w.Free;

	cf.Free;
	m.Free;
end;

procedure CopyVertices(src : P4AVertStatic; dest : P4AVertLevel; count : Longword); overload;
begin
	while count > 0 do
	begin
		dest^.point := src^.point;
		dest^.normal := src^.normal;
		dest^.tangent := src^.tangent;
		dest^.binormal := src^.binormal;
		dest^.tc.x := Trunc(src^.tc.x * 1024.0);
		dest^.tc.y := Trunc(src^.tc.y * 1024.0);
		dest^.lm.x := 0;
		dest^.lm.y := 0;
		Inc(src);
		Inc(dest);
		Dec(count);
	end;
end;

procedure From4AToLevel(l : T4ALevel; model : T4AModelHierrarhy; model_version : Byte);
var
	I : Longint;
	verts : array of T4AVertLevel;
	rm : T4AModelRef;
	m  : T4AModelSimple;
begin
	for I := 0 to Length(model.meshes) - 1 do
	begin
		m := model.meshes[I];

		rm := T4AModelRef.Create;
		rm.version := model_version;

		rm.shaderid := l.AddMaterial(m.shader, m.texture, m.material);
		rm.bbox := m.bbox;
		rm.bsphere := m.bsphere;

		SetLength(verts, Length(m.vertices));
		CopyVertices(P4AVertStatic(m.vertices), P4AVertLevel(verts), Length(verts));

		rm.vertexformat := MODEL_VF_LEVEL;
		rm.vertexoffset := l.AddVertexBuffer(verts[0], Length(verts));
		rm.vertexcount := Length(verts);

		rm.indexoffset := l.AddIndexBuffer(m.indices[0], Length(m.indices));
		rm.indexcount := Length(m.indices);

		l.AddVisual(rm);
	end;
end;

procedure CopyVertices(src : TD3DVertexBuffer; dest : P4AVertLevel; count : Longword; lmap_id : Shortint); overload;
var
	tc : TVec2;
	
	// lightmap
	offset : TVec2;
	scale  : TVec2;
begin
	while count > 0 do
	begin
		src.GetPosition(dest^.point);
		src.GetNormal(dest^.normal);
		src.GetTangent(dest^.tangent);
		src.GetBinormal(dest^.binormal);
		src.GetTexcoord1(tc);
		dest^.tc.x := Trunc(tc.x * 1024.0);
		dest^.tc.y := Trunc(tc.y * 1024.0);
		if lmap_id > 0 then
		begin
		{
			offset.x := ((lmap_id-1) mod 4) * 0.25;
			offset.y := ((lmap_id-1) div 4) * 0.25;
			scale.x := 0.25;
			scale.y := 0.25;
		}
		
			offset.x := 0;
			offset.y := 0;
			scale.x := 1;
			scale.y := 1;
			
			src.GetTexcoord2(tc);
			//WriteLn('U: ', tc.x:3:3, ' V: ', tc.y:3:3);
			
			dest^.lm.x := Trunc(((tc.x * scale.x) + offset.x) * 32768.0);
			dest^.lm.y := Trunc(((tc.y * scale.y) + offset.y) * 32768.0);
		end;

		if src.ntype <> 4 then
			dest^.normal := dest^.normal or ($7F shl 24);

		Inc(dest);
		Dec(count);
		src.NextVertex;
	end;
end;

procedure TransformMUVertices(v : P4AVertLevel; count : Longword; mu : TOGFModelMU);
var
	n : TVec3;
	ao : Single;
begin
	while count > 0 do
	begin
		ao := ((v^.normal and $FF000000) shr 24) / 255;
		ao := (ao * mu.c_scale[3] * 0.5) + mu.c_bias[3] * 0.5;
		if ao > 1.0 then ao := 1.0;
		if ao < 0.0 then ao := 0.0;
	
	
		Transform(v^.point, mu.xform);
		UnpackNormal(n, v^.normal);
		Transform33(n, mu.xform); Normalize(n);
		v^.normal := PackNormal(n) or (Trunc(ao * 255) shl 24);
		UnpackNormal(n, v^.tangent);
		Transform33(n, mu.xform); Normalize(n);
		v^.tangent := PackNormal(n) or (v^.tangent and $FF000000);
		UnpackNormal(n, v^.binormal);
		Transform33(n, mu.xform); Normalize(n);
		v^.binormal := PackNormal(n) or (v^.binormal and $FF000000);

		Inc(v);
		Dec(count);
	end;
end;

procedure ScaleAO(l : T4ALevel);
var
	I : Longint;
	ao : Byte;
begin
	for I := 0 to Length(l.vbuffer) - 1 do
	begin
		ao := (l.vbuffer[I].normal and $FF000000) shr 24;
		l.vbuffer[I].normal := (l.vbuffer[I].normal and $00FFFFFF) or (LMap.ScaleAO(ao) shl 24);
	end;
end;

procedure CreateSector(l : T4ALevel; model_version : Byte);
var
	I : Longword;
	s : T4AModelHierrarhyL;
begin
	s := T4AModelHierrarhyL.Create;
	s.version := model_version;
	SetLength(s.meshes, Length(l.visuals));
	for I := 0 to Length(l.visuals) - 1 do
	begin
		AABBMerge(s.bbox, l.visuals[I].bbox);
		s.meshes[I] := I;
	end;
	CalcBSphere(s.bsphere, s.bbox);
	l.AddSector(l.AddVisual(s));
end;

procedure GenerateCform(l : T4ALevel; fn : String; target : Longint);
var
	cf : TNxCform;
	w : TMemoryWriter;
	I, mt : Integer;
begin
	cf := TNxCform.Create(True);
	for I := 0 to Length(l.visuals) - 1 do
		if l.visuals[I] is T4AModelRef then
		begin
			mt := materialbytexture.IndexOf(l.materials[l.visuals[I].shaderid].texture);
			if (mt = -1) or (materialbytexture.Data[mt] <> '<none>') then
				cf.Add4AModel(T4AModelRef(l.visuals[I]), l);
		end;

	w := TMemoryWriter.Create;
	case target of
		0: cf.Save(w);
		1: cf.SaveLL(w);
		2: cf.SaveRedux(w);
	end;
	w.SaveTo(fn);

	w.Free;
	cf.Free;
end;

procedure FromModelToLevel(start, count : Integer; destdir : String; params : TLevelFlags; target_ver : TTargetVersion);
var
	l : T4ALevel;
	m : T4AModelHierrarhy;
	I : Integer;
	
	model_version : Byte;
	w : TMemoryWriter;
begin
	l := T4ALevel.Create;
	
	// create output directory
	if target_ver >= tvArktika1 then
		ForceDirectories(destdir + '\static')
	else
		ForceDirectories(destdir);
	
	// select version
	case target_ver of
		tv2033: begin
			model_version := 8;
		end;
		tvBuild15102012, 
		tvBuild03122012, 
		tvLastLight: begin
			model_version := 21;
		end;
		tvRedux: begin
			model_version := 22;
		end;
		tvArktika1: begin
			model_version := 34; // quite old
			l.version := LEVEL_VER_ARKTIKA1;
		end;
		tvExodus: begin
			model_version := 46;
			l.version := LEVEL_VER_EXODUS;
		end;
	end;

	for I := start to start+count do
	begin
		m := T4AModelHierrarhy.Create;
		m.LoadFromFile(ParamStr(I));
		From4AToLevel(l, m, model_version);
		m.Free;
	end;

	CreateSector(l, model_version);

	// cform
	WriteLn('generating nxcform...');
	if target_ver >= tvArktika1 then
		GenerateCform(l, destdir + '\static\level.nxcform33x', 2)
	else if target_ver >= tvRedux then
		GenerateCform(l, destdir + '\level.nxcform33x', 2)
	else if target_ver >= tvBuild15102012 then
		GenerateCform(l, destdir + '\level.nxcform_xbox', 1)
	else
		GenerateCform(l, destdir + '\level.nxcform_pc', 0);
		
	if target_ver >= tvLastLight then
		l.sound_occlusion_version := 5;

	// save level
	if target_ver >= tvArktika1 then
	begin
		l.filename := 'superstatic'; // shall we save each model as separate lightmap instead ?
		
		// save .pf
		w := TMemoryWriter.Create;
		w.WriteString(
			'*32,' + 
			IntToStr(Length(l.vbuffer)*Sizeof(T4AVertLevel)) + ',' + 
			IntToStr(Length(l.vbuffer)*Sizeof(T4AVertLevel)+ 40) + ',' +
			IntToStr(Length(l.ibuffer)*Sizeof(Word))
		);
		w.SaveTo(destdir + '\static\' + l.filename + '.pf');
		w.Free;
		
		// save level.lightmaps
		w := TMemoryWriter.Create;
		if target_ver >= tvExodus then
			w.WriteLongword(LEVEL_LIGHTMAPS_VER_1)
		else
			w.WriteLongword(LEVEL_LIGHTMAPS_VER_0);
			
		w.WriteLongword(2); // count of lightmaps
		w.WriteStringZ(l.filename); // name of 1st lightmap
		w.WriteStringZ('default_lightmap');
		
		if target_ver >= tvExodus then
			w.WriteWord($FFFF); // ?
			
		w.SaveTo(destdir + '\static\level.lightmaps');
		w.Free;
	end;

	ScaleAO(l);

	Writeln('vertex buffer size: ', Length(l.vbuffer)*Sizeof(T4AVertLevel));
	Writeln('index buffer size: ', Length(l.ibuffer)*Sizeof(Word));

	if target_ver >= tvArktika1 then
	begin
		l.SaveTo(destdir + '\static', False)
	end else
		l.SaveTo(destdir, lfXBox in params);
		
	l.Free;
end;

procedure FromXRayLevelTo4ALevel(srcdir, destdir : String; params : TLevelFlags; target_ver : TTargetVersion);
var
	l : T4ALevel;
	xl : TXRayLevel;

	I : Integer;
	model_version : Byte;
	
	root_id : Longint;
	n : String;
	
	// for Arktika.1 or Exodus
	part_name : String; 
	w : TMemoryWriter;
	
	function AddVisual(ogf : TOGFModel; sectorid : Longint) : Longint;
	var
		J : Integer;
		verts : array of T4AVertLevel;
		rm : T4AModelRef;
		hm : T4AModelHierrarhyL;
		m	: TOGFModelSimple;
		h : TOGFModelHierrarhy;	
	
		sh, mt : Integer;
		texture, shader, material : String;
		flags : Longword;
	
		icount, ioffset : Longword;
	
		comma : Longint;
		lmap_name : String;
		lmap_id : Integer;
	
		id : Longint;
		nadded : Longint;
		meshes : array of Longword;
	begin
		Result := -1;
		
		if ogf is TOGFModelSimple then
		begin
			m := TOGFModelSimple(ogf);
			
			if (lfSkipMU in params) and (m is TOGFModelMU) then
				Exit;
	
			// parse texture name
			texture := LowerCase(xl.materials[m.shaderid,2]);
			comma := Pos(',', texture);
			if comma > 0 then
			begin
				lmap_name := Copy(texture, comma+1);
				SScanf(lmap_name, 'lmap#%d_1', [@lmap_id]);
				
				if not (lfUseLMap in params) or (lmap_id > 16) then
					lmap_id := -1;
				
				texture := Copy(texture, 1, comma-1);
			end else
				lmap_id := -1;
			
			// guess material by texture name
			sh := shaderbytexture.IndexOf(texture);
			mt := materialbytexture.IndexOf(texture);
			if sh <> -1 then shader := shaderbytexture.Data[sh]
			else shader := 'geometry\default';
			if mt <> -1 then material := materialbytexture.Data[mt]
			else material := 'materials\wood';
	
			if shader = '<none>' then
				Exit;
			if material = '<none>' then
				material := 'materials\wood';
				
			flags := sectorid;
			if lmap_id > 0 then 
				flags := flags or $00010000;
	
			// add visual
			rm := T4AModelRef.Create;
	
			rm.version  := model_version;
			rm.shaderid := l.AddMaterial(shader, texture, material, flags);
			rm.bbox     := m.bbox;
			rm.bsphere  := m.bsphere;
			
			// duplicate material info for exodus ( why ?? )
			if target_ver >= tvExodus then
			begin
				rm.texture  := texture;
				rm.shader   := shader;
				rm.material := material;
				rm.name     := 'default';
				rm.flags    := flags;
				
				rm.unkwn_parm_1 := $166;
				rm.unkwn_parm_2 := 0.364;
			end;
			
			// - copy vertices
			SetLength(verts, m.geom_vcount);
			xl.vbuffers[m.geom_vbuffer].currentvert := m.geom_voffset;
			CopyVertices(xl.vbuffers[m.geom_vbuffer], P4AVertLevel(verts), m.geom_vcount, lmap_id);
			if m is TOGFModelMU then
				TransformMUVertices(P4AVertLevel(verts), Length(verts), m as TOGFModelMu);
	
			if (m.modeltype = OGF_MT_TREE_PM) or (m.modeltype = OGF_MT_TREE_ST) then
			begin
				// gotcha! texture coordinates of tree models quantized differently
				for J := 0 to Length(verts) - 1 do 
				begin
					verts[J].tc.x := verts[J].tc.x div 2;
					verts[J].tc.y := verts[J].tc.y div 2;
				end;
			end;
			
			rm.vertexformat := MODEL_VF_LEVEL;
			rm.vertexoffset := l.AddVertexBuffer(verts[0], m.geom_vcount);
			rm.vertexcount := m.geom_vcount;
	
			// - copy indices
			if m.modeltype = OGF_MT_TREE_PM then
			begin
				icount := xl.swibuffers[m.geom_swibuffer][0].nfaces*3;
				ioffset := m.geom_ioffset+xl.swibuffers[m.geom_swibuffer][0].offset;
			end else
			if Length(m.swis) = 0 then
			begin
				icount := m.geom_icount;
				ioffset := m.geom_ioffset;
			end else
			begin
				icount := m.swis[0].nfaces*3;
				ioffset := m.geom_ioffset+m.swis[0].offset;
			end;
			rm.indexoffset := l.AddIndexBuffer(xl.ibuffers[m.geom_ibuffer,ioffset], icount);
			rm.indexcount := icount;
			
			if target_ver >= tvExodus then
			begin
				// simplified model for shadowmap pass
				// not implemented
				rm.shadow_vcount := 0;
				rm.shadow_icount := 0;
			end;
	
			Result := l.AddVisual(rm);
		end else
		if ogf is TOGFModelHierrarhy then
		begin
			h := TOGFModelHierrarhy(ogf);
			
			// add visual
			hm := T4AModelHierrarhyL.Create;
			
			hm.version  := model_version;
			hm.shaderid := $FFFF;
			hm.bbox    := h.bbox;
			hm.bsphere := h.bsphere;
			
			SetLength(meshes, Length(h.meshesl));
			nadded := 0;
			
			for J := 0 to Length(h.meshesl) - 1 do
			begin
				id := AddVisual(xl.visuals[h.meshesl[J]], sectorid);
				if id >= 0 then
				begin
					meshes[nadded] := id;
					Inc(nadded);
				end;
			end;
			
			
			hm.unkwn_parm_1 := $166;
			hm.unkwn_parm_2 := 0.364;
			
			hm.meshes := meshes;
			
			if nadded > 0 then
				Result := l.AddVisual(hm)
			else
				hm.Free;
		end;
	end;
	
begin
	l := T4ALevel.Create;
	
	WriteLn('loading level ''' + srcdir + '''');
	xl := TXRayLevel.Create;
	xl.Load(srcdir);
	
	// create output directory
	if target_ver >= tvArktika1 then
		ForceDirectories(destdir + '\static')
	else
		ForceDirectories(destdir);
	
	// select version
	case target_ver of
		tv2033: begin
			model_version := 8;
		end;
		tvBuild15102012, 
		tvBuild03122012, 
		tvLastLight: begin
			model_version := 21;
		end;
		tvRedux: begin
			model_version := 22;
		end;
		tvArktika1: begin
			model_version := 34; // quite old
			l.version := LEVEL_VER_ARKTIKA1;
		end;
		tvExodus: begin
			model_version := 46;
			l.version := LEVEL_VER_EXODUS;
		end;
	end;
	
	// geometry & visuals	
	SetLength(l.sectors, 0);
	
	for I := 0 to Length(xl.sectors) - 1 do
	begin
		root_id := AddVisual(xl.visuals[xl.sectors[I].root], Length(l.sectors));
		if root_id >= 0 then
		begin
			SetLength(l.sectors, Length(l.sectors)+1);
			l.sectors[Length(l.sectors)-1] := root_id;
		end;
	end;

	// cform
	WriteLn('generating nxcform...');
	if target_ver >= tvArktika1 then
		GenerateCform(l, destdir + '\static\level.nxcform33x', 2)
	else if target_ver >= tvRedux then
		GenerateCform(l, destdir + '\level.nxcform33x', 2)
	else if target_ver >= tvBuild15102012 then
		GenerateCform(l, destdir + '\level.nxcform_xbox', 1)
	else
		GenerateCform(l, destdir + '\level.nxcform_pc', 0);
		
	// lightmap
	if lfUseLMap in params then
	begin
		WriteLN('converting lightmap...');
		if lfXBox in params then
			ConvertLightMap(srcdir, destdir + '\level.lmap_xbox', True)
		else
			ConvertLightMap(srcdir, destdir + '\level.lmap_pc', False);
	end;
	
	// portals
	SetLength(l.portals, Length(xl.portals));
	for I := 0 to Length(xl.portals) - 1 do
	begin
		SetLength(l.portals[I].points, xl.portals[I].points_count);
		Move(xl.portals[I].points[0], l.portals[I].points[0], xl.portals[I].points_count*Sizeof(TVec3));
		
		n := IntToStr(I);
		l.portals[I].name := 'portal' + StringOfChar('0', 4-Length(n)) + n;
	end;
	
	if target_ver >= tvLastLight then
		l.sound_occlusion_version := 5;

	// save level
	if target_ver >= tvArktika1 then
	begin
		part_name := ExtractFileName(srcdir);
		l.filename := part_name;
		
		// save .pf
		w := TMemoryWriter.Create;
		w.WriteString(
			'*32,' + 
			IntToStr(Length(l.vbuffer)*Sizeof(T4AVertLevel)) + ',' + 
			IntToStr(Length(l.vbuffer)*Sizeof(T4AVertLevel)+ 40) + ',' +
			IntToStr(Length(l.ibuffer)*Sizeof(Word))
		);
		w.SaveTo(destdir + '\static\' + part_name + '.pf');
		w.Free;
		
		// save level.lightmaps
		w := TMemoryWriter.Create;
		if target_ver >= tvExodus then
			w.WriteLongword(LEVEL_LIGHTMAPS_VER_1)
		else
			w.WriteLongword(LEVEL_LIGHTMAPS_VER_0);
			
		w.WriteLongword(2); // count of lightmaps
		w.WriteStringZ(part_name); // name of 1st lightmap
		w.WriteStringZ('default_lightmap');
		
		if target_ver >= tvExodus then
			w.WriteWord($FFFF); // ?
			
		w.SaveTo(destdir + '\static\level.lightmaps');
		w.Free;
	end;
	
	ScaleAO(l);

	Writeln('vertex buffer size: ', Length(l.vbuffer)*Sizeof(T4AVertLevel));
	Writeln('index buffer size: ', Length(l.ibuffer)*Sizeof(Word));

	if target_ver >= tvArktika1 then
	begin
		l.SaveTo(destdir + '\static', False)
	end else
		l.SaveTo(destdir, lfXBox in params);

	l.Free;
	xl.Free;
end;

procedure From4AToRaw(src, dst : String);
var
	I, J : Integer;

	r : TMemoryReader;
	w : TMemoryWriter;

	mdl : T4AModelHierrarhy;
	s : T4AModelSimple;

	points : array of TVec3;
	faces : array of Longint;
	
	function AddPoint(const p : TVec3) : Longint;
	var
		I : Integer;
	begin
		for I := 0 to Length(points) - 1 do
			if (points[I].x = p.x) and (points[I].y = p.y) and (points[I].z = p.z) then
			begin
				Result := I;
				Exit;
			end;
		
		I := Length(points);
		SetLength(points, I+1); 
		points[I] := p;
		
		Result := I;
	end;

	function AddFace(const p1, p2, p3 : TVec3) : Boolean;
	var
	//	e1, e2, n : TVec3;
		s : Longint;
	begin
		//e1.x := p2.x - p1.x;
		//e1.y := p2.y - p1.y;
		//e1.z := p2.z - p1.z;

		//e2.x := p3.x - p1.x;
		//e2.y := p3.y - p1.y;
		//e2.z := p3.z - p1.z;

		//n.x := e1.y*e2.z - e1.z*e2.y;
		//n.y := e1.z*e2.x - e1.x*e2.z;
		//n.z := e1.x*e2.y - e1.y*e2.x;

		//if n.y >= 0.0 then
		begin
			s := Length(faces);
			SetLength(faces, s+3);
			faces[s] := AddPoint(p1);
			faces[s+1] := AddPoint(p2);
			faces[s+2] := AddPoint(p3);
			AddFace := True;
		end //else
			//AddFace := False;
	end;
begin
	r := TMemoryReader.CreateFromFile(src);
	mdl := T4AModelHierrarhy.Create;
	mdl.Load(r);
	r.Free;

	SetLength(points, 0);
	SetLength(faces, 0);

	for I := 0 to Length(mdl.meshes) - 1 do
	begin
		s := mdl.meshes[I];

		for J := 0 to (Length(s.indices) div 3) - 1 do
		begin
			AddFace(
				s.vertices[s.indices[(J*3)]].point,
				s.vertices[s.indices[(J*3)+1]].point,
				s.vertices[s.indices[(J*3)+2]].point
			);
		end;
	end;
	mdl.Free;

	w := TMemoryWriter.Create;
	w.WriteLongint(1); // Version
	w.WriteLongint(Length(points));
	w.Write(points[0], Length(points)*Sizeof(TVec3));
	w.WriteLongint(Length(faces) div 3);
	w.Write(faces[0], Length(faces)*Sizeof(Longint));

	w.SaveTo(dst);
	w.Free;
end;

procedure ChangeModelVersion(f_in, f_out : String; new_version : Byte);
var
	r : TMemoryReader;
	m : T4AModel;
begin
	r := TMemoryReader.CreateFromFile(f_in);
	try
		m := Load4AModel(r);
		try
			SetModelVersion(m, new_version);
			m.SaveToFile(f_out);
		finally
			m.Free;
		end;
	finally
		r.Free;
	end;
end;

procedure LoadList(fn : String; list : TFPGMap<String,String>);
var
	f : TextFile;
	line, key, data : String;
	pos : Integer;
begin
	if FileExists(fn) then
	begin
		Assign(f, fn);
		Reset(f);
		while not EOF(f) do
		begin
			ReadLn(f, line);
			pos := AnsiPos('=', line);
			if pos <> 0 then
			begin
				data := Trim(Copy(line, pos+1));
				key := Trim(Copy(line, 1, pos-1));
				list.AddOrSetData(key, data);
			end;
		end;
		CloseFile(f);
	end;
end;

procedure LoadLists;
begin
	shaderbytexture := TFPGMap<String,String>.Create;
	materialbytexture := TFPGMap<String,String>.Create;
	collisionbyName := TFPGMap<String,String>.Create;

	LoadList('shadersbytextures.txt', shaderbytexture);
	LoadList('materialsbytextures.txt', materialbytexture);
	LoadList('collisionbyName.txt', collisionbyName);
end;

procedure UnloadLists;
begin
	FreeAndNil(shaderbytexture);
	FreeAndNil(materialbytexture);
	FreeAndNil(collisionbyName);
end;

var
	I : Integer;
	flags : TLevelFlags;
	target_ver : TTargetVersion;
	model_version : Byte;
	err : Word;
begin
	LoadLists;

	flags := [];
	target_ver := tv2033;
	model_version := MODEL_VER_2033;

	I := 1;
	while I <= ParamCount do
	begin
		// option switches
		if ParamStr(I) = '-nomu' then 
		begin
			Include(flags, lfSkipMU);
		end else
		if ParamStr(I) = '-lmap' then
		begin
			LMap.soc_version := False;
			Include(flags, lfUseLMap);
		end else
		if ParamStr(I) = '-lmap_soc' then 
		begin
			LMap.soc_version := True;
			Include(flags, lfUseLMap);
		end else
		if ParamStr(I) = '-ao_scale' then 
		begin
			if (ParamCount - I) > 0 then
			begin
				Val(ParamStr(I+1), LMap.ao_scale, err);
				if err <> 0 then
					WriteLn('Warning: ''', ParamStr(I+1), ''' is not a valid number');
				Inc(I,1);
			end else
				WriteLn('Missing argument for parameter -ao_scale');
		end else
		if ParamStr(I) = '-version' then 
		begin
			if (ParamCount - I) > 0 then
			begin
				Val(ParamStr(I+1), model_version, err);
				if err <> 0 then
					WriteLn('Warning: ''', ParamStr(I+1), ''' is not a valid number');
				Inc(I,1);
			end else
				WriteLn('Missing argument for parameter -version');
		end else
		if ParamStr(I) = '-xbox' then
		begin
			Include(flags, lfXBox);
		end else
		
		// target version switches
		if ParamStr(I) = '-2033' then
		begin
			target_ver := tv2033;
			model_version := MODEL_VER_2033
		end else
		if ParamStr(I) = '-build_15_10_2012' then
		begin
			target_ver := tvBuild15102012;
			model_version := MODEL_VER_LL
		end else
		if ParamStr(I) = '-build_3_12_2012' then
		begin
			target_ver := tvBuild03122012;
			model_version := MODEL_VER_LL
		end else
		if (ParamStr(I) = '-ll') or (ParamStr(I) = '-last_light') then
		begin
			Include(flags, lfLastLight);
			target_ver := tvLastLight;
			model_version := MODEL_VER_LL
		end else 
		if ParamStr(I) = '-redux' then 
		begin
			Include(flags, lfRedux);
			target_ver := tvRedux;
			model_version := MODEL_VER_REDUX
		end else
		if ParamStr(I) = '-arktika1' then
		begin
			target_ver := tvArktika1;
			model_version := MODEL_VER_ARKTIKA1
		end else
		if ParamStr(I) = '-exodus' then
		begin
			target_ver := tvExodus;
			model_version := MODEL_VER_EXODUS
		end else
		
		// operations
		if ParamStr(I) = '-ogf2model' then 
		begin
			if (ParamCount - I) >= 2 then
			begin
				FromOGFTo4A(ParamStr(I+1), ParamStr(I+2));
				Inc(I,2);
			end else
				WriteLn('Not enough parameters for -ogf2model');
		end else
		if ParamStr(I) = '-model2ogf' then 
		begin
			if (ParamCount - I) >= 2 then
			begin
				From4AToOGF(ParamStr(I+1), ParamStr(I+2));
				Inc(I,2);
			end else
				WriteLn('Not enough parameters for -model2ogf');
		end else
		if ParamStr(I) = '-model2nxcform_pc' then 
		begin
			if (ParamCount - I) >= 2 then
			begin
				From4AToNxCform(ParamStr(I+1), ParamStr(I+2), False);
				Inc(I,2);
			end else
				WriteLn('Not enough parameters for -model2nxcform_pc');
		end else
		if ParamStr(I) = '-model2nxcform33x' then 
		begin
			if (ParamCount - I) >= 2 then
			begin
				From4AToNxCform(ParamStr(I+1), ParamStr(I+2), True);
				Inc(I,2);
			end else
				WriteLn('Not enough parameters for -model2nxcform33x');
		end else
		if ParamStr(I) = '-model2level' then
		begin
			if (ParamCount - I) >= 2 then
			begin
				FromModelToLevel(I+1, ParamCount-(I+1)-1, ParamStr(ParamCount), flags, target_ver);
				Inc(I,2);
			end else
				WriteLn('Not enough parameters for -model2level');				
		end else
		if ParamStr(I) = '-level2level' then 
		begin
			if (ParamCount - I) >= 2 then
			begin
				FromXRayLevelTo4ALevel(ParamStr(I+1), ParamStr(I+2), flags, target_ver);
				Inc(I,2);
			end else
				WriteLn('Not enough parameters for -level2level');
		end else
		if ParamStr(I) = '-model2raw' then
		begin
			if (ParamCount - I) >= 2 then
			begin
				From4AToRaw(ParamStr(I+1), ParamStr(I+2));
				Inc(I,2);
			end else
				WriteLn('Not enough parameters for -model2raw');
		end else 
		if ParamStr(I) = '-changever' then
		begin
			if (ParamCount - I) >= 2 then
			begin
				ChangeModelVersion(ParamStr(I+1), ParamStr(I+2), model_version);
				Inc(I,2);
			end else
			if (ParamCount - I) >= 1 then
			begin
				ChangeModelVersion(ParamStr(I+1), ParamStr(I+1), model_version);
				Inc(I,1);
			end else
				WriteLn('Not enough parameters for -changever');
		end else 
			Writeln('Unknown parameter ', ParamStr(I));
		
		Inc(I);
	end;
	
	UnloadLists;
	
	if ParamCount = 0 then
	begin
		WriteLn('Usage:');
		WriteLn(#9'model -ogf2model infile outfile');
		WriteLn(#9'model -model2ogf infile outfile');
		WriteLn(#9'model -model2nxcform_pc infile outfile');
		WriteLn(#9'model -model2nxcform33x infile outfile');
		WriteLn(#9'model [-ll] [-redux] -model2level model1 model2 .. modelN leveldir');
		WriteLn(#9'model [-nomu] [-lmap] [-lmap_soc] [-ao_scale <n>] [-ll] [-redux] -level2level xrleveldir leveldir');
		WriteLn(#9'model <target or -version> -changever infile [outfile]');
		WriteLn(#9);
		WriteLn('Options:');
		WriteLn(#9'-nomu         : skip Multiple Usage models');
		WriteLn(#9'-lmap         : convert lightmaps (CS/CoP version) to level.lmap_pc');
		WriteLn(#9'-lmap_soc     : convert lightmaps (SoC version) to level.lmap_pc');
		WriteLn(#9'-ao_scale <n> : multiply ambient occlusion values by real number <n>');
		WriteLn(#9'-version <n>  : explicitly set model version to <n> (for -changever)');
		WriteLn(#9);
		WriteLn(#9'-2033     : target Metro 2033 (2010)');
		WriteLn(#9'-ll       : target Metro Last Light (2013)');
		WriteLn(#9'-redux    : target Metro Redux');
		WriteLn(#9'-arktika1 : target Arktika.1');
		WriteLn(#9'-exodus   : target Metro Exodus');
	end;
end.