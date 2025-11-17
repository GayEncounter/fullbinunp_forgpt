unit cform_utils;

interface
uses classes, fouramdl;

procedure LoadPhysics(const fn : String; ph : TList);

procedure MakePhysics(ph : TList; model : T4AModelHierrarhy; twosided : Boolean = False);
procedure MakePhysicsSkinned(ph : TList; model : T4AModelSkinned; twosided : Boolean = False);
procedure MakePhysicsSkeleton(ph : TList; model : T4AModelSkeleton; twosided : Boolean = False);
procedure MakePhysicsSoftbody(ph : TList; m : T4AModelSoftbody; twosided : Boolean = False);
procedure MakeLevelCform(meshes : TList; level : T4ALevel);
procedure MakeLevelCform2(meshes : TList; level : T4ALevel2);

implementation
uses sysutils, vmath, chunkedFile, PhysX;

procedure LoadPhysics(const fn : String; ph : TList);
var
	r : TMemoryReader;

	I : Longint;
	isLE : Boolean;
	magic, version, file_version, count : Longint;

	mtlid : Word;
	strlen : Longint;

	size : Longint;
	m : TPHTriMesh;

	function ReadLongint : Longint;
	begin
		if isLE then ReadLongint := LEtoN(r.ReadLongint)
		else ReadLongint := BEtoN(r.ReadLongint);
	end;
begin
	r := TMemoryReader.CreateFromFile(fn);
	try
		// looks like this byte indicates little-endianess, lmao
		// also may be platform (1 = PC, 0 = XBOX)
		isLE := (r.ReadByte = 1);

		magic := ReadLongint;
		version := ReadLongint;
		file_version := ReadLongint;
		count := ReadLongint;

		if magic <> $00001FDF then
			raise Exception.Create('"' + fn + '" is not nxcform');

		WriteLn('Cform version ', version, '; model count ', count);

		ph.Capacity := count;
		for I := 0 to count - 1 do
		begin
			if r.ReadByte = 1 then
			begin
				// material id in level file
				mtlid := r.ReadWord;
			end else
			begin
				// inline material
				strlen := ReadLongint;
				r.pos := r.pos + strlen;		// shader
				strlen := ReadLongint;
				r.pos := r.pos + strlen;		// texture
				strlen := ReadLongint;
				r.pos := r.pos + strlen;		// game material

				//if version >= 11 then
				if not isLE then
					r.pos := r.pos + 5;				// flags ??
			end;
			
			size := ReadLongint;

			m := PHLoadTriMesh(r.data+r.pos, size);
			if m = nil then
				WriteLn('PHLoadTriMeshLoad failed')
			else
				ph.Add(m);

			Inc(r.pos, size);
		end;
	finally
		r.Free;
	end;
end;

procedure MakePhysics(ph : TList; model : T4AModelHierrarhy; twosided : Boolean);
var
	p : TPHTriMesh;
	indices : array of Word;
	stride : Longint;

	I, J : Longint;
	m : T4AModelSimple;
begin
	for I := 0 to Length(model.meshes) - 1 do
	begin
		m := model.meshes[I];
		
		if twosided then
		begin
			SetLength(indices, Length(m.indices)*2);
			for J := 0 to (Length(m.indices) div 3) - 1 do
			begin
				indices[(J*6)	] := m.indices[(J*3)+2];
				indices[(J*6)+1] := m.indices[(J*3)+1];
				indices[(J*6)+2] := m.indices[(J*3)	];
				indices[(J*6)+3] := m.indices[(J*3)	];
				indices[(J*6)+4] := m.indices[(J*3)+1];
				indices[(J*6)+5] := m.indices[(J*3)+2];
			end;
		end else
		begin
			indices := m.indices;
		end;
		
		case m.vertexformat of
			MODEL_VF_STATIC:
				stride := Sizeof(T4AVertStatic);
			MODEL_VF_LEVEL:
				stride := Sizeof(T4AVertLevel); // задел на будущее :)
			else
				raise Exception.CreateFmt('Unknown vertex format %D, texture %S', [m.vertexformat, m.texture]);
		end;

		if Length(m.vertices) > 0 then
		begin
			p := PHCreateTrimesh(Length(m.vertices), Length(indices) div 3,
			@m.vertices[0], @indices[0], stride, 6,
			NX_MF_16_BIT_INDICES or NX_MF_HARDWARE_MESH);
			
			if p = nil then
				WriteLn('Error! PHCreateTriMesh failed ', m.texture);
				//raise Exception.Create('PHCreateTriMesh failed');			
		end else
			p := nil;

		ph.Add(p);
	end;
end;

procedure MakePhysicsSkinned(ph : TList; model : T4AModelSkinned; twosided : Boolean);
var
	p : TPHTriMesh;
	points : array of TVec3;
	indices : array of Word;

	I, J : Longint;
	m : T4AModelSkinnedMesh;
	
	scale : Single;
begin
	for I := 0 to Length(model.meshes) - 1 do
	begin
		m := model.meshes[I];
		
		if twosided then
		begin
			SetLength(indices, Length(m.indices)*2);
			for J := 0 to (Length(m.indices) div 3) - 1 do
			begin
				indices[(J*6)  ] := m.indices[(J*3)+2];
				indices[(J*6)+1] := m.indices[(J*3)+1];
				indices[(J*6)+2] := m.indices[(J*3)  ];
				indices[(J*6)+3] := m.indices[(J*3)  ];
				indices[(J*6)+4] := m.indices[(J*3)+1];
				indices[(J*6)+5] := m.indices[(J*3)+2];
			end;
		end else
		begin
			indices := m.indices;
		end;
			
		scale := m.GetPointScale;

		SetLength(points, Length(m.vertices));
		for J := 0 to Length(m.vertices) - 1 do
			with m.vertices[J].point do
			begin
				points[J].x := x * scale;
				points[J].y := y * scale;
				points[J].z := z * scale;
			end;
		
		if Length(points) > 0 then
		begin
			p := PHCreateTrimesh(Length(points), Length(indices) div 3,
			@points[0], @indices[0], 12, 6,
			NX_MF_16_BIT_INDICES or NX_MF_HARDWARE_MESH);
			
			if p = nil then
				WriteLn('Error! PHCreateTriMesh failed ', m.texture);
				//raise Exception.Create('PHCreateTriMesh failed');
		end else
			p := nil;

		ph.Add(p);
	end;
end;

procedure MakePhysicsSkeleton(ph : TList; model : T4AModelSkeleton; twosided : Boolean);
var
	I : Longint;
begin
	for I := 0 to Length(model.meshes[0]) - 1 do
		MakePhysicsSkinned(ph, model.meshes[0,I], twosided);
end;

procedure MakePhysicsSoftbody(ph : TList; m : T4AModelSoftbody; twosided : Boolean);
var
	p : TPHTriMesh;
	points : array of TVec3;
	indices : array of Word;

	I, J : Longint;
	
	center : TVec3;
begin
	center := m.vertices[0].point;
	for I := 1 to Length(m.vertices) - 1 do
	begin
		center.x := center.x + m.vertices[I].point.x;
		center.y := center.y + m.vertices[I].point.y;
		center.z := center.z + m.vertices[I].point.z;
	end;
	
	center.x := center.x / Length(m.vertices);
	center.y := center.y / Length(m.vertices);
	center.z := center.z / Length(m.vertices);
	
	SetLength(points, Length(m.vertices));
	for I := 0 to Length(m.vertices) - 1 do
	begin
		points[I].x := m.vertices[I].point.x - center.x;
		points[I].y := m.vertices[I].point.y - center.y;
		points[I].z := m.vertices[I].point.z - center.z;
	end;
		
	if twosided then
	begin
		SetLength(indices, Length(m.indices)*2);
		for J := 0 to (Length(m.indices) div 3) - 1 do
		begin
			indices[(J*6)  ] := m.indices[(J*3)+2];
			indices[(J*6)+1] := m.indices[(J*3)+1];
			indices[(J*6)+2] := m.indices[(J*3)  ];
			indices[(J*6)+3] := m.indices[(J*3)  ];
			indices[(J*6)+4] := m.indices[(J*3)+1];
			indices[(J*6)+5] := m.indices[(J*3)+2];
		end;
	end else
	begin
		indices := m.indices;
	end;
		
	if Length(points) > 0 then
	begin
		p := PHCreateTrimesh(Length(points), Length(indices) div 3,
		@points[0], @indices[0], 12, 6,
		NX_MF_16_BIT_INDICES or NX_MF_HARDWARE_MESH);
		
		if p = nil then
			WriteLn('Error! PHCreateTriMesh failed ', m.texture);
			//raise Exception.Create('PHCreateTriMesh failed');
	end else
		p := nil;

	ph.Add(p);
end;

procedure MakeLevelCform(meshes : TList; level : T4ALevel);
var
	m : T4AModelRef;
	r : Pointer;
	I : Longint;
begin
	for I := 0 to Length(level.visuals)-1 do
		if level.visuals[I] is T4AModelRef then
		begin
			m := T4AModelRef(level.visuals[I]);
			
			r := PHCreateTriMesh(m.vertexcount, m.indexcount div 3, 
			@level.vbuffer[m.vertexoffset], @level.ibuffer[m.indexoffset],
			SizeOf(T4AVertLevel), 6, 
			NX_MF_16_BIT_INDICES or NX_MF_HARDWARE_MESH);
			
			if r = nil then
				WriteLn('PHCreateTriMesh failed, texture ', level.materials[m.shaderid].texture)
			else
				meshes.Add(r);
		end;
end;

procedure MakeLevelCform2(meshes : TList; level : T4ALevel2);
var
	I : Longint;
begin
	for I := 0 to Length(level.sublevels)-1 do
		MakeLevelCform(meshes, level.sublevels[I]);
end;

end.
