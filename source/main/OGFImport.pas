unit OGFImport;

interface
uses fouramdl;

function ImportOGF(const path : String) : T4AModelHierrarhy;

implementation
uses vmath, chunkedFile, OGF, Sysutils;

procedure CopyVertices(src : POGFVertStatic; dest : P4AVertStatic; count : Longword); overload;
begin
	while count > 0 do
	begin
		dest^.point := src^.point;
		dest^.normal := PackNormal(src^.normal);
		// calculate tangent, binormal ?
		dest^.tc := src^.tc;
		Inc(src);
		Inc(dest);
		Dec(count);
	end;
end;

procedure CopyVertices(src : POGFVertSkin1; dest : P4AVertStatic; count : Longword); overload;
begin
	while count > 0 do
	begin
		dest^.point := src^.point;
		dest^.normal := PackNormal(src^.normal);
		dest^.tangent := PackNormal(src^.tangent);
		dest^.binormal := PackNormal(src^.binormal);
		dest^.tc := src^.tc;
		Inc(src);
		Inc(dest);
		Dec(count);
	end;
end;

procedure CopyVertices(src : POGFVertSkin2; dest : P4AVertStatic; count : Longword); overload;
begin
	while count > 0 do
	begin
		dest^.point := src^.point;
		dest^.normal := PackNormal(src^.normal);
		dest^.tangent := PackNormal(src^.tangent);
		dest^.binormal := PackNormal(src^.binormal);
		dest^.tc := src^.tc;
		Inc(src);
		Inc(dest);
		Dec(count);
	end;
end;

procedure CopyVertices(src : POGFVertSkin3; dest : P4AVertStatic; count : Longword); overload;
begin
	while count > 0 do
	begin
		dest^.point := src^.point;
		dest^.normal := PackNormal(src^.normal);
		dest^.tangent := PackNormal(src^.tangent);
		dest^.binormal := PackNormal(src^.binormal);
		dest^.tc := src^.tc;
		Inc(src);
		Inc(dest);
		Dec(count);
	end;
end;

procedure CopyVertices(src : POGFVertSkin4; dest : P4AVertStatic; count : Longword); overload;
begin
	while count > 0 do
	begin
		dest^.point := src^.point;
		dest^.normal := PackNormal(src^.normal);
		dest^.tangent := PackNormal(src^.tangent);
		dest^.binormal := PackNormal(src^.binormal);
		dest^.tc := src^.tc;
		Inc(src);
		Inc(dest);
		Dec(count);
	end;
end;

procedure SetAO(v : P4AVertStatic; count : Longword);
begin
	while count > 0 do
	begin
		v^.normal := v^.normal or ($7F shl 24);
		Inc(v);
		Dec(count);
	end;
end;

procedure ConvertMesh(src : TOGFModelSimple; m : T4AModelSimple);
begin
	m.bbox := src.bbox;
	m.bsphere := src.bsphere;

	m.texture := LowerCase(src.texture);
	m.shader := 'geometry\default';
	m.material := 'materials\wood';

	m.vertexformat := MODEL_VF_STATIC;
	SetLength(m.vertices, src.vertexcount);
	case src.vertexformat of
		OGF_VF_STATIC:
			CopyVertices(POGFVertStatic(src.vertices), P4AVertStatic(m.vertices), Length(m.vertices));
		OGF_VF_SKIN1, 1:
			CopyVertices(POGFVertSkin1(src.vertices), P4AVertStatic(m.vertices), Length(m.vertices));
		OGF_VF_SKIN2, 2:
			CopyVertices(POGFVertSkin2(src.vertices), P4AVertStatic(m.vertices), Length(m.vertices));
		3:
			CopyVertices(POGFVertSkin3(src.vertices), P4AVertStatic(m.vertices), Length(m.vertices));
		4:
			CopyVertices(POGFVertSkin4(src.vertices), P4AVertStatic(m.vertices), Length(m.vertices));
		else
			raise Exception.CreateFmt('Unsupported OGF vertex format 0x%8x', [src.vertexformat]);
	end;

	if Length(src.swis) = 0 then
	begin
		SetLength(m.indices, Length(src.indices));
		Move(src.indices[0], m.indices[0], Length(m.indices)*Sizeof(Word));
	end else
	begin
		SetLength(m.indices, src.swis[0].nfaces*3);
		Move(src.indices[src.swis[0].offset], m.indices[0], Length(m.indices)*Sizeof(Word));
	end;
end;

function ImportOGF(const path : String) : T4AModelHierrarhy;
var
	mdl : T4AModelHierrarhy;
	ogf : TOGFModel;
	ogfh : TOGFModelHierrarhy;

	r : TMemoryReader;

	I : Longint;
	m : T4AModelSimple;
begin
	r := TMemoryReader.CreateFromFile(path);
	try
		ogf := LoadOGF(r);
		if ogf = nil then
			raise Exception.Create('Unsupported model type');
	finally
		r.Free;
	end;

	mdl := T4AModelHierrarhy.Create;
	try
		mdl.bbox := ogf.bbox;
		mdl.bsphere := ogf.bsphere;
	
		if ogf is TOGFModelHierrarhy then
		begin
			ogfh := TOGFModelHierrarhy(ogf);
			SetLength(mdl.meshes, Length(ogfh.meshes));
			for I := 0 to Length(mdl.meshes) - 1 do
				mdl.meshes[I] := nil;
				
			for I := 0 to Length(ogfh.meshes) - 1 do
			begin
				m := T4AModelSimple.Create;
				ConvertMesh(ogfh.meshes[I] as TOGFModelSimple, m);
				SetAO(P4AVertStatic(m.vertices), Length(m.vertices));
				mdl.meshes[I] := m;
			end;
		end else
		begin
			SetLength(mdl.meshes, 1);
			m := T4AModelSimple.Create;
			ConvertMesh(ogf as TOGFModelSimple, m);
			SetAO(P4AVertStatic(m.vertices), Length(m.vertices));
			mdl.meshes[0] := m;
		end;
		
		Result := mdl;
	except
		on E: Exception do
		begin
			mdl.Free;
			raise;
		end;
	end;
end;

end.