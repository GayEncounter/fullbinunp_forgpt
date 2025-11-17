unit uXRayExport;

interface
uses fouramdl;

function ExportObject(const name : String; model : T4AModelHierrarhy; export_lwo : Boolean) : Boolean;

procedure ExportXRayLevel(map_name : String);

implementation
uses classes, sysutils, lwoexport, chunkedFile, vmath, Texture, 
	uScene, uEntity,
	uXRayExportOptions,
	uAprilFools;

//=========================================================//
//  Utilities                                              //
//=========================================================//

function IsVisible(m : T4AModelSimple) : Boolean;
begin
	IsVisible := not (
		(Copy(m.name, 1, 15) = 'collision_part_') or
		(m.name = 'ao') or
		(m.shader = 'special\invisible')
	);
end;

function IsDoubleside(const shader : String) : Boolean;
var
	params : String;
begin
	if Pos('@', shader) > 0 then
	begin
		params := Copy(shader, Pos('@', shader)+1);
		
		IsDoubleside := Pos('double_sided=1', params) > 0;
	end else
		IsDoubleside := False;
end;

function MakePath(const path : String) : Boolean;
begin
	if not DirectoryExists(path) then
	begin
		Result := ForceDirectories(path);
		if not Result then
			WriteLn('Failed to create directory ''', path, '''');
	end;
end;

function VectorStr(X, Y, Z : Single) : String;
var
	S : String;
begin
	WriteStr(S, X:1:6, ', ', Y:1:6, ', ', Z:1:6);
	Result := S;
end;

//=========================================================//
//  Object export functions                                //
//=========================================================//

function ExportObject(const name : String; model : T4AModelHierrarhy; export_lwo : Boolean) : Boolean;
var
	I, J : Longint;
	meshes : array of LWOMesh;
	
	mtlset : Longint;
	shader : String;
	
	textures : array of String;
begin
	if Pos('@', name) > 0 then
		mtlset := model.GetMtlsetIndex(Copy(name, Pos('@', name)+1))
	else
		mtlset := -1;

	SetLength(meshes, 0);
	SetLength(textures, 0);
	
	for I := 0 to Length(model.meshes) - 1 do
	begin
		if not IsVisible(model.meshes[I]) then
			Continue;
		if Length(model.meshes[I].vertices) = 0 then // Exodus gotchas
			Continue;
			
		J := Length(meshes);
		SetLength(meshes, J+1);
		SetLength(textures, J+1);
			
		model.GetMaterialOverride(mtlset, model.meshes[I].name, textures[J], shader);
		
		if textures[J] = '' then
			textures[J] := model.meshes[I].texture;
			
		textures[J] := GetRealTextureName(textures[J]);
		
		if model.meshes[I].name <> '' then
			meshes[J].surface_name := PAnsiChar(model.meshes[I].name)
		else
			meshes[J].surface_name := nil;
			
		meshes[J].texture := PAnsiChar(textures[J]);
		meshes[J].engine_shader := 'default';
		meshes[J].compiler_shader := 'default';
		meshes[J].game_material := 'default';
		
		meshes[J].vertex_count := Length(model.meshes[I].vertices);
		meshes[J].points := @model.meshes[I].vertices[0].point;
		meshes[J].point_stride := Sizeof(T4AVertStatic);
		meshes[J].uvs := @model.meshes[I].vertices[0].tc;
		meshes[J].uv_stride := Sizeof(T4AVertStatic);
		
		meshes[J].tri_count := Length(model.meshes[I].indices) div 3;
		meshes[J].tris := Pointer(model.meshes[I].indices);
		meshes[J].tri_stride := 0;
		
		meshes[J].flags := MESH_FLIPUVS;
		
		if IsDoubleside(shader) then
			meshes[J].flags := meshes[J].flags or MESH_DOUBLESIDED;
	end;
	
	if Length(meshes) = 0 then
	begin
		WriteLn('empty model');
		Result := False;
	end else
	begin
		if export_lwo then
			Result := ExportLWO(PAnsiChar(name + '.lwo'), PLWOMesh(meshes), Length(meshes)) = 0
		else
			Result := lwoexport.ExportObject(PAnsiChar(name + '.object'), PLWOMesh(meshes), Length(meshes)) = 0;
	end
end;

function ExportSuperstaticObject(const name : String; level : T4ALevel; export_lwo : Boolean) : Boolean;
var
	I, J, K : Longint;
	meshes : array of LWOMesh;
	m : T4AModelRef;
	
	vstart : Longword;
	start : Longword;
	
	data : array of record
		texture : String;
		points : array of TVec3;
		uvs : array of TVec2;
		tris : array of Longword;
	end;
begin
	// merge by material
	SetLength(data, Length(level.materials));
	for I := 0 to Length(data) - 1 do
	begin
		data[I].texture := GetRealTextureName(level.materials[I].texture);
		
		SetLength(data[I].points, 0);
		SetLength(data[I].uvs, 0);
		SetLength(data[I].tris, 0);
	end;
	
	for I := 0 to Length(level.visuals) - 1 do
	begin
		if not (level.visuals[I] is T4AModelRef) then
			Continue;
			
		m := T4AModelRef(level.visuals[I]);
		J := m.shaderid;
		
		vstart := Length(data[J].points);
		SetLength(data[J].points, vstart + m.vertexcount);
		SetLength(data[J].uvs, vstart + m.vertexcount);
		
		for K := 0 to m.vertexcount - 1 do
		begin
			data[J].points[vstart+K] := level.vbuffer[m.vertexoffset+K].point;
			
			data[J].uvs[vstart+K].x := level.vbuffer[m.vertexoffset+K].tc.x / 1024.0;
			data[J].uvs[vstart+K].y := level.vbuffer[m.vertexoffset+K].tc.y / 1024.0;
		end;
		
		start := Length(data[J].tris);
		SetLength(data[J].tris, start + m.indexcount);
		for K := 0 to m.indexcount - 1 do
			data[J].tris[start+K] := vstart + level.ibuffer[m.indexoffset+K];
	end;
	
	SetLength(meshes, length(data));
	for I := 0 to Length(data) - 1 do
	begin
		meshes[I].surface_name := nil;
		meshes[I].texture := PAnsiChar(data[I].texture);
		meshes[I].engine_shader := 'default';
		meshes[I].compiler_shader := 'default';
		meshes[I].game_material := 'default';
		
		meshes[I].vertex_count := Length(data[I].points);
		meshes[I].points := Pointer(data[I].points);
		meshes[I].point_stride := Sizeof(TVec3);
		
		meshes[I].uvs := Pointer(data[I].uvs);
		meshes[I].uv_stride := Sizeof(TVec2);
		
		meshes[I].tri_count := Length(data[I].tris) div 3;
		meshes[I].tris := @data[I].tris[0];
		meshes[I].tri_stride := 0;
		
		meshes[I].flags := MESH_32BITINDICES or MESH_FLIPUVS;
	end;
	
	if Length(meshes) = 0 then
	begin
		WriteLn('empty model');
		Result := False;
	end else
	begin
		if export_lwo then
			Result := ExportLWO(PAnsiChar(name + '.lwo'), PLWOMesh(meshes), Length(meshes)) = 0
		else
			Result := lwoexport.ExportObject(PAnsiChar(name + '.object'), PLWOMesh(meshes), Length(meshes)) = 0
	end
end;

//=========================================================//
//  X-Ray scene export stuff                               //
//=========================================================//

type
	TSceneObject = class
		reference_name : String;
		transform : TMatrix;
		
		constructor Create(const ref : String); overload;
		constructor Create(const mat : TMatrix; const ref : String); overload;
	end;
	
constructor TSceneObject.Create(const ref : String);
begin
	inherited Create;
	reference_name := ref;
	Identity(transform);
end;
	
constructor TSceneObject.Create(const mat : TMatrix; const ref : String);
begin
	inherited Create;
	reference_name := ref;
	transform := mat;
end;
	
procedure ExportLTX(const filename : String; objects : TStringList);
var
	f : Text;
	I : Longint;
	O : TSceneObject;
	t, rot, scale : TVec3;
begin
	MakePath(filename);
	
	// main file
	Assign(f, filename + '.level');
	Rewrite(f);
	
	WriteLn(f, '[build_params]');
	WriteLn(f, '        light_jitter_samples             = 9');
	WriteLn(f, '        light_pixel_per_meter            = 10.000000');
	WriteLn(f, '        light_quality                    = 1');
	WriteLn(f, '        light_quality_reserved           = 0');
	WriteLn(f, '        light_rms                        = 4');
	WriteLn(f, '        light_rms_zero                   = 4');
	WriteLn(f, '        reserved_0                       = 0.500000');
	WriteLn(f, '        reserved_1                       = 1.000000');
	WriteLn(f, '        reserved_2                       = 1.000000');
	WriteLn(f, '        reserved_3                       = 150.000000');
	WriteLn(f, '        reserved_4                       = 0.040000');
	WriteLn(f, '        reserved_5                       = 0.850000');
	WriteLn(f, '        smooth_angle                     = 75.000000');
	WriteLn(f, '        weld_distance                    = 0.005000');
	WriteLn(f, ' ');
	WriteLn(f, '[camera]');
	WriteLn(f, '        hpb                              = -6.064811, -0.768989, 0.000000');
	WriteLn(f, '        pos                              = -6.992448, 6.671986, 2.980559');
	WriteLn(f, ' ');
	WriteLn(f, '[guid]');
	WriteLn(f, '        guid_g0                          = 5723503490787537849');
	WriteLn(f, '        guid_g1                          = 15527435521999281576');
	WriteLn(f, ' ');
	WriteLn(f, '[level_options]');
	WriteLn(f, '        bop                              = ""');
	WriteLn(f, '        game_type                        = 30');
	WriteLn(f, '        level_path                       = mp_test');
	WriteLn(f, '        level_prefix                     = test');
	WriteLn(f, '        light_hemi_quality               = 3');
	WriteLn(f, '        light_sun_quality                = 3');
	WriteLn(f, '        map_version                      = 1.0');
	WriteLn(f, '        version                          = 12');
	WriteLn(f, '        version_bp                       = 9');
	WriteLn(f, ' ');
	WriteLn(f, '[level_tag]');
	WriteLn(f, '        create_time                      = 1220014361');
	WriteLn(f, '        owner                            = \\MEGATHREE\Usercheg');
	WriteLn(f, ' ');
	WriteLn(f, '[version]');
	WriteLn(f, '        value                            = 5');
	WriteLn(f, ' ');
	
	Close(f);
	
	// scene_object.part
	Assign(f, filename + '\scene_object.part');
	Rewrite(f);
	
	WriteLn(f, '[appendrandom]');
	WriteLn(f, '        AppendRandomMaxRotation          = 0.000000, 0.000000, 0.000000');
	WriteLn(f, '        AppendRandomMaxScale             = 1.000000, 1.000000, 1.000000');
	WriteLn(f, '        AppendRandomMinRotation          = 0.000000, 0.000000, 0.000000');
	WriteLn(f, '        AppendRandomMinScale             = 1.000000, 1.000000, 1.000000');
	WriteLn(f, '        AppendRandomObjects_size         = 0');
	WriteLn(f, ' ');
	WriteLn(f, '[guid]');
	WriteLn(f, '        guid_g0                          = 5723503490787537849');
	WriteLn(f, '        guid_g1                          = 15527435521999281576');
	WriteLn(f, ' ');
	WriteLn(f, '[main]');
	WriteLn(f, '        flags                            = 0');
	WriteLn(f, '        objects_count                    = ', objects.Count);
	WriteLn(f, '        version                          = 0');
	WriteLn(f, ' ');
	WriteLn(f, '[modif]');
	WriteLn(f, '        name                             =');
	WriteLn(f, '        time                             = 0');
	WriteLn(f, ' ');
	
	for I := 0 to objects.Count - 1 do
	begin
		O := objects.Objects[I] as TSceneObject;
		
		Decompose(O.transform, t, rot, scale);
	
		WriteLn(f, '[object_', I, ']');
		WriteLn(f, '        clsid                            = 2');
		WriteLn(f, '        co_flags                         = 0');
		WriteLn(f, '        flags                            = 0');
		WriteLn(f, '        name                             = ', objects[I]);
		WriteLn(f, '        position                         = ', VectorStr(t.x, t.y, t.z));
		WriteLn(f, '        reference_name                   = ', O.reference_name);
		WriteLn(f, '        rotation                         = ', VectorStr(rot.x, rot.y, rot.z));
		WriteLn(f, '        scale                            = ', VectorStr(scale.x, scale.y, scale.z));
		WriteLn(f, '        version                          = 18');
		WriteLn(f, ' ');
	end;
	
	Close(f);
end;

procedure ExportXR(filename : String; objects : TStringList);
var
	I : Longint;
	O : TSceneObject;
	t, rot, scale : TVec3;
	
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	
	w.OpenChunk($9DF3); // level file version
	w.WriteLongword(5);
	w.CloseChunk;
	
	// w.OpenChunk($7711); // compile options, not necessary
	// w.CloseChunk;
	
	w.OpenChunk($7712); // objects count (all kinds)
	w.WriteLongword(objects.Count);
	w.CloseChunk;
	
	w.OpenChunk($8002); // static objects
	
	w.OpenChunk(2); // static objects count
	w.WriteLongword(objects.Count);
	w.CloseChunk;
	
	w.OpenChunk(3); // objects itself
	
	for I := 0 to objects.Count - 1 do
	begin
		O := objects.Objects[I] as TSceneObject;
		
		w.OpenChunk(I);
		
		w.OpenChunk($7703); // object class
		w.WriteLongword(2); // - scene object
		w.CloseChunk;
		
		w.OpenChunk($7777); // object data
		
		w.OpenChunk($F906); // object flags
		w.WriteLongword($00000003);
		w.CloseChunk;
		
		w.OpenChunk($F907); // object name
		w.WriteStringZ(objects[I]);
		w.CloseChunk;
		
		Decompose(O.transform, t, rot, scale);
		
		w.OpenChunk($F903); // object transformation
		w.Write(t, Sizeof(t));
		w.Write(rot, Sizeof(rot));
		w.Write(scale, Sizeof(scale));
		w.CloseChunk;
		
		w.OpenChunk($0900); // static object version
		w.WriteWord($0011);
		w.CloseChunk;
		
		w.OpenChunk($0902); // library reference
		w.WriteLongword(0); // timestamp of .object file
		w.WriteLongword(0); // unknown
		w.WriteStringZ(O.reference_name);
		w.CloseChunk;
		
		w.OpenChunk($0905); // object motion, unused here
		w.WriteLongword(0);
		w.CloseChunk;
		
		w.CloseChunk; // end of object data
		
		w.CloseChunk;
	end;
	
	w.CloseChunk;
	
	w.OpenChunk($1001); // static objects version
	w.WriteWord(0);
	w.CloseChunk;
	
	w.OpenChunk($1003); // static objects flags
	w.WriteLongword(0);
	w.CloseChunk;
	
	// w.OpenChunk($1002); // append random parameters, not necessary
	// w.CloseChunk;
	
	w.CloseChunk; // end of static objects
	
	w.OpenChunk($7709); // camera position
	w.WriteSingle(0.0); w.WriteSingle(0.0); w.WriteSingle(0.0); // rotation
	w.WriteSingle(0.0); w.WriteSingle(3.0); w.WriteSingle(-10.0); // translate
	w.CloseChunk;
	
	w.OpenChunk($7710); // snap objects list
	w.WriteLongword(0);
	w.CloseChunk;		
	
	// save the file
	filename := filename + '.level';
	MakePath(ExtractFileDir(filename));
	w.SaveTo(filename);
	
	w.Free;
end;


//=========================================================//
//  Main export function                                   //
//=========================================================//

procedure ExportXRayLevel(map_name : String);
var
	use_ltx : Boolean;
	use_lwo : Boolean;
	objects_path : String;
	maps_path : String;
	
	I : Longint;
	E : TEntity;
	
	objects : TStringList;
	models : TStringList;
	
	name : String;
	path : String;
	level_name : String;
	
	idx : Longint;
begin
	//ShowBlueScreen;

	use_ltx := True;
	use_lwo := True;
	objects_path := 'H:\POLIGON\xray-cs-sdk\editors\rawdata\objects\';
	maps_path := 'H:\POLIGON\xray-cs-sdk\editors\maps\';
	
	if not XRayExportOptions(map_name, objects_path, maps_path, use_ltx, use_lwo) then
		Exit;
	
	objects := TStringList.Create;
	objects.OwnsObjects := True;
	
	// superstatic
	if Scene.level <> nil then
	begin
		name := 'levels\' + map_name + '\superstatic';
		path := objects_path + name;

		MakePath(ExtractFileDir(path));
		
		WriteLn('Exporting superstatic geometry...');
		if not ExportSuperstaticObject(path, Scene.level, use_lwo) then
			WriteLn('FAILED');
			
		// add scene object
		idx := objects.Add('superstatic');
		objects.Objects[idx] := TSceneObject.Create(name);
	end;
	
	if Scene.level2 <> nil then
	begin
		for I := 0 to Length(Scene.level2.sublevels) - 1 do
		begin
			level_name := Scene.level2.sublevels[I].filename;
			
			name := 'levels\' + map_name + '\' + level_name;
			path := objects_path + name;
	
			MakePath(ExtractFileDir(path));
			
			WriteLn('Exporting superstatic geometry part ''', level_name, '''...');
				
			if not ExportSuperstaticObject(path, Scene.level2.sublevels[I], use_lwo) then
				WriteLn('FAILED');
				
			// add scene object
			idx := objects.Add(level_name);
			objects.Objects[idx] := TSceneObject.Create(name);
		end;
	end;
	
	// decals
	if (Scene.decals <> nil) and (Length(Scene.decals.meshes) > 0) then
	begin
		name := 'levels\' + map_name + '\decals';
		path := objects_path + name;

		MakePath(ExtractFileDir(path));
		
		WriteLn('Exporting decals...');
		if not ExportObject(path, Scene.decals, use_lwo) then
			WriteLn('FAILED');
			
		// add scene object
		idx := objects.Add('decals');
		objects.Objects[idx] := TSceneObject.Create(name);
	end;
	
	// egeoms
	if (Scene.egeoms <> nil) and (Length(Scene.egeoms.meshes) > 0) then
	begin
		name := 'levels\' + map_name + '\egeoms';
		path := objects_path + name;

		MakePath(ExtractFileDir(path));
		
		WriteLn('Exporting egeoms...');
		if not ExportObject(path, Scene.egeoms, use_lwo) then
			WriteLn('FAILED');
			
		// add scene object
		idx := objects.Add('egeoms');
		objects.Objects[idx] := TSceneObject.Create(name);
	end;
	
	// entities
	models := TStringList.Create;
	for I := 0 to Scene.entities.Count - 1 do
	begin
		E := TEntity(Scene.entities[I]);
		
		if (E.model <> nil) and (E.model.model is T4AModelHierrarhy) {and (E.classname = 'STATICPROP')} then
		begin
			// add model
			idx := models.IndexOf(E.VisualName);
			if idx < 0 then
			begin
				idx := models.Add(E.VisualName);
				models.Objects[idx] := E.model.model;
			end;
			
			// add scene object
			idx := objects.Add(E.Name);
			objects.Objects[idx] := TSceneObject.Create(E.Matrix, E.VisualName);			
		end;
	end;
	
	WriteLn('Exporting objects...');
	for I := 0 to models.Count - 1 do
	begin
		WriteLn('..', models[I]);
		
		name := models[I];
		path := objects_path + name;
		MakePath(ExtractFileDir(path));
		
		if not ExportObject(path, T4AModelHierrarhy(models.Objects[I]), use_lwo) then
			WriteLn('FAILED');
	end;
	
	models.Free;
	
	WriteLn('Exporting X-Ray level...');
	
	if use_ltx then
		ExportLTX(maps_path + map_name, objects)
	else
		ExportXR(maps_path + map_name, objects);
	
	WriteLn('Done.');
	
	objects.Free;
end;

end.
