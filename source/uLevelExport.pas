unit uLevelExport;

interface
procedure ExportLevel(map_name : String);

implementation
uses sysutils, classes, fouramdl, vmath, uScene, uEntity, uExportScene, uLevelExportOptions, uAprilFools;

procedure ExportLight(export_scene : TExportScene; E : TEntity);
var
	l_type : Shortint;
	color : TVec4;
	l_brightness : Single;
	l_range : Single;
	l_angle : Single;
begin
	l_type := E.data.GetInt('ltype', -1, 'u8');
	
	color.x := 0; color.y := 0; color.z := 0; color.w := 0;
	color := E.data.GetVec4('color', color, 'color, vec4f');
	
	l_brightness := E.data.GetFloat('brightness', 1.0, 'fp32');
	l_range := E.data.GetFloat('range', 1.0, 'fp32');
	l_angle := E.data.GetFloat('spot_cone_angle', 1.0, 'angle, fp32');
	
	export_scene.AddLight(E.Name, E.Matrix, l_type, color, l_brightness, l_range, l_angle);
	WriteLN('Export light ', e.name);
end;

procedure ExportLevel(map_name : String);
var
	E : TEntity;
	L : T4ALevel;
	I : Longint;
	
	entities : TList;
	
	export_scene : TExportScene;
	model_id : Longint;
	matrix : TMatrix;
	
	format : String;
	filename : String;
	selectionOnly : Boolean;
	exportSuperstatic : Boolean;
	exportLights : Boolean;
begin
	//ShowBlueScreen;

	if not Assigned(Scene.entities) then
		Exit; // no level loaded
		
	selectionOnly := False;
	exportSuperstatic := True;
	exportLights := True;
	if not LevelExportOptions(filename, selectionOnly, exportSuperstatic, exportLights) then
		Exit;
		
	if selectionOnly then
		entities := Scene.GetSelectedList
	else
		entities := Scene.entities;
		
	export_scene := TExportScene.Create;
	
	for I := 0 to entities.Count - 1 do
	begin
		E := TEntity(entities[I]);

		model_id := -1;

		if (E.VisualName <> '') and (E.model <> nil) then
		begin
			if E.model.model is T4AModelHierrarhy then
				model_id := export_scene.AddModel(E.VisualName, T4AModelHierrarhy(E.model.model), E.mtlset)
			else if E.model.model is T4AModelSkeleton then
				model_id := export_scene.AddModel(E.VisualName, T4AModelSkeleton(E.model.model), E.mtlset);
		end;
		
		if model_id <> -1 then
			export_scene.AddObject(E.Name, E.Matrix, model_id);
	end;
	
  if exportLights then
  begin
		for I := 0 to entities.Count - 1 do
		begin
			E := TEntity(entities[I]);
			if E.classname = 'o_hlamp' then
				ExportLight(export_scene, E);
		end;  		
  end;
	
	if selectionOnly then
		entities.Free;
	
  if exportSuperstatic then 
  begin
  	if Scene.level <> nil then
  	begin
  		model_id := export_scene.AddLevel('__superstatic', Scene.level);
  		Identity(matrix);
			export_scene.AddObject('__superstatic', matrix, model_id);
  	end;
  	
  	if Scene.level2 <> nil then
  		for I := 0 to Length(Scene.level2.sublevels) - 1 do
  		begin
  			L := Scene.level2.sublevels[I];
  			model_id := export_scene.AddLevel(L.filename, L);
  			Identity(matrix);
				export_scene.AddObject(L.filename, matrix, model_id);		
  		end;
  end;
	
	if (export_scene.objects.Count > 0) or (export_scene.lights.Count > 0) then
	begin
		format := LowerCase(Copy(ExtractFileExt(filename), 2));
		export_scene.DoExport(format, filename);
	end;
	
	export_scene.Free;
end;

end.
