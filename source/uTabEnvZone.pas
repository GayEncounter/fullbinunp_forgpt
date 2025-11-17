unit uTabEnvZone;

interface
uses vmath, Iup;

function CreateTab : Ihandle;
procedure UpdateTab;

procedure CreateEnvZone(const pos : TVec3);

procedure DeselectAll;

procedure DeleteSelection;

implementation
uses sysutils, classes, Konfig, Engine, properties, uEditorUtils, uEnvZone, uScene, uLevelUndo, uLEOptions;

function list_zones_cb(ih : Ihandle; txt : PAnsiChar; item, state : Longint) : Longint; cdecl;
var
	zone : TEnvZone;
begin
	zone := Scene.EnvZoneByName(txt);
	if zone <> nil then
	begin
		DeselectAll;
		zone.Selected := True;
		UpdateTab;
		Redisplay;
	end;
	
	Result := IUP_DEFAULT;
end;

procedure property_before_change_cb(tree : Ihandle; prop : TSimpleValue) cdecl;
begin
	UndoSaveEnv('Property changed: ''' + prop.name + ' : ' + prop.vtype + '''');
end;

function property_edit_cb(tree : Ihandle; parent : TSection; prop : TSimpleValue) : Longint; cdecl;
var
	I : Longint;
	sect : TSection;
	list : TStringList;
	
	s : TStringValue;
	str : String;
	
	before_change_cb : TPropsBeforeChangeCb;
	after_change_cb  : TPropsAfterChangeCb;
begin
	Result := 1; // 0 - cancel, 1 - default editor, 2 - apply
	
	before_change_cb := TPropsBeforeChangeCb(IupGetAttribute(tree, 'PROPS_BEFORE_CHANGE_CB'));
	after_change_cb := TPropsAfterChangeCb(IupGetAttribute(tree, 'PROPS_AFTER_CHANGE_CB'));
	
	if (prop.vtype = 'choose') and (prop.name = 'sound_eff') then
	begin
		sect := Engine.GetSoundParams().root.GetSect('sound_effects');
		
		list := TStringList.Create;
		for I := 1 to sect.ParamCount-1 do
			list.Add((sect.GetParam(I) as TSection).GetStr('name'));
		
		s := parent.GetParam('sound_eff', 'stringz') as TStringValue;
		str := s.str;
		
		if EditChoose(str, False, list, 'Select sound effect') then
		begin
			if Assigned(before_change_cb) then before_change_cb(tree, s);
			
			s.str := str;
			properties.UpdateCaption(tree, s);
			
			if Assigned(after_change_cb) then after_change_cb(tree, s);
			
			Result := 2
		end else
			Result := 0;
			
		list.Free;
	end;
	
	if (prop.vtype = 'choose_array, str_shared') and (prop.name = 'random_sounds') then
	begin
		sect := Engine.GetSoundParams().root.GetSect('sound_schemes').GetSect('environment');
		
		list := TStringList.Create;
		for I := 0 to sect.ParamCount-1 do
			if (sect.GetParam(I) is TSection) then
				list.Add(sect.GetParam(I).name);
		
		s := parent.GetParam('random_sounds', 'stringz') as TStringValue;
		str := s.str;
		
		if EditChooseArray(str, list, 'Select random sounds') then
		begin
			if Assigned(before_change_cb) then before_change_cb(tree, s);
			
			s.str := str;
			properties.UpdateCaption(tree, s);
			
			if Assigned(after_change_cb) then after_change_cb(tree, s);

			Result := 2
		end else
			Result := 0;
			
		list.Free;
	end;
	
	if (prop.vtype = 'choose') and (prop.name = 'sound_echo') then
	begin
		sect := Engine.GetSoundParams().root.GetSect('echos');
		
		list := TStringList.Create;
		for I := 1 to sect.ParamCount-1 do
			list.Add((sect.GetParam(I) as TSection).GetStr('name'));
		
		s := parent.GetParam('sound_echo', 'stringz') as TStringValue;
		str := s.str;
		
		if EditChoose(str, False, list, 'Select sound echo') then
		begin
			if Assigned(before_change_cb) then before_change_cb(tree, s);
			
			s.str := str;
			properties.UpdateCaption(tree, s);
			
			if Assigned(after_change_cb) then after_change_cb(tree, s);
			
			Result := 2
		end else
			Result := 0;
			
		list.Free;
	end;
end;

function btn_add_layer_cb(ih : Ihandle) : Longint; cdecl;
var
	selected : TEnvZoneArray;
	name : array[0..255] of Char;
	template : TTextKonfig;
	data : TSection;
begin
	StrPCopy(@name[0], 'new_layer');
	selected := Scene.GetSelectedEZ;
	
	template := TTextKonfig.Create;
	case Scene.GetVersion of
		sceneVer2033:  template.LoadFromFile('editor_data\env_zone\2033_layer.txt');
		sceneVerLL:    template.LoadFromFile('editor_data\env_zone\ll_layer.txt');
		sceneVerRedux: template.LoadFromFile('editor_data\env_zone\redux_layer.txt');
		else           FreeAndNil(template);
	end;
	
	if template <> nil then
	begin
		if IupGetParam('Add layer', nil, nil, 'Name: %s'#10, @name) = 1 then
		begin
			if selected[0].GetLayer(name) = nil then
			begin
				data := template.root.Copy as TSection;
				data.name := name;
				(data.GetParam('name', 'stringz') as TStringValue).str := name;
				
				UndoSaveEnv('Add layer ''' + name + '''');
				selected[0].AddLayer(data);
			end else
				ShowError('Layer ''' + name + ''' already exist!');
		end;
		
		template.Free;
	end;
	
	UpdateTab;
	
	Result := IUP_DEFAULT;
end;

function btn_remove_layer_cb(ih : Ihandle) : Longint; cdecl;
var
	selected : TEnvZoneArray;
	layers : TSection;
	list : TStringList;
	I : Longint;
begin
	selected := Scene.GetSelectedEZ;
	layers := selected[0].data.GetSect('layers');
	list := TStringList.Create;
	
	for I := 0 to layers.ParamCount - 1 do
		if layers.GetParam(I) is TSection then
			list.Add(layers.GetParam(I).name);
			
	I := iup.ListDialog('Remove layer', list, 0, 40,40);
	if I <> -1 then
	begin
		UndoSaveEnv('Remove layer ''' + list[I] + '''');
		selected[0].RemoveLayer(list[I]);
	end;
		
	list.Free;
	
	UpdateTab;

	Result := IUP_DEFAULT;
end;

function CreateTab : Ihandle;
var
	fr_list : Ihandle;
	list : Ihandle;

	fr_zone : Ihandle;
	t_props_env : Ihandle;
	
	btn_add_layer : Ihandle;
	btn_remove_layer : Ihandle;
begin
	list := IupList(nil);
	IupSetAttribute(list, 'NAME', 'LIST_ENVZONES');
	IupSetAttribute(list, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(list, 'VISIBLELINES', '15');
	IupSetCallback(list, 'ACTION', @list_zones_cb);
	
	fr_list := IupFrame(IupVbox(list, nil));
	IupSetAttribute(fr_list, 'TITLE', 'List');

	if uLEOptions.props_two_column then
	begin
		t_props_env := IupFlatTree;
		IupSetAttribute(t_props_env, 'EXTRATEXTWIDTH', '200');
	end else
		t_props_env := IupTree;

	IupSetAttributes(t_props_env, 'NAME=TREE_PROPS_ENV, RASTERSIZE=200x');
	IupSetCallback(t_props_env, 'PROPS_EDIT_CB', @property_edit_cb);
	IupSetCallback(t_props_env, 'PROPS_BEFORE_CHANGE_CB', @property_before_change_cb);
	
	btn_add_layer := iup.Button('Add layer', @btn_add_layer_cb);
	btn_remove_layer := iup.Button('Remove layer', @btn_remove_layer_cb);
	
	fr_zone := IupFrame(IupVBox(
		IupSetAttributes(IupHBox(btn_add_layer, btn_remove_layer, nil), 'MARGIN=0x0'), 
		t_props_env, 
		nil
	));
	
	IupSetAttribute(fr_zone, 'NAME', 'FRAME_ENVZONE');
	IupSetAttribute(fr_zone, 'TITLE', 'Zone');
	IupSetAttribute(fr_zone, 'VISIBLE', 'NO');
	
	Result := IupVBox(
		fr_list,
		fr_zone,
		nil
	);
end;

procedure UpdateTab;
var
	list : Ihandle;
	fr_zone : Ihandle;
	t : Ihandle;
	
	I : Longint;
	selected : TEnvZoneArray;
begin
	list := IupGetDialogChild(MainDialog, 'LIST_ENVZONES');
	fr_zone := IupGetDialogChild(MainDialog, 'FRAME_ENVZONE');
	t := IupGetDialogChild(MainDialog, 'TREE_PROPS_ENV');
	
	IupSetAttribute(list, 'REMOVEITEM', 'ALL');
	if Assigned(Scene.env_zones) then
		for I := 0 to Scene.env_zones.Count - 1 do
			IupSetAttribute(list, 'APPENDITEM', @TEnvZone(Scene.env_zones[I]).Name[1]);
			
	selected := Scene.GetSelectedEZ;
			
	if Length(selected) = 1 then
	begin
		SetupProperties(t, selected[0].data);
		IupSetAttribute(fr_zone, 'VISIBLE', 'YES');
	end else
		IupSetAttribute(fr_zone, 'VISIBLE', 'NO');
end;

function _IsNameFree(const name : String) : Boolean;
begin
	Result := Scene.EnvZoneByName(name) = nil;
end;

procedure CreateEnvZone(const pos : TVec3);
var
	template : TTextKonfig;
	data : TSection;
	zone : TEnvZone;
	mat : TMatrix;
begin
	template := TTextKonfig.Create;
	
	case Scene.GetVersion of
		sceneVer2033:  template.LoadFromFile('editor_data\env_zone\2033.txt');
		sceneVerLL:    template.LoadFromFile('editor_data\env_zone\ll.txt');
		sceneVerRedux: template.LoadFromFile('editor_data\env_zone\redux.txt');
		else           FreeAndNil(template);
	end;
	
	if template <> nil then
	begin
		data := template.root.Copy as TSection;
		data.name := 'env_zone';
		
		zone := TEnvZone.Create(Scene.ph_scene, data);
		zone.Name := GenerateName('env_zone', _IsNameFree);
		
		Translate(mat, pos);
		zone.Transform(mat);
		
		Scene.env_zones.Add(zone);
		
		UpdateTab;
	end;
end;

procedure DeselectAll;
var
	z : TEnvZoneArray;
	I : Longint;
begin
	z := Scene.GetSelectedEZ;
	for I := 0 to Length(z) - 1 do
		z[I].Selected := False;
end;

procedure DeleteSelection;
var
	selected : TEnvZoneArray;
	I : Longint;
begin
	selected := Scene.GetSelectedEZ;
	for I := 0 to Length(selected) - 1 do
	begin
		Scene.env_zones.Remove(selected[I]);
		selected[I].Free;
	end;
	
	UpdateTab;
end;

end.
