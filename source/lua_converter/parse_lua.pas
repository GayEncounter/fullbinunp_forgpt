program parse_lua;
uses sysutils, classes, LUATable, Konfig, ConverterBase, ConverterEntities, ConverterScripts, vmath, levelbin;

procedure ConvertStartup(l : TLuaTable; k : TSection);
var
	weather_preset : String;
	arr, elem : TSection;
	tbl, item : TLuaTable;
	I : Longint;
	vec : TVec3;
begin
	weather_preset := l['weather_preset'].AsString;
	
	// where to obtain these values ?
	k.AddInt('game_time', 136800024, 'u32');
	k.AddFloat('dc_prev_speed', 1.0);
	k.AddFloat('dc_target_speed', 1.0);
	k.AddFloat('dc_curr_speed', 1.0);
	k.AddInt('dc_time_0', 0, 's32');
	k.AddInt('dc_time_1', 0, 's32');
	k.AddStr('main_preset_name', weather_preset);
	k.AddStr('current', weather_preset);
	k.AddStr('name', weather_preset);
	k.AddBool('game_time', False);
	k.AddBool('removing', False);
	k.AddInt('start', 0, 's32');
	k.AddInt('finish', 1, 's32');
	k.AddInt('source_type', 0, 'u8');
	k.AddHint('modifiers', 'array');
	k.AddSect('modifiers').AddInt('count', 0, 'u32');
	k.AddInt('_total_time', 0, 'u64');
	
	k.AddHint('weather_preset', 'choose');
	k.AddStr('weather_preset', weather_preset);
	k.AddHint('dc_start', 'time');
	k.AddInt('dc_start', l['dc_start'].ParseInt, 'u32');
	k.AddHint('dc_duration', 'time');
	k.AddInt('dc_duration', l['dc_duration'].ParseInt, 'u32');
	k.AddHint('foliage_set', 'choose');
	k.AddStr('foliage_set', l['foliage_set'].AsString);	
	k.AddFloat('foliage_fuzziness', l['foliage_fuzziness'].AsReal);
	
	k.AddHint('raytrace_options', 'flags32');
	if l['raytrace_options'] <> nil then
		k.AddInt('raytrace_options', l['raytrace_options'].ParseInt, 'u32')
	else
		k.AddInt('raytrace_options', 0, 'u32');
	
	
	if l['tablet_presets_vector'] <> nil then
	begin
		k.AddInt('tablet_presets_count', l['tablet_presets_count'].ParseInt, 'u8');
		
		tbl := l['tablet_presets_vector'].AsTable;
		
		k.AddHint('tablet_presets_vector', 'array');
		arr := k.AddSect('tablet_presets_vector');
		arr.AddInt('count', tbl.ItemCount, 'u32');
		
		for I := 0 to tbl.ItemCount-1 do
		begin
			item := tbl[RecStr('rec_', I, 1)].AsTable;
			elem := arr.AddSect(RecStr('rec_', I, 4));
			
			elem.AddVec3('map_positional_min', ParseLuaVec3(item['map_positional_min'].AsTable));
			elem.AddVec3('map_positional_max', ParseLuaVec3(item['map_positional_max'].AsTable));
			elem.AddBool('map_positional_preview_aabb', item['map_positional_preview_aabb'].AsBoolean);
			elem.AddHint('map_menu_name', 'choose');
			elem.AddStr('map_menu_name', item['map_menu_name'].AsString);
			elem.AddInt('map_rotation', item['map_rotation'].ParseInt, 'u8');
			elem.AddStr('subst_tablet_model', item['subst_tablet_model'].AsString);
		end;
	end else
	begin
		k.AddInt('tablet_presets_count', 1, 'u8');
		
		k.AddHint('tablet_presets_vector', 'array');
		arr := k.AddSect('tablet_presets_vector');
		arr.AddInt('count', 1, 'u32');
		
		elem := arr.AddSect('rec_0000');
		
		vec.x := -50; vec.y := 0; vec.z := -50;
		elem.AddVec3('map_positional_min', vec);
		vec.x := 50; vec.y := 25; vec.z := 50;
		elem.AddVec3('map_positional_max', vec);
		elem.AddBool('map_positional_preview_aabb', False);
		elem.AddHint('map_menu_name', 'choose');
		elem.AddStr('map_menu_name', '');
		elem.AddInt('map_rotation', 0, 'u8');
		elem.AddStr('subst_tablet_model', 'dynamic\hud\weapon\map\tablet_n');		
	end;
	
	k.AddStr('next_level', l['next_level'].AsString);
	k.AddStr('back_music', l['back_music'].AsString);
	k.AddInt('migration_rules', l['migration_rules'].ParseInt, 'u16');
end;

function ConvertLevel(const source_dir : String) : TTextKonfig;
var
	levl : TLuaTable;
	k : TTextKonfig;
	I : Longint;
	
	layers, layer : TLuaTable;
	entities : TSection;
	
	layer_name : String;
begin
	k := TTextKonfig.Create;
	
	levl := TLuaTable.Create;
	ParseLuaFile(levl, source_dir + 'level.lua');
	
	// convert startup section
	ConvertStartup(levl['startup'].AsTable, k.root.AddSect('startup'));
	
	// entity version
	k.root.AddSect('entities_params').AddInt('version', 53, 'u16');
	
	// entities itself
	layers := levl['layers'].AsTable;
	
	k.root.AddHint('entities', 'array');
	entities := k.root.AddSect('entities');
	entities.AddInt('count', $FFFFFFFF, 'u32');
	
	for I := 0 to layers.ItemCount - 1 do
	begin
		layer := layers.ItemById[I].AsTable;
		layer_name := layer['name'].AsString;
		
		WriteLn('processing layer ''' + layer_name + '''');
		ConvertLayer(entities, source_dir + 'la_' + layer_name + '.lua');
	end;
	
	TIntegerValue(entities.items[0]).num := entities.ParamCount-1;
	
	//
	AddAchievementData(entities);
	AddExposedBlocks(entities);
	CutEditorScriptBlocks(entities);
	//
	
	levl.Free;
	
	Result := k;
end;

var
	I : Longint;
	source_dir : String;
	output_fn : String;
	output_binary : Boolean;
	
	reslt : TTextKonfig;
begin
	source_dir := '';
	output_fn := '';
	output_binary := False;
	
	I := 1;
	while I <= ParamCount do
	begin
		if ParamStr(I) = '-dir' then
		begin
			if (ParamCount-I) > 0 then
			begin
				source_dir := ParamStr(I+1);
				Inc(I);
			end else
				WriteLn('Missing argument for -dir parameter');	
		end else
		if ParamStr(I) = '-o' then
		begin
			if (ParamCount-I) > 0 then
			begin
				output_fn := ParamStr(I+1);
				Inc(I);
			end else
				WriteLn('Missing argument for -o parameter');	
		end else
		if ParamStr(I) = '-v' then
		begin
			TLuaTable.PrintDataKeysOnAccess := True;
		end else
		if ParamStr(I) = '-text' then
		begin
			output_binary := False;
		end else
		if ParamStr(I) = '-binary' then
		begin
			output_binary := True;
		end else
			WriteLn('unknown option ''' + ParamStr(I) + '''');
		
		Inc(I);
	end;
	
	if (source_dir <> '') and (source_dir[Length(source_dir)] <> '/')  and (source_dir[Length(source_dir)] <> '\') then
		source_dir := source_dir + '\';
		
	if output_fn = '' then
	begin
		if output_binary then
			output_fn := 'level.bin'
		else
			output_fn := 'level.bin.txt';
	end;
	
	reslt := ConvertLevel(source_dir);
	
	if output_binary then
		SaveLevelBin(output_fn, reslt, 36)
	else
		reslt.SaveToFile(output_fn);
		
	reslt.Free;
end.