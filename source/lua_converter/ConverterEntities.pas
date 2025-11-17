unit ConverterEntities;

interface
uses Konfig;

procedure ConvertLayer(entities : TSection; const layer_fn : String);

procedure AddAchievementData(entities : TSection);
procedure AddExposedBlocks(entities : TSection);

implementation
uses LUATable, framework, Lua_reader;

procedure ConvertLayer(entities : TSection; const layer_fn : String);
var
	I, J : Longint;
	
	la, sbp, spawns : TLuaTable;
	groups, grp, objects, tool : TLuaTable;
	
	framework : TFramework;
	reader : TLuaReader;
	
	sbp_version : Word;
begin
	la := TLuaTable.Create;
	ParseLuaFile(la, layer_fn);
	
	sbp := la['spawns_base_params'].AsTable;
	sbp_version := sbp['version'].ParseInt;
	if sbp_version <> 53 then
		WriteLn('WARNING spawn version is not 53');
	
	{ spawns inside groups }
	groups := la['groups'].AsTable;
	for I := 0 to groups.ItemCount - 1 do
	begin
		grp := groups.ItemById[I].AsTable;
		
		if grp['objects'] <> nil then
		begin
			objects := grp['objects'].AsTable;
			for J := 0 to objects.ItemCount - 1 do
			begin
				tool := objects.ItemById[J].AsTable;
				if tool['__tool'].AsString = 'spawns' then
				begin
					spawns := tool['spawns'].AsTable;
					reader := TLuaReader.CreateArray(spawns, entities, spawns.ItemCount);
					
					framework := TFramework.Create;
					framework.ReadEntities(reader, sbp_version, 'js\exodus\read_entities.js');
					framework.Free;				
				end;
			end;
		end;
	end;
	
	{ ungrouped }
	spawns := la['spawns'].AsTable;
	reader := TLuaReader.CreateArray(spawns, entities, spawns.ItemCount);
	
	framework := TFramework.Create;
	framework.ReadEntities(reader, sbp_version, 'js\exodus\read_entities.js');
	framework.Free;
	
	la.Free;
end;

procedure AddAchievementData(entities : TSection);
var
	I, index : Longint;
	e, ach : TSection;
	points_data_hint : TSimpleValue;
	ach_hint : TSimpleValue;
	v : TFloatArrayValue;
begin
	for I := 1 to entities.ParamCount - 1 do
	begin
		e := entities.GetParam(I) as TSection;
		if (e.GetStrDef('class', '') = 'PLAYER') and (e.GetSect('achievement_data', False) = nil) then
		begin
			points_data_hint := e.GetParam('points_data', 'array');
			if points_data_hint <> nil then
			begin
				ach_hint := TSimpleValue.Create('achievement_data', 'array');
				ach := TSection.Create('achievement_data');
				ach.AddInt('count', 0, 'u32');
				
				index := e.items.IndexOf(points_data_hint);
				
				e.items.Insert(index, ach);
				e.items.Insert(index, ach_hint);
			end else
				WriteLn('Can''t find points_data : array in PLAYER entity, something messed up');
		end else
		if (e.GetSect('flares_data', False) <> nil) and (e.GetSect('flares_data').GetParam('cmul', 'color, vec4f') = nil) then
		begin
			v := TFloatArrayValue.Create('cmul', 'color, vec4f');
			SetLength(v.data, 4);
			v.data[0] := 1.0;
			v.data[1] := 1.0;
			v.data[2] := 1.0;
			v.data[3] := 1.0;
			e.GetSect('flares_data').items.Add(v);
		end
	end;
end;

procedure AddExposedBlocks(entities : TSection);
var
	I, J : Longint;
	e, cvs, cvss : TSection;
begin
	for I := 1 to entities.ParamCount - 1 do
	begin
		e := entities.GetParam(I) as TSection;
		cvs := e.GetSect('commons_vs', False);
		if cvs <> nil then
		begin
			for J := 1 to cvs.ParamCount - 1 do
			begin
				cvss := cvs.GetParam(J) as TSection;
				if (cvss.GetStr('vs_ref') <> '') and (cvss.GetSect('exposed_blocks', False) = nil) then
				begin
					cvss.AddHint('exposed_blocks', 'array');
					cvss.AddSect('exposed_blocks').AddInt('count', 0, 'u32');
				end;
			end;
		end;
	end;
end;
	
end.