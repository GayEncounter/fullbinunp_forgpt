uses classes, sysutils, Konfig, uCrc;

var
	sr : TSearchRec;
	T : TTextKonfig;
	list : TStringList;
	entities, E : TSection;
	I : Longint;
	fn : String;
	
	class_crc : String;
	key_crc : String;
	
	b_last_light : Boolean = False;
	
begin
	if ParamCount >= 1 then
		b_last_light := (ParamStr(1) = '-last_light');

	list := TStringList.Create;
	
	if FindFirst('maps\*', faDirectory, sr) = 0 then
	begin
		repeat
			if not ((sr.Name = '.') or (sr.Name = '..')) then
			begin
				T := TTextKonfig.Create;
				T.LoadFromFile('maps\' + sr.Name + '\level.bin.txt');
				
				entities := T.root.GetSect('entities');
				
				for I := 1 to entities.ParamCount-1 do
				begin
					E := entities.GetParam(I) as TSection;
					
					fn := 'static_data\' + LowerCase(E.GetStr('class') + '_' + E.GetStr('static_data_key'));
					
					if b_last_light then
					begin
						class_crc := LowerCase(IntToHex(GetStringCrc(E.GetStr('class')), 8));
						key_crc := LowerCase(IntToHex(GetStringCrc(E.GetStr('static_data_key')), 8));
						fn := '''static_data\' + class_crc + '_' + class_crc + '_' + key_crc + ''', // ' + fn;
					end;
					
					if list.IndexOf(fn) < 0 then
						list.Add(fn);
				end;
				
				T.Free;
			end;	
		until FindNext(sr) <> 0;
		FindClose(sr);
	end;
  
	for I := 0 to list.Count-1 do
		WriteLn(list[I]);
	
	list.Free;
end.