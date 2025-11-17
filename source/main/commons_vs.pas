uses classes, sysutils, Konfig;

var
	sr : TSearchRec;
	T : TTextKonfig;
	list : TStringList;
	entities, E : TSection;
	I,J : Longint;

	vs : String;
	rec : TSection;
	
	commons_vs : TSection;
	vss_ver_6 : TSection;
	vss, blocks, block : TSection;
	attacks : TSection;
	
begin
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
					
					commons_vs := E.GetSect('commons_vs', False);
					if commons_vs <> nil then
					begin
						for J := 1 to commons_vs.ParamCount - 1 do
						begin
							rec := commons_vs.GetParam(J) as TSection;
							vs := rec.GetStr('vs_ref');		
							if (vs <> '') and (list.IndexOf(vs) < 0) then
								list.Add(vs);
						end;
					end;
					
					commons_vs := E.GetSect('removed_vs', False);
					if commons_vs <> nil then
					begin
						for J := 1 to commons_vs.ParamCount - 1 do
						begin
							rec := commons_vs.GetParam(J) as TSection;
							vs := rec.GetStr('vs_ref');		
							if (vs <> '') and (list.IndexOf(vs) < 0) then
								list.Add(vs);
						end;
					end;
					
					vs := E.GetStrDef('movement_hit_vs_ref', '');
					if (vs <> '') and (list.IndexOf(vs) < 0) then
						list.Add(vs);

					attacks := E.GetSect('attacks', False);
					if attacks <> nil then
					begin
						for J := 0 to attacks.ParamCount - 1 do
						begin
							rec := attacks.GetParam(J) as TSection;
							vs := rec.GetStr('vs_reference');		
							if (vs <> '') and (list.IndexOf(vs) < 0) then
								list.Add(vs);
						end;
					end;
					
					vss_ver_6 := E.GetSect('vss_ver_7', False);
					if vss_ver_6 = nil then
						vss_ver_6 := E.GetSect('vss_ver_6', False);
						
					if (vss_ver_6 <> nil) and (vss_ver_6.ParamCount > 1) then
					begin
						vss := vss_ver_6.GetParam(1) as TSection;
						blocks := vss.GetSect('blocks');
						if blocks <> nil then
						begin
							for J := 0 to blocks.ParamCount - 1 do
							begin
								if blocks.GetParam(J) is TSection then
								begin
									block := blocks.GetParam(J) as TSection;
									if block.GetStrDef('clsid', '') = 'actions/attach vs' then
									begin
										vs := block.GetStr('vs');		
										if (vs <> '') and (list.IndexOf(vs) < 0) then
											list.Add(vs);
									end;
								end;
							end
						end;
					end;
					
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