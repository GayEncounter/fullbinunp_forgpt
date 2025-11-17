program binunp;
uses classes, sysutils, Konfig, texturePrefs, levelbin,
     framework, Konfig_reader, Timer, StrUtils, LUATable;

procedure CompileLevel(infn, outfn : String; kind : Integer; json_mode : Boolean);
var
	tk : TTextKonfig;
	tmr : TTimer;
begin
	tk := TTextKonfig.Create;
	
	tmr.Start;
	
	if json_mode then
		tk.LoadJSONFromFile(infn)
	else
		tk.LoadFromFile(infn);
	
	WriteLn('Parsed ''', infn, ''' in ', tmr.CurrentTime:10:10, 's');

	if kind = 0 then
		kind := GetDefaultKindForLevelBin(tk);

	tmr.Start;
	SaveLevelBin(outfn, tk, kind);
	WriteLn('Saving time: ', tmr.CurrentTime:10:10, 's');

	tk.Free;
end;

procedure DecompileLevel(inf, outf : String; build_15_10_2012 : Boolean; json_mode : Boolean);
var
	TK : TTextKonfig;
	tmr : TTimer;
begin
	tmr.Start;
	TK := LoadLevelBin(inf, build_15_10_2012);
	WriteLn('Loaded ''', inf, ''' in ', tmr.CurrentTime:10:10, 's');
	
	if TK <> nil then
	begin
		Writeln('Saving to ', outf);
	
		tmr.Start;		
		if json_mode then
			TK.SaveJSONToFile(outf)
		else
			TK.SaveToFile(outf);
		WriteLn('Saving time: ', tmr.CurrentTime:10:10, 's');
		
		TK.Free;
	end else
		Writeln('This is not level.bin file!');
end;

procedure CompileConfig(inf, outf : String; kind : Integer; last_light : Boolean; json_mode : Boolean);
var
	K : TKonfig;
	T : TTextKonfig;
begin
	T := TTextKonfig.Create;
	
	if json_mode then
		T.LoadJSONFromFile(inf)
	else
		T.LoadFromFile(inf);

	K := TKonfig.Create;
	K.kind := kind;
	K.Compile(T, last_light);

	K.Save(outf);

	K.Free;
	T.Free;
end;

procedure DecompileConfig(inf, outf : String; last_light : Boolean; json_mode : Boolean);
var
	K : TKonfig;
	T : TTextKonfig;
begin
	K := TKonfig.Create;
	if K.Load(inf) then
	begin
		WriteLn('kind = ', K.kind);
		T := TTextKonfig.Create;
		K.Decompile(T, last_light);
		if json_mode then
			T.SaveJSONToFile(outf)
		else
			T.SaveToFile(outf);
		T.Free;
	end;
	K.Free;
end;

procedure DecompileSpecial(script, inf, outf : String; lua_mode : Boolean; json_mode : Boolean);
var
	k : TKonfig;
	tk : TTextKonfig;
	L : TLuaTable;
	js : TFramework;
	t : TTexturesBin;
	status : Boolean;
begin
	k := TKonfig.Create;
	if k.Load(inf) then
	begin
		WriteLn('kind = ', K.kind);


		if AnsiEndsStr(inf, 'textures.bin') then // must die
		begin
			tk := TTextKonfig.Create;
				
			t := TTexturesBin.Create;
			t.Load(k);
			WriteLn('loading ok');
			t.Save(tk);
			t.Free;
			
			tk.SaveToFile(outf);
			tk.Free;
		end else
		begin
			js := TFramework.Create;
			js.DefineGlobal('g_bin_file_name', ExtractFileName(inf));
			
			if lua_mode then
			begin
				L := js.DecompileLua(k, script, @status);
				PrintLuaTableToFile(L, outf);
				L.Free;
			end else
			begin
				tk := js.DecompileKonfig(k, script, @status);
				if json_mode then
					tk.SaveJSONToFile(outf)
				else
					tk.SaveToFile(outf);
				tk.Free;
			end;
			
			if status then
				WriteLn('loading ok')
			else
				WriteLn('loading failed');
			js.Free
		end;


	end;
	k.Free;
end;

procedure Usage;
begin
	Writeln('Usage: ');
	Writeln(#9'binunp [-json_out] [-l] [-ll] [-build_15_10_2012] -d infile [outfile]');
	Writeln(#9'binunp [-json_in] [-l] [-ll] [-k n] -c infile [outfile]');
	Writeln(#9'binunp [-json_out] [-lua_out] -s script infile [outfile]');
	Writeln(#9'binunp -?');
	Writeln(#9'binunp -help');
	Writeln;
	Writeln('-l option = compile/decompile level.bin');
	Writeln('-ll option = compile/decompile bin from Last Light or later versions');
	Writeln('-k option = config type (for compilation), may be 3, 4, 5, 16 or 36, by default 5');
	Writeln('-build_15_10_2012 = decompile level.bin from Metro Last Light Build 2662 (Oct 15, 2012)');
	WriteLn('-lua_out = output .lua file for official Exodus SDK');
	WriteLn('-json_out = output JSON file');
	WriteLn('-json_in = read JSON file instead of custom Pascal-like syntax');
	WriteLn('-? or -help = this help');
end;

var
	I : Integer;
	kind : Byte;
	level : Boolean;
	last_light : Boolean;
	out_file : String;
	build_15_10_2012 : Boolean;
	
	lua_mode : Boolean;
	
	output_json : Boolean;
	input_json : Boolean;
begin
	kind := 0;
	level := False;
	last_light := False;
	build_15_10_2012 := False;
	
	lua_mode := False;
	
	input_json := False;
	output_json := False;

	if (ParamCount = 0) or ((ParamCount = 1) and ((ParamStr(1) = '-?') or (LowerCase(ParamStr(1)) = '-help'))) then
	begin
		Usage;
		Exit;
	end;

	I := 1;
	while I <= ParamCount do
	begin
		if ParamStr(I) = '-v' then
			TKonfigReader.WarnIfDataLeft := True
		else if ParamStr(I) = '-vv' then
			TKonfigReader.PrintDebugInfo := True
		else if ParamStr(I) = '-l' then
			level := True
		else if ParamStr(I) = '-ll' then	
			last_light := True
		else if ParamStr(I) = '-build_15_10_2012' then
			build_15_10_2012 := True
		else if ParamStr(I) = '-lua_out' then
			lua_mode := True
		else if ParamStr(I) = '-json_in' then
			input_json := True
		else if ParamStr(I) = '-json_out' then
			output_json := True
		else if ParamStr(I) = '-k' then
		begin
			kind := StrToInt(ParamStr(I+1));
			Inc(I, 1);
			
			if not (kind in [3, 4, 5, 16, 20, 36]) then
			begin
				Writeln('Unsupported config type ', kind, ', reset to 5');
				kind := 5;
			end;
		end else
		if ParamStr(I) = '-d' then
		begin
			if (ParamCount - I) >= 1 then
			begin
				if (ParamCount - I) >= 2 then
				begin
					out_file := ParamStr(I+2)
				end else
				begin
					if output_json then
						out_file := ParamStr(I+1) + '.json'
					else
						out_file := ParamStr(I+1) + '.txt';
				end;
								
				if level then
					DecompileLevel(ParamStr(I+1), out_file, build_15_10_2012, output_json)
				else
					DecompileConfig(ParamStr(I+1), out_file, last_light, output_json);
					
				Inc(I, 2);
			end else
				WriteLn('Not enough parameters for -d');
		end else
		if ParamStr(I) = '-c' then
		begin
			if (ParamCount - I) >= 1 then
			begin
				if (ParamCount - I) >= 2 then
					out_file := ParamStr(I+2)
				else
					out_file := ChangeFileExt(ParamStr(I+1), '');
					
				if level then
				begin
					CompileLevel(ParamStr(I+1), out_file, kind, input_json)
				end else
				begin
					if kind = 0 then
						kind := 5; // default
						
					CompileConfig(ParamStr(I+1), out_file, kind, last_light, input_json);
				end;
				
				Inc(I, 2);
			end else
				WriteLn('Not enough parameters for -c');
		end else
		if ParamStr(I) = '-s' then
		begin
			if (ParamCount - I) >= 2 then
			begin
				if (ParamCount - I) >= 3 then
				begin
					out_file := ParamStr(I+3)
				end else 
				begin
					if lua_mode then
					begin
						out_file := ParamStr(I+2) + '.lua'
					end else
					begin
						if output_json then
							out_file := ParamStr(I+2) + '.json'
						else
							out_file := ParamStr(I+2) + '.txt';
					end;

				end;
				
				DecompileSpecial(ParamStr(I+1), ParamStr(I+2), out_file, lua_mode, output_json);
				Inc(I, 3);
			end else
				WriteLn('Not enough parameters for -s');
		end else		
			Writeln('Unknown parameter ', ParamStr(I));
		
		Inc(I);
	end;
end.