unit framework;

interface
uses Konfig, Variants, mujs, Konfig_reader, LUATable;

type
	TFrameworkBlockDesc = record
		in_names : array of String;
		out_names : array of String;
	end;

type
	TFramework = class
		J : js_State;
		
		constructor Create;
		destructor Destroy; override;
		
		function DecompileKonfig(K : TKonfig; script_name : String; status : PBoolean = nil) : TTextKonfig;
		function DecompileLua(K : TKonfig; script_name : String; status : PBoolean = nil) : TLuaTable;
		function GetBlockDesc(const clsid : String; data : TSection; var desc : TFrameworkBlockDesc) : Boolean;
		function NeedUpdateBlockDesc(const clsid : String; const name,vtype : String; data : TSection) : Boolean;
		
		function ReadEntities(kr : IKonfigReader; version : Word; const script_name : String) : Boolean;
		
		function ExecuteScript(script_name : String) : Boolean;
		procedure DefineGlobal(const name : String; v : Variant);
	end;

implementation
uses uCrc, chunkedFile, sysutils { for FileExists },
	Konfig_script; // class TSection

procedure Script_report(J : js_State; message : PAnsiChar); cdecl;
begin
	WriteLn('[JS] ', message);
end;

procedure Script_crc32(J : js_State); cdecl;
var
	crc : Longint;
	str : String;
begin
	if js_gettop(J) >= 2 then
	begin
		str := js_trystring(J, 1, '');
		crc := GetStringCrc(Str);
		js_pushnumber(J, Longword(crc));
	end else
		js_pushnumber(J, 0);
end;

procedure Script_print(J : js_State); cdecl;
var
	I : Longint;
begin
	for I := 1 to js_gettop(J)-1 do
		Write(js_trystring(J, I, ''));
	WriteLn;
	js_pushundefined(J);
end;

procedure Script_module(J : js_State); cdecl;
var
	filename : String;
	module : PAnsiChar;
	error : Boolean;
	
	r : TMemoryReader;
	source : String;
begin
	module := js_trystring(J, 1, nil);
	if module <> nil then
	begin
		filename := 'js\' + module + '.js';
		if not FileExists(filename) then
			filename := ExtractFilePath(ParamStr(0)) + filename;
		
		r := TMemoryReader.CreateFromFile(filename);
		SetLength(source, r.size);
		r.Read(source[1], r.size);
		r.Free;
		
		source := '((function() {' + source + '}).apply(this));'#10;
		error := False;
		
		if js_ploadstring(J, PAnsiChar(filename), PAnsiChar(source)) = 0 then
		begin
			js_newobject(J); // our module
			
			js_copy(J, -2); // copy function (from js_ploadstring)
			js_copy(J, -2); // copy this (from js_newobject)
			
			if js_pcall(J, 0) = 0 then
				js_pop(J, 1) // remove result
			else
				error := True;
		end else
			error := True;
			
		if error then
		begin
			js_report(J, PAnsiChar('module: error loading ''' + module + ''''));
			js_report(J, js_tostring(J, -1));
			
			js_pushnull(J);
		end 
		else 
			js_copy(J, -1); // copy 'our module'
	end else
	begin
		js_report(J, 'module: invalid call');
		js_pushnull(J);
	end;
end;

constructor TFramework.Create;
var
	_common_js_name : String;
begin
	inherited Create;
	
	J := js_newstate(nil, nil, JS_STRICT);
	js_setreport(J, Script_report);

	js_pushglobal(J);

	js_pushglobal(J);
	js_defproperty(J, -2, '_G', JS_READONLY);

	js_newcfunction(J, Script_crc32, 'crc32', 1);
	js_setproperty(J, -2, 'crc32');

	js_newcfunction(J, Script_print, 'print', 0);
	js_setproperty(J, -2, 'print');
	
	js_newcfunction(J, Script_module, 'module', 0);
	js_setproperty(J, -2, 'module');
	
	js_pop(J, 1);

	if FileExists('js\_common_.js') then
		js_dofile(J, 'js\_common_.js')
	else
	begin
		_common_js_name := ExtractFilePath(ParamStr(0)) + 'js\_common_.js';
		if FileExists(_common_js_name) then
			js_dofile(J, PAnsiChar(_common_js_name));
	end;
	
	Konfig_reader.Script_Init(J);
	Konfig_script.Script_Init(J);
end;

destructor TFramework.Destroy;
begin
	Konfig_reader.Script_Finish(J);
	Konfig_script.Script_Finish(J);
	js_freestate(J);
	
	inherited Destroy;
end;

procedure RunDecompileScript(J : js_State; kr : TKonfigReader; script_name : String; status : PBoolean = nil);
var
	ret : Longint;
begin
	Konfig_reader.Script_Push(J, kr);
	js_setglobal(J, 'reader');

	if not FileExists(script_name) then
		script_name := ExtractFilePath(ParamStr(0)) + script_name;
		
	ret := js_dofile(J, PAnsiChar(script_name));
	if status <> nil then
		status^ := (ret = 0);

	js_pushundefined(J);
	js_setglobal(J, 'reader');

	// kr freed by mujs
end;

function TFramework.DecompileKonfig(K : TKonfig; script_name : String; status : PBoolean = nil) : TTextKonfig;
var
	tk : TTextKonfig;
	R : TKonfigReader;
begin
	tk := TTextKonfig.Create;
	R := TKonfigReader.Create(K, tk.root, nil);
	
	RunDecompileScript(self.J, R, script_name, status);

	// R freed by mujs

	Result := tk;
end;

function TFramework.DecompileLua(K : TKonfig; script_name : String; status : PBoolean = nil) : TLuaTable;
var
	L : TLuaTable;
	R : TKonfigReader;
begin
	L := TLuaTable.Create;
	R := TKonfigReader.Create(K, nil, L);
	
	RunDecompileScript(self.J, R, script_name, status);

	// R freed by mujs

	Result := L;
end;

function TFramework.GetBlockDesc(const clsid : String; data : TSection; var desc : TFrameworkBlockDesc) : Boolean;
var
	ret : Longint;
	I : Longint;
begin
	js_pushglobal(J);
	js_getproperty(J, -1, 'GetBlockDesc');
	
	js_pushnull(J); // this
	js_pushstring(J, PAnsiChar(clsid));
	Konfig_script.Script_Push(J, data, False); // arg 2
	ret := js_pcall(J, 2);
	
	if ret = 0 then
	begin
		if (js_isundefined(J, -1) = 0) and (js_isnull(J, -1) = 0) then
		begin
			// gather input names
			js_getproperty(J, -1, 'in_names');
			if js_isarray(J, -1) <> 0 then
			begin
				SetLength(desc.in_names, js_getlength(J, -1));
				for I := 0 to Length(desc.in_names) - 1 do
				begin
					js_getindex(J, -1, I);
					desc.in_names[I] := js_trystring(J, -1, PAnsiChar('in'+IntToStr(I)));
					js_pop(J, 1);
				end
			end else
				SetLength(desc.in_names, 0);
			js_pop(J, 1);
			
			// gather output names
			js_getproperty(J, -1, 'out_names');
			if js_isarray(J, -1) <> 0 then
			begin
				SetLength(desc.out_names, js_getlength(J, -1));
				for I := 0 to Length(desc.out_names) - 1 do
				begin
					js_getindex(J, -1, I);
					desc.out_names[I] := js_trystring(J, -1, PAnsiChar('out'+IntToStr(I)));
					js_pop(J, 1);
				end
			end else
				SetLength(desc.out_names, 0);
			js_pop(J, 1);
			
			js_pop(J, 1); // pop result
			Result := True;
		end else
		begin
			js_pop(J, 1); // pop result
			Result := False;
		end;
	end else
	begin
		js_pop(J, 1); // pop error
		Result := False;
	end;
	
	js_pop(J, 1); // pop global
end;

function TFramework.NeedUpdateBlockDesc(const clsid : String; const name,vtype : String; data : TSection) : Boolean;
var
	ret : Longint;
begin
	js_pushglobal(J);
	js_getproperty(J, -1, 'NeedUpdateBlockDesc');
	
	js_pushnull(J); // this
	js_pushstring(J, PAnsiChar(clsid));
	js_pushstring(J, PAnsiChar(name));
	js_pushstring(J, PAnsiChar(vtype));
	Konfig_script.Script_Push(J, data, False);
	ret := js_pcall(J, 4);
	
	if ret = 0 then
	begin
		Result := js_tryboolean(J, -1, 0) <> 0;
		js_pop(J, 1); // pop result
	end else
	begin
		Result := False;
		js_pop(J, 1); // pop error
	end;
	
	js_pop(J, 1); // pop global
end;

function TFramework.ReadEntities(kr : IKonfigReader; version : Word; const script_name : String) : Boolean;
begin
	Konfig_reader.Script_Push(J, kr);
	js_setglobal(J, 'reader');

	DefineGlobal('g_entities_version', version);

	Result := ExecuteScript(script_name);

	js_pushundefined(J);
	js_setglobal(J, 'reader');
end;

function TFramework.ExecuteScript(script_name : String) : Boolean;
var
	ret : Longint;
begin
	if not FileExists(script_name) then
		script_name := ExtractFilePath(ParamStr(0)) + script_name;
		
	ret := js_dofile(J, PAnsiChar(script_name));
	Result := (ret = 0);	
end;

procedure TFramework.DefineGlobal(const name : String; v : Variant);
begin
	case varType(v) of
		varEmpty: js_pushundefined(J);
		varNull: js_pushnull(J);
		varSingle: js_pushnumber(J, Single(v));
		varDouble: js_pushnumber(J, Double(v));
		varShortInt: js_pushnumber(J, ShortInt(v));
		varSmallInt: js_pushnumber(J, SmallInt(v));
		varInteger: js_pushnumber(J, Integer(v));
		varInt64: js_pushnumber(J, Int64(v));
		varByte: js_pushnumber(J, Byte(v));
		varWord: js_pushnumber(J, Word(v));
		varLongWord: js_pushnumber(J, LongWord(v));
		varQWord: js_pushnumber(J, QWord(v));
		varBoolean: js_pushboolean(J, Integer(Boolean(v)));
		varString: js_pushstring(J, PAnsiChar(String(v)));
		else 
			WriteLn('framework.DefineGlobal: unsupported variant type');
			Exit;
	end;
	
	js_defglobal(J, PAnsiChar(name), JS_READONLY);
end;

end.