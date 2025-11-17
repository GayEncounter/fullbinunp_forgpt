unit Konfig_script;

interface
uses mujs, Konfig;

procedure Script_Init(J : js_State);
procedure Script_Finish(J : js_State);
procedure Script_Push(J : js_State; S : TSection; js_take_ownership : Boolean);

implementation
uses sysutils;

const
	TAG = 'TSECTION';

procedure ArgsThis(J : js_State; var this : TSection);
begin
	this := TSection(js_touserdata(J, 0, TAG));
	if this = nil then
		js_typeerror(J, 'cannot convert ''this'' to TSection');
end;

procedure ArgsThis2Str(J : js_State; var this : TSection; var s1, s2 : PAnsiChar; def1, def2 : PAnsiChar);
begin
	if js_gettop(J) >= 2 then 
		s1 := js_trystring(J, 1, def1)
	else 
		s1 := def1;

	if js_gettop(J) >= 3 then 
		s2 := js_trystring(J, 2, def2)
	else 
		s2 := def2;

	this := TSection(js_touserdata(J, 0, TAG));
	if this = nil then
		js_typeerror(J, 'cannot convert ''this'' to TSection');
		
	if s1 = nil then
		js_typeerror(J, 'missing argument 1 to call function');
	if s2 = nil then
		js_typeerror(J, 'missing argument 2 to call function');
end;

procedure ArgsThis3Str(J : js_State; var this : TSection; var s1, s2, s3 : PAnsiChar; def1, def2, def3 : PAnsiChar);
begin
	if js_gettop(J) >= 2 then 
		s1 := js_trystring(J, 1, def1)
	else 
		s1 := def1;

	if js_gettop(J) >= 3 then 
		s2 := js_trystring(J, 2, def2)
	else 
		s2 := def2;
		
	if js_gettop(J) >= 4 then 
		s3 := js_trystring(J, 3, def3)
	else 
		s3 := def3;

	this := TSection(js_touserdata(J, 0, TAG));
	if this = nil then
		js_typeerror(J, 'cannot convert ''this'' to TSection');
		
	if s1 = nil then
		js_typeerror(J, 'missing argument 1 to call function');
	if s2 = nil then
		js_typeerror(J, 'missing argument 2 to call function');
	if s3 = nil then
		js_typeerror(J, 'missing argument 3 to call function');
end;

procedure S_Clear(J : js_State); cdecl;
var
	this : TSection;
begin
	ArgsThis(J, this);

	try
		this.Clear;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_AddStr(J : js_State); cdecl;
var
	this : TSection;
	name, value, _type : PAnsiChar;
begin
	ArgsThis3Str(J, this, name, value, _type, nil, nil, 'stringz');

	try
		this.AddStr(name, value, _type);
		js_copy(J, 0); // return this
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_AddBool(J : js_State); cdecl;
var
	this : TSection;
	name, _type : PAnsiChar;
	value : Boolean;
begin
	this := TSection(js_touserdata(J, 0, TAG));
	if this = nil then
		js_typeerror(J, 'cannot convert ''this'' to TSection');
		
	if js_gettop(J) >= 3 then
	begin
		name := js_tostring(J, 1);
		value := js_toboolean(J, 2) <> 0;
		if js_gettop(J) >= 4 then
			_type := js_trystring(J, 3, 'bool')
		else
			_type := 'bool';
	end else
		js_typeerror(J, 'AddInt: invalid call');

	try
		this.AddBool(name, value, _type);
		js_copy(J, 0); // return this
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_AddInt(J : js_State); cdecl;
var
	this : TSection;
	name, _type : PAnsiChar;
	value : Int64;
begin
	this := TSection(js_touserdata(J, 0, TAG));
	if this = nil then
		js_typeerror(J, 'cannot convert ''this'' to TSection');
		
	if js_gettop(J) >= 4 then
	begin
		name := js_tostring(J, 1);
		value := Trunc(js_tonumber(J, 2));
		_type := js_tostring(J, 3);
	end else
		js_typeerror(J, 'AddInt: invalid call');

	try
		this.AddInt(name, value, _type);
		js_copy(J, 0); // return this
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_GetStr(J : js_State); cdecl;
var
	this : TSection;
	name, _type : PAnsiChar;
	v : TStringValue;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'stringz');

	try
		v := this.GetParam(name, _type) as TStringValue;
		
		if v <> nil then
			js_pushstring(J, PAnsiChar(v.str))
		else
			js_pushundefined(J)
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_GetBool(J : js_State); cdecl;
var
	this : TSection;
	name, _type : PAnsiChar;
	v : TBoolValue;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'bool');

	try
		v := this.GetParam(name, _type) as TBoolValue;
		
		if v <> nil then
			js_pushboolean(J, Integer(v.bool))
		else
			js_pushundefined(J)
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_GetInt(J : js_State); cdecl;
var
	this : TSection;
	name, _type : PAnsiChar;
	v : TIntegerValue;
begin
	ArgsThis2Str(J, this, name, _type, nil, nil);

	try
		v := this.GetParam(name, _type) as TIntegerValue;
		
		if v <> nil then
			js_pushnumber(J, v.num)
		else
			js_pushundefined(J)
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure Script_Init(J : js_State);
	procedure DefFunc(f : js_CFunction; name : PAnsiChar; nargs : Longint = 1);
	begin
		js_newcfunction(J, f, name, nargs);
		js_defproperty(J, -2, name, JS_DONTENUM);
	end;
begin
	js_newobject(J);

	DefFunc(S_Clear, 'Clear', 0);
	
	DefFunc(S_AddStr, 'AddStr', 2);
	DefFunc(S_AddBool, 'AddBool', 2);
	DefFunc(S_AddInt, 'AddInt', 3);
	
	DefFunc(S_GetStr, 'GetStr', 1);
	DefFunc(S_GetBool, 'GetBool', 1);
	DefFunc(S_GetInt, 'GetInt', 2);

	js_setregistry(J, 'TSection_prototype');
end;

procedure Script_Finish(J : js_State);
begin
	js_delregistry(J, 'TSection_prototype');
end;

procedure TSection_finalize(J : js_State; p : Pointer); cdecl;
var
	s : TSection;
begin
	s := TSection(p);
	s.Free;
end;

procedure Script_Push(J : js_State; S : TSection; js_take_ownership : Boolean);
var
	fin : js_Finalize;
begin
	if Assigned(S) then
	begin
		if js_take_ownership then
			fin := TSection_finalize
		else
			fin := nil;
			
		js_getregistry(J, 'TSection_prototype');
		js_newuserdata(J, TAG, Pointer(S), fin);
	end else
		js_pushnull(J);
end;

end.