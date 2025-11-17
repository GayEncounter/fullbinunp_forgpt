unit properties;

interface
uses classes, Iup, Konfig;

type
	TPropsBeforeChangeCb = procedure(tree : Ihandle; val : TSimpleValue); cdecl;
	TPropsAfterChangeCb = procedure(tree : Ihandle; val : TSimpleValue); cdecl;
	TPropsEditCb = function(tree : Ihandle; sect : TSection; val : TSimpleValue) : Longint; cdecl; // ret = 0 - cancel, 1 - default editor, 2 - apply

procedure UpdateCaption(tree : Ihandle; nid : Longint; v : TSimpleValue); overload;
procedure UpdateCaption(tree : Ihandle; v : TSimpleValue); overload;

function EditBool8(v : TIntegerValue; const names : String) : Boolean;
function EditEnum(v : TIntegerValue; const names : String; caption : String = '') : Boolean;
function EditChoose(var str : String; allow_none : Boolean; const names : String; caption : String = '') : Boolean; overload;
function EditChoose(var str : String; allow_none : Boolean; names : TStringList; caption : String = '') : Boolean; overload;
function EditChooseArray(var str : String; items : TStringList; caption : String = '') : Boolean;

procedure SetupProperties(tree : Ihandle; data : TSection);

implementation
uses 
	sysutils,
	uScene, uEntity, vmath, uEditorUtils,
	matrix_editor,
	uChoose, uChooseTexture, uChooseColoranim, uChooseSound, uLEOptions;

procedure UpdateCaption(tree : Ihandle; nid : Longint; v : TSimpleValue); overload;
var
	e : TEntity;
	eid : Word;
	
	I : Integer;
	vec : TFloatArrayValue;
		
	title : String;
	
	float_fmt : String = '0.000';
begin	
	if v.vtype = 'entity_link, uobject_link' then
	begin
		eid := (v as TIntegerValue).num;
		if eid <> 65535 then
		begin
			e := Scene.EntityById(eid);
			if e <> nil then
				title := e.Name
			else
				title := '<invalid>';
		end else
			title := '<none>';
	end else
	if v is TStringValue then
		title := (v as TStringValue).str
	else
	if v is TIntegerValue then
		title := IntToStr((v as TIntegerValue).num)
	else
	if v is TSingleValue then
		title := FormatFloat(float_fmt, (v as TSingleValue).num)
	else
	if v is TBoolValue then
	begin
		if (v as TBoolValue).bool then
			title := 'True'
		else
			title := 'False';
	end else
	if v is TFloatArrayValue then
	begin
		if (v.vtype = 'vec2f') or (v.vtype = 'vec3f') or (v.vtype = 'vec4f') or (v.vtype = 'color, vec4f') then
		begin
			vec := v as TFloatArrayValue;
			title := '[';
			for I := 0 to Length(vec.data) - 1 do
			begin
				if I > 0 then title := title + ', ';
				title := title + FormatFloat(float_fmt, vec.data[I])
			end;
			title := title + ']';
		end;
	end;
	
	if IupGetClassType(tree) = 'control' then
	begin
		if (Length(title) > 0) or (v is TStringValue) then
			title := v.name + ' : ' + v.vtype + ' = ' + title
		else
			title := v.name + ' : ' + v.vtype;
			
		iup.SetStrAttribute(tree, 'TITLE'+IntToStr(nid), title);
	end else
	begin
		if (Length(title) > 0) or (v is TStringValue) then
			title := ' = ' + title;

		iup.SetStrAttribute(tree, 'EXTRATEXT'+IntToStr(nid), title);
	end;
end;

procedure UpdateCaption(tree : Ihandle; v : TSimpleValue); overload;
var
	I : Longint;
begin
	for I := 0 to iup.GetInt(tree, 'COUNT') - 1 do
		if iup.GetAttribute(tree, 'USERDATA'+IntToStr(I)) = Pointer(v) then
		begin
			UpdateCaption(tree, I, v);
			Exit;
		end;
end;

function EditBool8(v : TIntegerValue; const names : String) : Boolean;
var
	sl : array of String;
	format : String;
	bools : array[0..7] of Longint;
	args : array[0..7] of PLongint;
	I : Longint;
begin
	sl := SplitString(names);

	format := '';
	for I := 0 to Length(sl) - 1 do
	begin
		format := format + sl[I] + ': %b'#10;
		
		if (v.num and (1 shl I)) <> 0 then
			bools[I] := 1
		else
			bools[I] := 0;
			
		args[I] := @bools[I];
	end;

	if IupGetParamv(PAnsiChar(v.name), nil, nil, PAnsiChar(format),
		 Length(sl), 0, @args[0]) <> 0 then
	begin
		v.num := 0;
		for I := 0 to Length(sl) - 1 do
			v.num := v.num or ((bools[I] and 1) shl I);

		EditBool8 := True;
	end else
		EditBool8 := False;
end;

function EditEnum(v : TIntegerValue; const names : String; caption : String) : Boolean;
var
	sl : array of String;
	ret : Longint;
begin
	sl := SplitString(names);

	if caption = '' then
		caption := v.name;

	ret := iup.ListDialog(caption, sl, v.num+1, 15, 15);
	if ret <> -1 then
	begin
		v.num := ret;
		EditEnum := True;
	end else
		EditEnum := False;
end;

function EditChoose(var str : String; allow_none : Boolean; const names : String; caption : String) : Boolean; overload;
var
	sl : TStringList;
	ret : Longint;
	I : Longint;
	op : Longint;
begin
	sl := TStringList.Create;
	sl.CommaText := names;
	if allow_none then
		sl.Add('<none>');
	
	op := 1;	
	for I := 0 to sl.Count-1 do
	begin
		if sl[I] = str then
			op := I+1;
	end;

	ret := iup.ListDialog(caption, sl, op, 15, 15);
	if ret <> -1 then
	begin
		if allow_none and (ret = sl.Count) then
			str := ''
		else
			str := sl[ret];
			
		EditChoose := True;
	end else
		EditChoose := False;

	sl.Free;
end;

function EditChoose(var str : String; allow_none : Boolean; names : TStringList; caption : String) : Boolean; overload;
var
	sl : TStringList;
	ret : Longint;
	I : Longint;
	op : Longint;
begin
	sl := TStringList.Create;	
	sl.AddStrings(names);
	if allow_none then
		sl.Add('<none>');
		
	op := 1;
	for I := 0 to sl.Count-1 do
	begin
		if sl[I] = str then
			op := I+1;
	end;

	ret := iup.ListDialog(caption, sl, op, 15, 15);
	if ret <> -1 then
	begin
		if allow_none and (ret = sl.Count) then
			str := ''
		else
			str := sl[ret];
			
		EditChoose := True;
	end else
		EditChoose := False;

	sl.Free;
end;

function EditChooseArray(var str : String; items : TStringList; caption : String) : Boolean;
var
	ret : Longint;
	I : Longint;
	marks : String;
begin
	SetLength(marks, items.Count);
	for I := 0 to items.Count-1 do
	begin
		if Pos(items[I], str) > 0 then
			marks[I+1] := '+'
		else
			marks[I+1] := '-';
	end;

	ret := iup.ListDialogMulti(caption, items, 1, 15, 15, marks);
	if ret <> -1 then
	begin
		str := '';
		for I := 0 to items.Count-1 do
			if marks[I+1] = '+' then
			begin
				if Length(str) > 0 then str := str + ',';
				str := str + items[I];
			end;
			
		EditChooseArray := True;
	end else
		EditChooseArray := False;
end;

procedure default_before_change_cb(tree : Ihandle; val : TSimpleValue); cdecl;
begin
end;

procedure default_after_change_cb(tree : Ihandle; val : TSimpleValue); cdecl;
begin
end;

function EditStrArray(sect : TSection; hint : TSimpleValue; size_type : String; tree : Ihandle) : Boolean;
var
	I, index : Longint;
	src : String;
	maxlen : Longint;
	buffer : array of AnsiChar;
	sl : TStringList;
	
	before_change_cb : TPropsBeforeChangeCb; 
	after_change_cb : TPropsAfterChangeCb;
begin
	before_change_cb := TPropsBeforeChangeCb(IupGetAttribute(tree, 'PROPS_BEFORE_CHANGE_CB'));
	after_change_cb := TPropsAfterChangeCb(IupGetAttribute(tree, 'PROPS_AFTER_CHANGE_CB'));

	if not Assigned(before_change_cb) then
		before_change_cb := default_before_change_cb;
	if not Assigned(after_change_cb) then
		after_change_cb := default_after_change_cb;

	Result := False;

	index := sect.items.IndexOf(hint);
	if index < 0 then
		Exit;
	
	I := index+1;
	src := '';
	while (I < sect.items.Count) and (TSimpleValue(sect.items[I]).name = hint.name) do
	begin
		if TSimpleValue(sect.items[I]) is TStringValue then
		begin
			if src <> '' then
				src := src + #10;
				
			src := src + TStringValue(sect.items[I]).str;
		end;
		
		Inc(I);
	end;
	
	maxlen := Length(src)+4096;
	SetLength(buffer, maxlen);
	
	if Length(src) > 0 then
		Move(src[1], buffer[0], Length(src)+1)
	else
		buffer[0] := #0;
	
	if IupGetText(PAnsiChar(hint.name), PAnsiChar(buffer), maxlen) <> 0 then
	begin
		before_change_cb(tree, hint);
		
		I := index+1;
		while (I < sect.items.Count) and (TSimpleValue(sect.items[I]).name = hint.name) do
		begin
			TSimpleValue(sect.items[I]).Free;
			sect.items.Delete(I);
		end;
		
		sl := TStringList.Create;
		sl.Delimiter := #10;
		sl.StrictDelimiter := True;
		
		sl.DelimitedText := PAnsiChar(buffer);
		
		sect.items.Insert(index+1, TIntegerValue.Create(hint.name, size_type, sl.Count));
		for I := 0 to sl.Count-1 do
			sect.items.Insert(index+2+I, TStringValue.Create(hint.name, sl[I]));
			
		sl.Free;
		
		after_change_cb(tree, hint);
		
		Result := True;
	end;
end;

function tree_props_button_cb(tree : Ihandle; button, pressed, x, y : Longint; status : PAnsiChar) : Longint; cdecl;
var
	id : Longint;
	format : String;

	ivalue : Longint;
	fvalue : Single;
	svalue : array[0..4095] of Char;

	v : TSimpleValue;

	entity : TEntity;

	i : TIntegerValue;
	f : TSingleValue;
	s : TStringValue;
	b : TBoolValue;

	str : String;

	c : TFloatArrayValue; // color
	cvalue : TVec4;

	vec2 : TVec2;
	vec3 : TVec3;
	vec4 : TVec4;
	matrix : TMatrix;

	ret : Longint;
	
	parent : Longint;
	sect : TSection;
	
	before_change_cb : TPropsBeforeChangeCb;
	after_change_cb  : TPropsAfterChangeCb;
	edit_cb          : TPropsEditCb;
begin
	id := IupConvertXYToPos(tree, x, y);
	before_change_cb := TPropsBeforeChangeCb(IupGetAttribute(tree, 'PROPS_BEFORE_CHANGE_CB'));
	after_change_cb := TPropsAfterChangeCb(IupGetAttribute(tree, 'PROPS_AFTER_CHANGE_CB'));
	edit_cb := TPropsEditCb(IupGetAttribute(tree, 'PROPS_EDIT_CB'));
	
	if not Assigned(before_change_cb) then
		before_change_cb := default_before_change_cb;
	if not Assigned(after_change_cb) then
		after_change_cb := default_after_change_cb;

	if (id <> -1) and iup_isdouble(status) then
	begin
		parent := iup.GetInt(tree, 'PARENT'+IntToStr(id));
		sect := TSection(iup.GetAttribute(tree, 'USERDATA'+IntToStr(parent)));
				
		v := TSimpleValue(iup.GetAttribute(tree, 'USERDATA'+IntToStr(id)));
		if v <> nil then
		begin
			if Assigned(edit_cb) then
				ret := edit_cb(tree, sect, v)
			else
				ret := 1;
						
			if ret = 1 then // default editor
			begin
				if (v.vtype = 'ref_model') or ((v.vtype = 'choose') and (v.name = 'visual')) then
				begin		
					s := sect.GetParam(v.name, 'stringz') as TStringValue;
					str := s.str;
					
					if ChooseModel(str) then
					begin
						before_change_cb(tree, s);
						
						s.str := str;
						UpdateCaption(tree, s);
						
						after_change_cb(tree, s);
					end;					
				end else
				if v.vtype = 'texture, str_shared' then
				begin
					s := sect.GetParam(v.name, 'stringz') as TStringValue;
					str := s.str;
					
					if ChooseTexture(str) then
					begin
						before_change_cb(tree, s);
						
						s.str := str;
						UpdateCaption(tree, s);
						
						after_change_cb(tree, s);
					end;
				end;
				if v.vtype = 'particles, str_shared' then
				begin
					s := sect.GetParam(v.name, 'stringz') as TStringValue;
					str := s.str;
					
					if ChooseParticles(str) then
					begin
						before_change_cb(tree, s);
						
						s.str := str;
						UpdateCaption(tree, s);
						
						after_change_cb(tree, s);
					end;
				end;
				if v.vtype = 'camera_track, str_shared' then
				begin
					s := sect.GetParam(v.name, 'stringz') as TStringValue;
					str := s.str;
					
					if ChooseCameraTrack(str) then
					begin
						before_change_cb(tree, s);
						
						s.str := str;
						UpdateCaption(tree, s);
						
						after_change_cb(tree, s);
					end;
				end;
				if v.vtype = 'sound' then
				begin
					s := sect.GetParam(v.name, 'stringz') as TStringValue;
					str := s.str;
					
					if ChooseSound(str) then
					begin
						before_change_cb(tree, s);
						
						s.str := str;
						UpdateCaption(tree, s);
						
						after_change_cb(tree, s);
					end;
				end;
				if v.vtype = 'ref_coloranim' then
				begin
					s := sect.GetParam(v.name, 'stringz') as TStringValue;
					str := s.str;
					
					if ChooseColoranim(str) then
					begin
						before_change_cb(tree, s);
						
						s.str := str;
						UpdateCaption(tree, s);
						
						after_change_cb(tree, s);
					end;
				end;
				if v.vtype = 'entity_link, uobject_link' then
				begin
					i := v as TIntegerValue;
	
					if ChooseEntity(entity) then
					begin
						before_change_cb(tree, i);
						
						if entity <> nil then
							i.num := entity.ID
						else
							i.num := 65535;
						UpdateCaption(tree, id, v);
							
						after_change_cb(tree, i);
					end;
				end else
				if v.vtype = 'str_array16' then
				begin
					if EditStrArray(sect, v, 'u16', tree) then
						SetupProperties(tree, TSection(IupGetAttribute(tree, 'USERDATA0')));
				end else
				if (v.vtype = 'str_array32') or (v.vtype = 'str_array') then
				begin
					if EditStrArray(sect, v, 'u32', tree) then
						SetupProperties(tree, TSection(IupGetAttribute(tree, 'USERDATA0')));
				end else
				if v is TIntegerValue then
				begin
					i := v as TIntegerValue;
					ivalue := i.num;
	
					format := '%i';
					if i.vtype = 'u8' then
						format := '%i[0,255]';
					if i.vtype = 's8' then
						format := '%i[-128,127]';
					if i.vtype = 'u16' then
						format := '%i[0,65535]';
					if i.vtype = 's16' then
						format := '%i[-32768,32767]';
	
					format := 'Value: ' + format + #10;
					if IupGetParam(PAnsiChar(v.name), nil, nil, PAnsiChar(format), @ivalue) = 1 then
					begin
						before_change_cb(tree, i);
						
						i.num := ivalue;
						UpdateCaption(tree, id, v);
						
						after_change_cb(tree, i);
					end;
				end else
				if v is TSingleValue then
				begin
					f := v as TSingleValue;
					fvalue := f.num;
					if IupGetParam(PAnsiChar(v.name), nil, nil, 'Value: %r'#10, @fvalue) = 1 then
					begin
						before_change_cb(tree, f);
						
						f.num := fvalue;
						UpdateCaption(tree, id, v);
						
						after_change_cb(tree, f);
					end;
				end else
				if v is TStringValue then
				begin
					s := v as TStringValue;
					Move(s.str[1], svalue[0], Length(s.str));
					svalue[Length(s.str)] := #0;
					if IupGetParam(PAnsiChar(v.name), nil, nil, 'Value: %s'#10, @svalue) = 1 then
					begin
						before_change_cb(tree, s);
						
						s.str := PAnsiChar(@svalue);
						UpdateCaption(tree, id, v);
						
						after_change_cb(tree, s);
					end;
				end else
				if v is TBoolValue then
				begin
					b := v as TBoolValue;
					
					before_change_cb(tree, b);
					
					b.bool := not b.bool;
					UpdateCaption(tree, id, v);
					
					after_change_cb(tree, b);
				end else
				if v is TFloatArrayValue then
				begin
					c := v as TFloatArrayValue;
	
					ret := 0;
	
					if c.vtype = 'pose, matrix' then
					begin
						if Length(c.data) = 16 then	c.GetMatrix44(matrix)
						else c.GetMatrix43(matrix);
						
						if EditMatrix(matrix, True) then
						begin
							before_change_cb(tree, c);
							
							if Length(c.data) = 16 then	c.SetMatrix44(matrix)
							else c.SetMatrix43(matrix);
							ret := 1;
						end;
					end;
					if c.vtype = 'pose, matrix_43T' then
					begin
						c.GetMatrix43T(matrix);
						
						if EditMatrix(matrix, True) then
						begin
							before_change_cb(tree, c);
							
							c.SetMatrix43T(matrix);
							ret := 1;
						end;
					end;
					if c.vtype = 'color, vec4f' then
					begin
						Move(c.data[0], cvalue, Sizeof(cvalue));
						if SelectColor(cvalue, True) then
						begin
							before_change_cb(tree, c);
							
							Move(cvalue, c.data[0], Sizeof(cvalue));
							ret := 1;
						end;
					end;
					if c.vtype = 'vec4f' then
					begin
						Move(c.data[0], vec4, Sizeof(vec4));
						ret := IupGetParam(PAnsiChar(v.name), nil, nil, 'X: %r'#10'Y: %r'#10'Z: %r'#10'W: %r'#10,
							@vec4.x, @vec4.y, @vec4.z, @vec4.w);
							
						if ret = 1 then
						begin
							before_change_cb(tree, c);
							Move(vec4, c.data[0], Sizeof(vec4));
						end;
					end;
					if c.vtype = 'vec3f' then
					begin
						Move(c.data[0], vec3, Sizeof(vec3));
						ret := IupGetParam(PAnsiChar(v.name), nil, nil, 'X: %r'#10'Y: %r'#10'Z: %r'#10,
							@vec3.x, @vec3.y, @vec3.z);
							
						if ret = 1 then
						begin
							before_change_cb(tree, c);
							Move(vec3, c.data[0], Sizeof(vec3));
						end;
					end;
					if c.vtype = 'vec2f' then
					begin
						Move(c.data[0], vec2, Sizeof(vec2));
						ret := IupGetParam(PAnsiChar(v.name), nil, nil, 'X: %r'#10'Y: %r'#10,
							@vec2.x, @vec2.y);
							
						if ret = 1 then
						begin
							before_change_cb(tree, c);
							Move(vec2, c.data[0], Sizeof(vec2));
						end;
					end;
					
					if ret = 1 then
					begin
						UpdateCaption(tree, id, v);
						after_change_cb(tree, c);
					end;
				end;
			end else
			if ret = 2 then // user editor
			begin
				UpdateCaption(tree, id, v);
			end;
			
		end;
	end;

	Result := IUP_DEFAULT;
end;

procedure AddSection(tree : Ihandle; ref : Longint; sect : TSection);
var
	I : Integer;
	v : TSimpleValue;

	nid : String;
begin
	iup.SetStrAttribute(tree, 'ADDBRANCH' + IntToStr(ref), sect.name);
	
	if uLEOptions.props_exclude_vss_ver_6 then
		if (sect.name = 'vss_ver_6') or (sect.name = 'vss_ver_7') then
			Exit;
	
	nid := IupGetAttribute(tree, 'LASTADDNODE');
	iup.SetAttribute(tree, 'USERDATA' + nid, Pointer(sect));
	
	ref := StrToInt(nid);
	
	for I := sect.items.Count - 1 downto 0 do
	begin
		v := TSimpleValue(sect.items[I]);

		if v is TSection then
		begin
			AddSection(tree, ref, v as TSection);
		end else
		begin
			iup.SetStrAttribute(tree, 'ADDLEAF'+IntToStr(ref), v.name + ' : ' + v.vtype);
			nid := IupGetAttribute(tree, 'LASTADDNODE');
			iup.SetAttribute(tree, 'USERDATA' + nid, Pointer(v));
			
			UpdateCaption(tree, StrToInt(nid), v);
		end;
	end;
end;

procedure SetupProperties(tree : Ihandle; data : TSection);
begin
	// if tree is native control callback name is BUTTON_CB, else FLAT_BUTTON_CB
	if IupGetClassType(tree) = 'control' then
		IupSetCallback(tree, 'BUTTON_CB', @tree_props_button_cb)
	else
		IupSetCallback(tree, 'FLAT_BUTTON_CB', @tree_props_button_cb);
	
	IupSetAttribute(tree, 'DELNODE', 'ALL');
	
	IupSetAttribute(tree, 'AUTOREDRAW', 'NO');
	AddSection(tree, -1, data);	
	IupSetAttribute(tree, 'AUTOREDRAW', 'YES');
	
	IupRedraw(tree, 1);
end;

end.
