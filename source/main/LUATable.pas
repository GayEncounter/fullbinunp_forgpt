unit LUATable;

interface
uses classes, sysutils, fgl;

type
	ELuaInvalidCast = class(Exception);

type
	TLuaType = (ltNil, ltBoolean, ltString, ltNumber, ltReal, ltTable);

type
	TLuaObject  = class;
	TLuaTable   = class;
	TLuaReal    = class;
	TLuaNumber  = class;
	TLuaBoolean = class;
	TLuaString  = class;
	
	TLuaObjectMap = TFPGMap<String,TLUAObject>;
	
	TLuaObject = class
		name : String;
		lua_type : TLuaType;
		
		constructor Create;
		function DataType : TLuaType;
		
		function AsBoolean : Boolean;
		function AsString : String;
		function AsNumber : Int64;
		function AsReal : Double;
		function AsTable : TLuaTable;
		
		function ParseInt : Int64;
		function ParseFloat : Double;
	end;
	
	TLuaTable = class(TLuaObject)
		PrintDataKeysOnAccess : Boolean; static;
		
		items : TList;
		items_map : TLuaObjectMap;
		
		constructor Create;
		destructor Destroy; override;
		function GetItemByKey(const key : String) : TLuaObject;
		function GetItemById(id : Longint) : TLuaObject;
		function GetItemCount : Longint;
		
		function NewTable(const nm : String) : TLuaTable;
		function NewReal(const nm : String; val : Double) : TLuaReal;
		function NewNumber(const nm : String; val : Int64) : TLuaNumber;
		function NewBoolean(const nm : String; val : Boolean) : TLuaBoolean;
		function NewString(const nm, val : String) : TLuaString;
		
		property ItemByKey[key : String] : TLuaObject read GetItemByKey; default;
		property ItemById[id : Longint] : TLuaObject read GetItemById; // ideally should be default also, but FreePascal doesn't support default property overloading
		property ItemCount : Longint read GetItemCount;
	end;
	
	TLuaReal = class(TLuaObject)
		value : Double;
		constructor Create(val : Double);
	end;
	
	TLuaNumber = class(TLuaObject)
		value : Int64;
		constructor Create(val : Int64);
	end;
	
	TLuaBoolean = class(TLuaObject)
		value : Boolean;
		constructor Create(val : Boolean);
	end;
	
	TLuaString = class(TLuaObject)
		value : String;
		constructor Create(val : String);
	end;
	
procedure ParseLuaFile(obj : TLuaTable; const filename : String);
procedure ParseLuaString(obj : TLuaTable; const source : String);

function PrintLuaTable(obj : TLuaTable) : String;
procedure PrintLuaTableToFile(obj : TLuaTable; const fn : String);

implementation
uses chunkedFile, Parser, EscapedStrings;

function TypeStr(t : TLuaType) : String;
begin
	case t of
		ltNil:     Result := 'nil';
		ltBoolean: Result := 'boolean';
		ltString:  Result := 'string';
		ltNumber:  Result := 'number';
		ltReal:    Result := 'real';
		ltTable:   Result := 'table';
		else       Result := '?';
	end;
end;

function IsIntegerStr(const str : String) : Boolean;
var
	I : Longint;
begin
	if Length(str) > 0 then
	begin
		Result := True;
		I := 1;
			
		if (str[1] = '-') then
			Inc(I);
		
		if (Length(str) > 2) and (str[I] = '0') and ((str[I+1] = 'x') or (str[I+1] = 'X')) then
		begin
			while I <= Length(str) do
			begin
				if not (str[I] in ['0'..'9', 'a'..'f', 'A'..'F']) then
				begin
					Result := False;
					Break;
				end;
				
				Inc(I);
			end;
		end else
		begin
			while I <= Length(str) do
			begin
				if not (str[I] in ['0'..'9']) then
				begin
					Result := False;
					Break;
				end;
				
				Inc(I);
			end;
		end;
	end else
		Result := False;
end;

function IsRealStr(const str : String) : Boolean;
var
	I : Longint;
	was_dot : Integer;
	was_exponent : Integer;
begin
	if Length(str) > 0 then
	begin
		Result := True;
		I := 1;
		was_dot := -1;
		was_exponent := -1;
		
		if (str[1] = '-') then
			Inc(I);
			
		while I <= Length(str) do
		begin
			if (str[I] = 'e') or (str[I] = 'E') then
			begin
				if was_exponent > 0 then
				begin
					Result := False;
					Break;
				end;
				was_exponent := I;
			end else 
			if str[I] = '.' then
			begin
				if was_exponent > 0 then
				begin
					Result := False;
					Break;
				end;
				
				if was_dot > 0 then
				begin
					Result := False;
					Break;
				end;
				
				was_dot := I;
			end else
			if not (str[I] in ['0'..'9']) then
			begin
				Result := False;
				Break;
			end;
			
			Inc(I);
		end;
	end else
		Result := False;
end;

function IsLuaIdentifier(const str : String) : Boolean;
var
	I : Longint;
begin
	if Length(str) > 0 then
	begin
		if (str <> 'nil') and (str <> 'true') and (str <> 'false') and (str[1] in ['a'..'z', 'A'..'Z', '_']) then
		begin
			Result := True;
			
			for I := 2 to Length(str) do
			begin
				if not (str[I] in ['0'..'9', 'a'..'z', 'A'..'Z', '_']) then
				begin
					Result := False;
					Break;
				end;
			end;
		end else
			Result := False;
	end else
		Result := False;
end;

constructor TLuaObject.Create;
begin
	inherited;
	lua_type := ltNil;
end;

function TLuaObject.DataType : TLuaType;
begin
	if self = nil then
		DataType := ltNil
	else
		DataType := lua_type;
end;

function TLuaObject.AsBoolean : Boolean;
begin
	if self.DataType = ltBoolean then
		Result := TLuaBoolean(self).value
	else
		raise ELuaInvalidCast.Create('can''t convert ''' + TypeStr(self.DataType) + ''' to boolean');
end;

function TLuaObject.AsString : String;
begin
	if self.DataType = ltString then
		Result := TLuaString(self).value
	else
		raise ELuaInvalidCast.Create('can''t convert ''' + TypeStr(self.DataType) + ''' to string');
end;

function TLuaObject.AsNumber : Int64;
begin
	if self.DataType = ltNumber then
		Result := TLuaNumber(self).value
	else
		raise ELuaInvalidCast.Create('can''t convert ''' + TypeStr(self.DataType) + ''' to number');
end;

function TLuaObject.AsReal : Double;
begin
	if self.DataType = ltReal then
		Result := TLuaReal(self).value
	else
		raise ELuaInvalidCast.Create('can''t convert ''' + TypeStr(self.DataType) + ''' to real');
end;

function TLuaObject.AsTable : TLuaTable;
begin
	if self.DataType = ltTable then
		Result := TLuaTable(self)
	else
		raise ELuaInvalidCast.Create('can''t convert ''' + TypeStr(self.DataType) + ''' to table');
end;

function TLuaObject.ParseInt : Int64;
begin
	if self.DataType = ltNumber then
		Result := TLuaNumber(self).value
	else if (self.DataType = ltString) and IsIntegerStr(TLuaString(self).value) then
		Result := StrToInt(TLuaString(self).value)
	else
		raise ELuaInvalidCast.Create('can''t convert ''' + TypeStr(self.DataType) + ''' to number');
end;

function TLuaObject.ParseFloat : Double;
begin
	if self.DataType = ltReal then
		Result := TLuaReal(self).value
	else if self.DataType = ltNumber then
		Result := TLuaNumber(self).value
	else if (self.DataType = ltString) and IsIntegerStr(TLuaString(self).value) then
		Result := Single(Longint(StrToInt(TLuaString(self).value)))
	else
		raise ELuaInvalidCast.Create('can''t convert ''' + TypeStr(self.DataType) + ''' to real');
end;

constructor TLuaTable.Create;
begin
	inherited;
	lua_type := ltTable;
	items := TList.Create;
	items_map := TLuaObjectMap.Create;
	items_map.Sorted := True;
end;
		
destructor TLuaTable.Destroy;
var
	I : Longint;
begin
	for I := 0 to items.Count - 1 do
		TLuaObject(items[I]).Free;
	items.Free;
	items_map.Free;
	inherited;
end;

function TLuaTable.GetItemByKey(const key : String) : TLuaObject;
var
	I : Longint;
//	item : TLuaObject;
begin
	if PrintDataKeysOnAccess then
		WriteLn(key);
{	
	for I := 0 to items.Count - 1 do
	begin
		item := TLuaObject(items[I]);
		if item.name = key then
		begin
			Result := item;
			Exit;
		end;
	end;
}

	if items_map.Find(key, I) then
		Result := items_map.Data[I]
	else
		Result := nil;
end;

function TLuaTable.GetItemById(id : Longint) : TLuaObject;
begin
	if (id >= 0) and (id < items.Count) then
		Result := items[id]
	else
		Result := nil;
end;

function TLuaTable.GetItemCount : Longint;
begin
	Result := items.Count;
end;

function TLuaTable.NewTable(const nm : String) : TLuaTable;
var
	item : TLuaTable;
begin
	item := TLuaTable.Create;
	item.name := nm;
	
	items.Add(item);
	
	// TODO handle name conflicts ?
	if item.name <> '' then
		items_map.AddOrSetData(item.name, item);
	
	Result := item;
end;

function TLuaTable.NewReal(const nm : String; val : Double) : TLuaReal;
var
	item : TLuaReal;
begin
	item := TLuaReal.Create(val);
	item.name := nm;
	
	items.Add(item);
	
	// TODO handle name conflicts ?
	if item.name <> '' then
		items_map.AddOrSetData(item.name, item);
	
	Result := item;	
end;

function TLuaTable.NewNumber(const nm : String; val : Int64) : TLuaNumber;
var
	item : TLuaNumber;
begin
	item := TLuaNumber.Create(val);
	item.name := nm;
	
	items.Add(item);
	
	// TODO handle name conflicts ?
	if item.name <> '' then
		items_map.AddOrSetData(item.name, item);
	
	Result := item;	
end;

function TLuaTable.NewBoolean(const nm : String; val : Boolean) : TLuaBoolean;
var
	item : TLuaBoolean;
begin
	item := TLuaBoolean.Create(val);
	item.name := nm;
	
	items.Add(item);
	
	// TODO handle name conflicts ?
	if item.name <> '' then
		items_map.AddOrSetData(item.name, item);
	
	Result := item;	
end;

function TLuaTable.NewString(const nm, val : String) : TLuaString;
var
	item : TLuaString;
begin
	item := TLuaString.Create(val);
	item.name := nm;
	
	items.Add(item);
	
	// TODO handle name conflicts ?
	if item.name <> '' then
		items_map.AddOrSetData(item.name, item);
	
	Result := item;	
end;

constructor TLuaReal.Create(val : Double);
begin
	inherited Create;
	lua_type := ltReal;
	value := val;
end;

constructor TLuaNumber.Create(val : Int64);
begin
	inherited Create;
	lua_type := ltNumber;
	value := val;
end;

constructor TLuaBoolean.Create(val : Boolean);
begin
	inherited Create;
	lua_type := ltBoolean;
	value := val;
end;

constructor TLuaString.Create(val : String);
begin
	inherited Create;
	lua_type := ltString;
	value := val;
end;

function ParseFloat(const str : String) : Double;
var
	tf : TFormatSettings;
begin
	tf.DecimalSeparator := '.';
	Result := StrToFloat(str, tf);
end;

function PrintFloat(val : Double) : String;
var
	tf : TFormatSettings;
begin
	tf.DecimalSeparator := '.';
	Result := FloatToStr(val, tf);
end;

function ParseLuaVariable(parser : TParser; allowEOF, allowEOT : Boolean) : TLuaObject; forward;

procedure ParseLuaTable(parser : TParser; table : TLuaTable);
var
	t1 : String;
	item : TLuaObject;
begin
	item := ParseLuaVariable(parser, False, True);
	while item <> nil do
	begin
		table.items.Add(item);
		
		// TODO handle name conflicts ?
		if item.name <> '' then
			table.items_map.AddOrSetData(item.name, item);
		
		parser.NextToken(t1);
		if t1 = '}' then
			Break
		else if t1 <> ',' then
			parser.Error(''','' expected');
			
		item := ParseLuaVariable(parser, False, True);
	end;
end;

function ParseLuaValue(parser : TParser; token : PAnsiString = nil; token_type : PTokenType = nil) : TLuaObject;
var
	t1, t2 : String;
	ttype : TTokenType;
	table : TLuaTable;
begin
	// what a ugly construction >_>
	if (token <> nil) and (token_type <> nil) then
	begin
		t1 := token^;
		ttype := token_type^;
	end else
		parser.NextToken(t1, False, @ttype);
	
	if t1 = '{' then
	begin
		table := TLuaTable.Create;
		try
			ParseLuaTable(parser, table);
		except
			table.Free;
			raise;
		end;
		
		Result := table;
	end else if (t1 = 'create_section')  and (ttype = tokIdentifier) then
	begin
		parser.NextToken(t2);
		if t2 <> '{' then
			parser.Error('''{'' expected');
			
		table := TLuaTable.Create;
		try
			ParseLuaTable(parser, table);
		except
			table.Free;
			raise;
		end;
		
		Result := table;
	end else if (t1 = 'nil') and (ttype = tokIdentifier) then
		Result := TLuaObject.Create
	else if (t1 = 'true') and (ttype = tokIdentifier) then
		Result := TLuaBoolean.Create(True)
	else if (t1 = 'false') and (ttype = tokIdentifier) then
		Result := TLuaBoolean.Create(False)
	else if IsIntegerStr(t1) and (ttype = tokIdentifier) then
		Result := TLuaNumber.Create(StrToInt(t1))
	else if IsRealStr(t1) and (ttype = tokIdentifier) then
		Result := TLuaReal.Create(ParseFloat(t1))
	else if ttype = tokString then
		Result := TLuaString.Create(t1)
	else 
		parser.Error('value ''' + t1 + ''' is unsupported')
end;

function ParseLuaVariable(parser : TParser; allowEOF, allowEOT : Boolean) : TLuaObject;
var
	t1, t2, t3, t4 : String;
	ttype : TTokenType;
begin
	if not parser.NextToken(t1, allowEOF, @ttype) then
	begin
		// end of file
		Result := nil;
		Exit;
	end;
		
	if allowEOT and (t1 = '}') then
	begin
		// end of table (for empty tables)
		Result := nil;
		Exit;
	end;
	
	if allowEOF and (t1 = ';') then
	begin
		while t1 = ';' do
		begin
			if not parser.NextToken(t1, allowEOF, @ttype) then
			begin
				// end of file
				Result := nil;
				Exit;
			end;
		end;
	end else	
	if allowEOF and (t1 = ',') then
	begin
		if not parser.NextToken(t1, allowEOF, @ttype) then
		begin
			// end of file
			Result := nil;
			Exit;
		end;
	end;
	
	// some value
	if (ttype = tokIdentifier) and IsLuaIdentifier(t1) and (t1 <> 'create_section') then
	begin
		parser.NextToken(t2);
		if t2 <> '=' then
			parser.Error('''='' expected');
			
		Result := ParseLuaValue(parser);
		Result.name := t1;
	end else 
	if t1 = '[' then
	begin
		parser.NextToken(t2);
		
		parser.NextToken(t3);
		if t3 <> ']' then
			parser.Error(''']'' expected');
			
		parser.NextToken(t4);
		if t4 <> '=' then
			parser.Error('''='' expected');
			
		Result := ParseLuaValue(parser);
		Result.name := t2;
	end else
	begin
		Result := ParseLuaValue(parser, @t1, @ttype);
		Result.name := '';
	end;
end;

function ParseLuaCode(parser : TParser; obj : TLuaTable) : Integer;
var
	count : Integer;
	item : TLuaObject;
begin
	count := 0;
	
	item := ParseLuaVariable(parser, True, False);
	while item <> nil do
	begin
		Inc(count);
		obj.items.Add(item);
		if item.name <> '' then
			obj.items_map.AddOrSetData(item.name, item);
		item := ParseLuaVariable(parser, True, False);
	end;
	
	Result := count;
end;

procedure ParseLuaFile(obj : TLuaTable; const filename : String);
var
	f : TFileStream;
	source : String;
begin
	f := TFileStream.Create(filename, fmOpenRead);
	try
		SetLength(source, f.Size);
		f.ReadBuffer(source[1], f.Size);
		ParseLuaString(obj, source);
	finally
		f.Free;
	end;
end;

procedure ParseLuaString(obj : TLuaTable; const source : String);
var
	parser : TParser;
begin
	parser := TParser.Create(source, [',', '=', '{', '}', '[', ']', ';'], True);
	try
		parser.StripCommentsLUA;
		parser.StripCommentsCPlusPlus; // bruh
		ParseLuaCode(parser, obj);
	finally
		parser.Free;
	end;
end;

procedure _PrintTable(w : TMemoryWriter; obj : TLuaTable; indent : Longint);
var
	I : Longint;
	item : TLuaObject;
	val_real : Single;
begin
	for I := 0 to obj.items.Count - 1 do
	begin
		item := TLuaObject(obj.items[I]);
		
		w.WriteString(StringOfChar(#9, indent));
		
		if item.name <> '' then
		begin
			if IsLuaIdentifier(item.name) then
			begin
				w.WriteString(item.name);
			end else
			begin
				w.WriteString('["');
				PrintEscapedString(w, item.name, '"');
				w.WriteString('"]');
			end;
			
			w.WriteString(' = ');
		end;
		
		case item.DataType of
			ltNil: 
				w.WriteString('nil');
			ltBoolean: 
				if item.AsBoolean then 
					w.WriteString('true') 
				else 
					w.WriteString('false');
			ltString: begin 
				w.WriteString('"'); 
				PrintEscapedString(w, item.AsString, '"');
				w.WriteString('"'); 
			end;
			ltNumber: 
				w.WriteString(IntToStr(item.AsNumber));
			ltReal: begin
				val_real := item.AsReal;
				
				// weird 4A way to write very small numbers
				// why ? Lua supports scientific notation too !
				if (Abs(val_real) < 0.0000015) and (val_real <> 0.0) then
				begin
					w.WriteString('"');
					w.WriteString(IntToStr(Longword(val_real)));
					w.WriteString('" /*');
					w.WriteString(PrintFloat(val_real));
					w.WriteString('*/');
				end else
					w.WriteString(PrintFloat(val_real));
			end;
			ltTable: begin 
				w.WriteString('create_section {'#10); 
				_PrintTable(w, item.AsTable, indent+1);
				w.WriteString(StringOfChar(#9, indent));
				w.WriteString('}');
			end;
		end;
		
		if I <> (obj.items.Count - 1) then
			w.WriteString(',');
			
		w.WriteString(#10);
	end;
end;

function PrintLuaTable(obj : TLuaTable) : String;
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	
	_PrintTable(w,obj,0);
	
	SetLength(Result, w.size);
	Move(w.data[0], Result[1], w.size);
	
	w.Free;
end;

procedure PrintLuaTableToFile(obj : TLuaTable; const fn : String);
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	
	_PrintTable(w,obj,0);
	
	w.SaveTo(fn);
	w.Free;
end;

end.