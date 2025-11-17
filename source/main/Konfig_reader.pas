unit Konfig_reader;

interface
uses chunkedFile, Konfig, mujs, vmath, LUATable;

type
	TStringRecoverFunc = function(crc : Longint; userdata : Pointer) : String;
	
type
	TU8Array = array of Byte;
	TU16Array = array of Word;
	TU32Array = array of Longword;
	TStrArray = array of String;
	TFP32Array = array of Single;

type
	IKonfigReader = class
		elem_fmt : String;
		cur_element : Longint;
		num_elements : Longint;
		
		function More : Boolean; virtual; abstract;
		
		function MoreElements : Boolean; virtual; abstract;
		function NextElement : IKonfigReader; virtual; abstract;

		function ReadSection(const name : String; name_check : Boolean = True) : IKonfigReader; virtual; abstract;
		function ReadArray(const name : String; count : PLongword = nil; const fmt : String = 'rec_%.4d') : IKonfigReader; virtual; abstract;
		function ReadArrayWithNoKey(count : PLongword = nil; const fmt : String = 'rec_%.4d') : IKonfigReader; virtual; abstract;
		
		function TryReadSection(const name : String) : IKonfigReader; virtual; abstract;
		function TryReadArray(const name : String; count : PLongword = nil; const fmt : String = 'rec_%.4d') : IKonfigReader; virtual; abstract;

		function ReadName(const nm : String) : String; virtual; abstract;
		function ReadStringCrc(const nm : String; f : TStringRecoverFunc; userdata : Pointer = nil) : String; virtual; abstract;
		procedure ReadHint(const nm : String; const tp : String); virtual; abstract;
		function ReadHintStr(const nm : String; const tp : String) : String; virtual; abstract;

		function ReadS8(const nm : String; const tp : String = 's8') : Shortint; virtual; abstract;
		function ReadS16(const nm : String; const tp : String = 's16') : Smallint; virtual; abstract;
		function ReadS32(const nm : String; const tp : String = 's32') : Longint; virtual; abstract;
		function ReadS64(const nm : String; const tp : String = 's64') : Int64; virtual; abstract;

		function ReadU8(const nm : String; const tp : String = 'u8') : Byte; virtual; abstract;
		function ReadU16(const nm : String; const tp : String = 'u16') : Word; virtual; abstract;
		function ReadU32(const nm : String; const tp : String = 'u32') : Longword; virtual; abstract;
		function ReadU64(const nm : String; const tp : String = 'u64') : QWord; virtual; abstract;

		function ReadFP32(const nm : String; const tp : String = 'fp32') : Single; virtual; abstract;

		function ReadString(const nm : String; const tp : String = 'stringz') : String; virtual; abstract;
		function ReadBool(const nm : String; const tp : String = 'bool') : Boolean; virtual; abstract;

		function ReadVec2(const nm : String; const tp : String = 'vec2f') : TVec2; virtual; abstract;
		function ReadVec3(const nm : String; const tp : String = 'vec3f') : TVec3; virtual; abstract;
		function ReadVec4(const nm : String; const tp : String = 'vec4f') : TVec4; virtual; abstract;
		function ReadVec4S16(const nm : String; const tp : String = 'vec4s16') : TVec4S16; virtual; abstract;
		procedure ReadVec3i(const nm : String; const tp : String = 'vec3i'); virtual; abstract;
		procedure ReadVec4i(const nm : String; const tp : String = 'vec3i'); virtual; abstract;

		procedure ReadMatrix44(const nm : String; const tp : String = 'pose, matrix'); virtual; abstract;
		procedure ReadMatrix43(const nm : String; const tp : String = 'pose, matrix_43T'); virtual; abstract;
		
		function ReadU8Array(const nm : String; const tp : String = 'u8_array') : TU8Array; virtual; abstract;
		function ReadU16Array(const nm : String; const tp : String = 'u16_array') : TU16Array; virtual; abstract;
		function ReadU32Array(const nm : String; const tp : String = 'u32_array') : TU32Array; virtual; abstract;
		function ReadFP32Array(const nm : String; const tp : String = 'fp32_array') : TFP32Array; virtual; abstract;
		function ReadU16Array16(const nm : String; const tp : String = 'u16_array') : TU16Array; virtual; abstract;
		function ReadU32Array16(const nm : String; const tp : String = 'u32_array') : TU32Array; virtual; abstract;
		function ReadFP32Array16(const nm : String; const tp : String = 'fp32_array') : TFP32Array; virtual; abstract;
		
		function ReadStrArray16(const nm : String; const tp : String = 'str_array16') : TStrArray; virtual; abstract;
		function ReadStrArray32(const nm : String; const tp : String = 'str_array32') : TStrArray; virtual; abstract;
		
		function ReadBool8(const nm : String; names : array of String; tp : String = 'bool8') : Byte; virtual; abstract;
		function ReadBool16(const nm : String; names : array of String; masks : array of Word; tp : String = 'u16') : Word; virtual; abstract;
		function ReadBool32(const nm : String; names : array of String; masks : array of Longword; tp : String = 'u32') : Longword; virtual; abstract;
		
		procedure ReadIdentifier(const nm : String); virtual; abstract;
		procedure ReadIdentifierArray(const nm : String; const tp : String = 'identifier_array'); virtual; abstract;
	end;

type
	TKonfigReader = class(IKonfigReader)
		bin_flags : Byte;
		data : TMemoryReader;
		dictionary : array of String;
		section_name : String;
		own_data : Boolean;
		
		dest_konf : TSection;
		dest_lua : TLuaTable;
		
		WarnIfDataLeft : Boolean; static;
		PrintDebugInfo : Boolean; static;

		constructor Create(K : TKonfig; D : TSection; L : TLuaTable); overload;
		constructor Create(parent : TKonfigReader; const name : String; r : TMemoryReader; D : TSection; L : TLuaTable; own_data : Boolean = False); overload;
		destructor Destroy; override;

		function More : Boolean; override;
		
		function MoreElements : Boolean; override;
		function NextElement : IKonfigReader; override;

		function ReadSection(const name : String; name_check : Boolean = True) : IKonfigReader; override;
		function ReadArray(const name : String; count : PLongword = nil; const fmt : String = 'rec_%.4d') : IKonfigReader; override;
		function ReadArrayWithNoKey(count : PLongword = nil; const fmt : String = 'rec_%.4d') : IKonfigReader; override;
		
		function TryReadSection(const name : String) : IKonfigReader; override;
		function TryReadArray(const name : String; count : PLongword = nil; const fmt : String = 'rec_%.4d') : IKonfigReader; override;

		function ReadName(const nm : String) : String; override;
		function ReadStringCrc(const nm : String; f : TStringRecoverFunc; userdata : Pointer = nil) : String; override;
		procedure ReadHint(const nm : String; const tp : String); override;
		function ReadHintStr(const nm : String; const tp : String) : String; override;

		function ReadS8(const nm : String; const tp : String = 's8') : Shortint; override;
		function ReadS16(const nm : String; const tp : String = 's16') : Smallint; override;
		function ReadS32(const nm : String; const tp : String = 's32') : Longint; override;
		function ReadS64(const nm : String; const tp : String = 's64') : Int64; override;

		function ReadU8(const nm : String; const tp : String = 'u8') : Byte; override;
		function ReadU16(const nm : String; const tp : String = 'u16') : Word; override;
		function ReadU32(const nm : String; const tp : String = 'u32') : Longword; override;
		function ReadU64(const nm : String; const tp : String = 'u64') : QWord; override;

		function ReadFP32(const nm : String; const tp : String = 'fp32') : Single; override;

		function ReadString(const nm : String; const tp : String = 'stringz') : String; override;
		function ReadBool(const nm : String; const tp : String = 'bool') : Boolean; override;

		function ReadVec2(const nm : String; const tp : String = 'vec2f') : TVec2; override;
		function ReadVec3(const nm : String; const tp : String = 'vec3f') : TVec3; override;
		function ReadVec4(const nm : String; const tp : String = 'vec4f') : TVec4; override;
		function ReadVec4S16(const nm : String; const tp : String = 'vec4s16') : TVec4S16; override;
		procedure ReadVec3i(const nm : String; const tp : String = 'vec3i'); override;
		procedure ReadVec4i(const nm : String; const tp : String = 'vec3i'); override;

		procedure ReadMatrix44(const nm : String; const tp : String = 'pose, matrix'); override;
		procedure ReadMatrix43(const nm : String; const tp : String = 'pose, matrix_43T'); override;
		
		function ReadU8Array(const nm : String; const tp : String = 'u8_array') : TU8Array; override;
		function ReadU16Array(const nm : String; const tp : String = 'u16_array') : TU16Array; override;
		function ReadU32Array(const nm : String; const tp : String = 'u32_array') : TU32Array; override;
		function ReadFP32Array(const nm : String; const tp : String = 'fp32_array') : TFP32Array; override;
		function ReadU16Array16(const nm : String; const tp : String = 'u16_array') : TU16Array; override;
		function ReadU32Array16(const nm : String; const tp : String = 'u32_array') : TU32Array; override;
		function ReadFP32Array16(const nm : String; const tp : String = 'fp32_array') : TFP32Array; override;
		
		function ReadStrArray16(const nm : String; const tp : String = 'str_array16') : TStrArray; override;
		function ReadStrArray32(const nm : String; const tp : String = 'str_array32') : TStrArray; override;
		
		function ReadBool8(const nm : String; names : array of String; tp : String = 'bool8') : Byte; override;
		function ReadBool16(const nm : String; names : array of String; masks : array of Word; tp : String = 'u16') : Word; override;
		function ReadBool32(const nm : String; names : array of String; masks : array of Longword; tp : String = 'u32') : Longword; override;

		procedure ReadIdentifier(const nm : String); override;
		procedure ReadIdentifierArray(const nm : String; const tp : String = 'identifier_array'); override;

		protected

		procedure _ReadDebugInfo(const nm, tp : String; raiseiffail : Boolean = False);
		function _ReadString : String;
		function _ReadSection(const name : String; name_check : Boolean = True) : TKonfigReader;
	end;

procedure Script_Init(J : js_State);
procedure Script_Finish(J : js_State);
procedure Script_Push(J : js_State; K : IKonfigReader);

implementation
uses uCrc, sysutils, base64 { for LUA };

constructor TKonfigReader.Create(K : TKonfig; D : TSection; L : TLuaTable);
begin
	Inherited Create;

	bin_flags := K.kind;
	if (bin_flags and konfMultiChunk) = 0 then
		data := TMemoryReader.Create(K.data[0], Length(K.data))
	else
		data := TMemoryReader.Create(K.data[8], Length(K.data)-8); // cut arch_chunk_0 section
	dictionary := K.dictionary;
	dest_konf := D;
	dest_lua := L;
	section_name := '<ROOT>';
	own_data := True;
end;

constructor TKonfigReader.Create(parent : TKonfigReader; const name : String; r : TMemoryReader; D : TSection; L : TLuaTable; own_data : Boolean);
begin
	inherited Create;

	bin_flags := parent.bin_flags;
	data := r;
	dictionary := parent.dictionary;
	dest_konf := D;
	dest_lua := L;
	section_name := name;
	self.own_data := own_data;
end;

destructor TKonfigReader.Destroy;
begin
	if WarnIfDataLeft AND More then 
		WriteLn('section ', section_name, ' data left');
	
	if own_data then
		data.Free;

	inherited;
end;

function TKonfigReader.More : Boolean;
begin
	More := data.pos < data.size;
end;

function TKonfigReader.MoreElements : Boolean;
begin
	if (bin_flags and konfNoSections) <> 0 then
		Result := cur_element < num_elements
	else
		Result := More;
end;

function TKonfigReader.NextElement : IKonfigReader;
begin
	Result := ReadSection(Format(elem_fmt, [cur_element]), False);
	Inc(cur_element);
end;

function TKonfigReader.ReadSection(const name : String; name_check : Boolean) : IKonfigReader;
var
	sect : TKonfigReader;
begin
	sect := _ReadSection(name, name_check);
	if sect <> nil then
	begin
		if Assigned(dest_konf) then
			sect.dest_konf := dest_konf.AddSect(name);
			
		if Assigned(dest_lua) then
			sect.dest_lua := dest_lua.NewTable(name);
			
		Result := sect;
	end else
		raise Exception.Create('cannot open section ''' + name + ''' (' + IntToHex(GetStringCrc(name), 8) + ')');
end;

function TKonfigReader.ReadArray(const name : String; count : PLongword; const fmt : String) : IKonfigReader;
var
	arr : IKonfigReader;
begin
	ReadHint(name, 'array');
	arr := ReadSection(name);
	arr.elem_fmt := fmt;
	arr.num_elements := arr.ReadU32('count');
	if count <> nil then
		count^ := arr.num_elements;

	Result := arr;
end;

function TKonfigReader.ReadArrayWithNoKey(count : PLongword; const fmt : String) : IKonfigReader;
var
	arr : IKonfigReader;
begin
	ReadHint('array with no key', 'array');
	arr := TKonfigReader.Create(self, 'array with no key', self.data, self.dest_konf, self.dest_lua, False);
	arr.elem_fmt := fmt;
	arr.num_elements := arr.ReadU32('count');
	if count <> nil then
		count^ := arr.num_elements;

	Result := arr;
end;

function TKonfigReader.TryReadSection(const name : String) : IKonfigReader;
var
	sect : TKonfigReader;
begin
	sect := _ReadSection(name, True);
	if sect <> nil then
	begin
		if Assigned(dest_konf) then
			sect.dest_konf := dest_konf.AddSect(name);
			
		if Assigned(dest_lua) then
			sect.dest_lua := dest_lua.NewTable(name);
			
		Result := sect;
	end else
		Result := nil;
end;

function TKonfigReader.TryReadArray(const name : String; count : PLongword; const fmt : String) : IKonfigReader;
var
	saved_pos : Longword;
	sect : TKonfigReader;
begin
	saved_pos := data.pos;

	try 
		_ReadDebugInfo(name, 'array', True);
	except
		on E: Exception do
		begin
			data.pos := saved_pos;
			Result := nil;
			Exit;
		end
	end;
	
	sect := _ReadSection(name, True);
	if sect <> nil then
	begin
		if Assigned(dest_konf) then
		begin
			dest_konf.AddHint(name, 'array');
			sect.dest_konf := dest_konf.AddSect(name);
		end;
		
		if Assigned(dest_lua) then
			sect.dest_lua := dest_lua.NewTable(name);
		
		sect.elem_fmt := fmt;
		sect.num_elements := sect.ReadU32('count');
		if count <> nil then
			count^ := sect.num_elements;
		
		Result := sect;
	end else
		Result := nil;
end;

function TKonfigReader.ReadName(const nm : String) : String;
var
	n : String;
begin
	ReadHint(nm, 'name');
	n := ReadString(nm, 'stringz');
	if Assigned(dest_konf) then dest_konf.name := n;

	Result := n;
end;

function TKonfigReader.ReadStringCrc(const nm : String; f : TStringRecoverFunc; userdata : Pointer) : String;
var
	crc : Longint;
	n : String;
begin
	_ReadDebugInfo(nm, 'u32');
	crc := data.ReadLongint;
	n := f(crc, userdata);
	
	if Assigned(dest_konf) then 
		dest_konf.AddStr(nm, n);
		
	if Assigned(dest_lua) then
		dest_lua.NewString(nm, n);
	
	Result := n;
end;

procedure TKonfigReader.ReadHint(const nm : String; const tp : String);
begin
	_ReadDebugInfo(nm, tp);
	if Assigned(dest_konf) then dest_konf.AddHint(nm, tp);
end;

function TKonfigReader.ReadHintStr(const nm : String; const tp : String) : String;
begin
	ReadHint(nm, tp);
	Result := ReadString(nm);
end;

function TKonfigReader.ReadS8(const nm : String; const tp : String) : Shortint;
var
	I : Shortint;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadShortint;
	
	if Assigned(dest_konf) then 
		dest_konf.AddInt(nm, I, tp);
		
	if Assigned(dest_lua) then
		dest_lua.NewNumber(nm, I);
	
	Result := I;
end;

function TKonfigReader.ReadS16(const nm : String; const tp : String) : Smallint;
var
	I : Smallint;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadSmallint;
	
	if Assigned(dest_konf) then 
		dest_konf.AddInt(nm, I, tp);
		
	if Assigned(dest_lua) then
		dest_lua.NewNumber(nm, I);
	
	Result := I;
end;

function TKonfigReader.ReadS32(const nm : String; const tp : String) : Longint;
var
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadLongint;
	
	if Assigned(dest_konf) then 
		dest_konf.AddInt(nm, I, tp);
		
	if Assigned(dest_lua) then
		dest_lua.NewNumber(nm, I);
	
	Result := I;
end;

function TKonfigReader.ReadS64(const nm : String; const tp : String) : Int64;
var
	I : Int64;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadInt64;
	
	if Assigned(dest_konf) then 
		dest_konf.AddInt(nm, I, tp);
		
	if Assigned(dest_lua) then
		dest_lua.NewNumber(nm, I);
	
	Result := I;
end;

function TKonfigReader.ReadU8(const nm : String; const tp : String) : Byte;
var
	I : Byte;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadByte;
	
	if Assigned(dest_konf) then 
		dest_konf.AddInt(nm, I, tp);
		
	if Assigned(dest_lua) then
		dest_lua.NewNumber(nm, I);
	
	Result := I;
end;

function TKonfigReader.ReadU16(const nm : String; const tp : String) : Word;
var
	I : Word;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadWord;
	
	if Assigned(dest_konf) then 
		dest_konf.AddInt(nm, I, tp);
		
	if Assigned(dest_lua) then
		dest_lua.NewNumber(nm, I);
	
	Result := I;
end;

function TKonfigReader.ReadU32(const nm : String; const tp : String) : Longword;
var
	I : Longword;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadLongword;
	
	if Assigned(dest_konf) then 
		dest_konf.AddInt(nm, I, tp);
		
	if Assigned(dest_lua) then
		dest_lua.NewNumber(nm, I);
		
	Result := I;
end;

function TKonfigReader.ReadU64(const nm : String; const tp : String) : QWord;
var
	I : QWord;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadQWord;
	
	if Assigned(dest_konf) then 
		dest_konf.AddInt(nm, I, tp);

	if Assigned(dest_lua) then
		dest_lua.NewNumber(nm, I);
	
	Result := I;
end;

function TKonfigReader.ReadFP32(const nm : String; const tp : String) : Single;
var
	I : Single;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadSingle;
	
	if Assigned(dest_konf) then 
		dest_konf.AddFloat(nm, I, tp);
	
	if Assigned(dest_lua) then
		dest_lua.NewReal(nm, I);
	
	Result := I;
end;

function TKonfigReader.ReadString(const nm : String; const tp : String) : String;
var
	I : String;
begin
	_ReadDebugInfo(nm, tp);
	I := _ReadString;

	if Assigned(dest_konf) then 
		dest_konf.AddStr(nm, I, tp);
		
	if Assigned(dest_lua) then
		dest_lua.NewString(nm, I);

	Result := I;
end;

function TKonfigReader.ReadBool(const nm : String; const tp : String) : Boolean;
var
	I : Boolean;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadByte <> 0;
	
	if Assigned(dest_konf) then 
		dest_konf.AddBool(nm, I, tp);
		
	if Assigned(dest_lua) then
		dest_lua.NewBoolean(nm, I);	
	
	Result := I;
end;

function TKonfigReader.ReadVec2(const nm : String; const tp : String) : TVec2;
var
	v : TVec2;
	t : TLuaTable;
begin
	_ReadDebugInfo(nm, tp);
	data.Read(v, Sizeof(v));
	
	if Assigned(dest_konf) then 
		dest_konf.AddVec2(nm, v, tp);
		
	if Assigned(dest_lua) then
	begin
		t := dest_lua.NewTable(nm);
		t.NewReal('', v.x);
		t.NewReal('', v.y);
	end;	
	
	Result := v;
end;

function TKonfigReader.ReadVec3(const nm : String; const tp : String) : TVec3;
var
	v : TVec3;
	t : TLuaTable;
begin
	_ReadDebugInfo(nm, tp);
	data.Read(v, Sizeof(v));
	
	if Assigned(dest_konf) then 
		dest_konf.AddVec3(nm, v, tp);
		
	if Assigned(dest_lua) then
	begin
		t := dest_lua.NewTable(nm);
		t.NewReal('', v.x);
		t.NewReal('', v.y);
		t.NewReal('', v.z);
	end;
	
	Result := v;
end;

function TKonfigReader.ReadVec4(const nm : String; const tp : String) : TVec4;
var
	v : TVec4;
	t : TLuaTable;
begin
	_ReadDebugInfo(nm, tp);
	data.Read(v, Sizeof(v));
	
	if Assigned(dest_konf) then 
		dest_konf.AddVec4(nm, v, tp);
		
	if Assigned(dest_lua) then
	begin
		t := dest_lua.NewTable(nm);
		t.NewReal('', v.x);
		t.NewReal('', v.y);
		t.NewReal('', v.z);
		t.NewReal('', v.w);
	end;
		
	Result := v;
end;

function TKonfigReader.ReadVec4S16(const nm : String; const tp : String) : TVec4S16;
var
	v : TVec4S16;
	t : TLuaTable;
begin
	_ReadDebugInfo(nm, tp);
	data.Read(v, Sizeof(v));
	
	if Assigned(dest_konf) then 
		dest_konf.AddVec4S16(nm, v, tp);
		
	if Assigned(dest_lua) then
	begin
		t := dest_lua.NewTable(nm);
		t.NewNumber('', v.x);
		t.NewNumber('', v.y);
		t.NewNumber('', v.z);
		t.NewNumber('', v.w);
	end;
	
	Result := v;
end;

procedure TKonfigReader.ReadVec3i(const nm : String; const tp : String);
var
	v : TIntegerArrayValue;
begin
	_ReadDebugInfo(nm, tp);
	v := TIntegerArrayValue.Create(nm, tp);
	v.FromReaderS32(data, 3);
	if Assigned(dest_konf) then dest_konf.items.Add(v);
end;

procedure TKonfigReader.ReadVec4i(const nm : String; const tp : String);
var
	v : TIntegerArrayValue;
begin
	_ReadDebugInfo(nm, tp);
	v := TIntegerArrayValue.Create(nm, tp);
	v.FromReaderS32(data, 4);
	if Assigned(dest_konf) then dest_konf.items.Add(v);
end;

procedure TKonfigReader.ReadMatrix44(const nm : String; const tp : String);
var
	mat : array[1..16] of Single;
	v : TFloatArrayValue;
begin
	_ReadDebugInfo(nm, tp);
	data.Read(mat, Sizeof(mat));
	
	if Assigned(dest_konf) then
	begin
		v := TFloatArrayValue.Create(nm, tp);
		SetLength(v.data, 16);
		Move(mat, v.data[0], Sizeof(mat));
		dest_konf.items.Add(v);
	end;
end;

procedure TKonfigReader.ReadMatrix43(const nm : String; const tp : String);
var
	mat : array[1..12] of Single;
	v : TFloatArrayValue;
begin
	_ReadDebugInfo(nm, tp);
	data.Read(mat, Sizeof(mat));
	
	if Assigned(dest_konf) then
	begin
		v := TFloatArrayValue.Create(nm, tp);
		SetLength(v.data, 12);
		Move(mat, v.data[0], Sizeof(mat));
		dest_konf.items.Add(v);
	end;
end;

function TKonfigReader.ReadU8Array(const nm : String; const tp : String) : TU8Array;
var
	ret : TU8Array;
	arr : TByteArrayValue;
	len : Longint;
	I : Longint;
	
	t : TLuaTable;
	tmpstr : String;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadLongword;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadByte;
		
	if Assigned(dest_konf) then 
	begin
		arr := TByteArrayValue.Create(nm, tp);
		arr.data := Copy(ret, 0, len);
		dest_konf.items.Add(arr);
	end;
	
	if Assigned(dest_lua) then
	begin
		t := dest_lua.NewTable(nm);
		t.NewNumber('', Length(ret));

		if Length(ret) > 0 then
		begin		
			SetLength(tmpstr, Length(ret));
			Move(ret[0], tmpstr[1], Length(ret));
			t.NewTable('').NewString('', EncodeStringBase64(tmpstr));
		end;
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadU16Array(const nm : String; const tp : String) : TU16Array;
var
	ret : TU16Array;
	arr : TIntegerArrayValue;
	len : Longint;
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadLongword;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadWord;
		
	if Assigned(dest_konf) then 
	begin
		arr := TIntegerArrayValue.Create(nm, tp);
		SetLength(arr.data, len);
		for I := 0 to len - 1 do
			arr.data[I] := ret[I];
		dest_konf.items.Add(arr);
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadU32Array(const nm : String; const tp : String) : TU32Array;
var
	ret : TU32Array;
	arr : TIntegerArrayValue;
	len : Longint;
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadLongword;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadLongword;
		
	if Assigned(dest_konf) then 
	begin
		arr := TIntegerArrayValue.Create(nm, tp);
		SetLength(arr.data, len);
		for I := 0 to len - 1 do
			arr.data[I] := ret[I];
		dest_konf.items.Add(arr);
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadFP32Array(const nm : String; const tp : String) : TFP32Array;
var
	ret : TFP32Array;
	arr : TFloatArrayValue;
	len : Longint;
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadLongword;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadSingle;
		
	if Assigned(dest_konf) then 
	begin
		arr := TFloatArrayValue.Create(nm, tp);
		SetLength(arr.data, len);
		for I := 0 to len - 1 do
			arr.data[I] := ret[I];
		dest_konf.items.Add(arr);
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadU16Array16(const nm : String; const tp : String) : TU16Array;
var
	ret : TU16Array;
	arr : TIntegerArrayValue;
	len : Longint;
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadWord;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadWord;
		
	if Assigned(dest_konf) then 
	begin
		arr := TIntegerArrayValue.Create(nm, tp);
		SetLength(arr.data, len);
		for I := 0 to len - 1 do
			arr.data[I] := ret[I];
		dest_konf.items.Add(arr);
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadU32Array16(const nm : String; const tp : String) : TU32Array;
var
	ret : TU32Array;
	arr : TIntegerArrayValue;
	len : Longint;
	I : Longint;
	
	t : TLuaTable;
	tmpstr : String;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadWord;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadLongword;
		
	if Assigned(dest_konf) then 
	begin
		arr := TIntegerArrayValue.Create(nm, tp);
		SetLength(arr.data, len);
		for I := 0 to len - 1 do
			arr.data[I] := ret[I];
		dest_konf.items.Add(arr);
	end;
	
	if Assigned(dest_lua) then
	begin
		t := dest_lua.NewTable(nm);
		t.NewNumber('', Length(ret));

		if Length(ret) > 0 then
		begin		
			SetLength(tmpstr, Length(ret)*Sizeof(Longword));
			Move(ret[0], tmpstr[1], Length(ret)*Sizeof(Longword));
			t.NewTable('').NewString('', EncodeStringBase64(tmpstr));
		end;
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadFP32Array16(const nm : String; const tp : String) : TFP32Array;
var
	ret : TFP32Array;
	arr : TFloatArrayValue;
	len : Longint;
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadWord;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadSingle;
		
	if Assigned(dest_konf) then 
	begin
		arr := TFloatArrayValue.Create(nm, tp);
		SetLength(arr.data, len);
		for I := 0 to len - 1 do
			arr.data[I] := ret[I];
		dest_konf.items.Add(arr);
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadStrArray16(const nm : String; const tp : String) : TStrArray;
var
	I, len : Longint;
	ret : TStrArray;
	s : String;
begin
	ReadHint(nm, tp);
	len := ReadU16(nm);
	SetLength(ret, len);
	for I := 0 to len-1 do
		ret[I] := ReadString(nm);
		
	if Assigned(dest_lua) then
	begin
		if len > 0 then
		begin
			s := ret[0];
			for I := 1 to len-1 do
				s := s + ',' + ret[I];
		end else
			s := '';
			
		dest_lua.NewString(nm, s);
	end;
		
	Result := ret;
end;

function TKonfigReader.ReadStrArray32(const nm : String; const tp : String) : TStrArray;
var
	I, len : Longint;
	ret : TStrArray;
	s : String;
begin
	ReadHint(nm, tp);
	len := ReadU32(nm);
	SetLength(ret, len);
	for I := 0 to len-1 do
		ret[I] := ReadString(nm);
		
	if Assigned(dest_lua) then
	begin
		if len > 0 then
		begin
			s := ret[0];
			for I := 1 to len-1 do
				s := s + ',' + ret[I];
		end else
			s := '';
			
		dest_lua.NewString(nm, s);
	end;
		
	Result := ret;
end;

function TKonfigReader.ReadBool8(const nm : String; names : array of String; tp : String) : Byte;
var
	I : Byte;
	n : Longint;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadByte;
	
	if Assigned(dest_konf) then 
		dest_konf.AddInt(nm, I, tp);	
		
	if Assigned(dest_lua) then
		for n := 0 to Length(names) - 1 do
			if ((1 shl n) and I) <> 0 then
				dest_lua.NewBoolean(names[n], True)
			else
				dest_lua.NewBoolean(names[n], False);
		
	Result := I;
end;

function TKonfigReader.ReadBool16(const nm : String; names : array of String; masks : array of Word; tp : String) : Word;
var
	I : Word;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadWord;
	if Assigned(dest_konf) then dest_konf.AddInt(nm, I, tp);	
	Result := I;
end;

function TKonfigReader.ReadBool32(const nm : String; names : array of String; masks : array of Longword; tp : String) : Longword;
var
	I : Word;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadLongword;
	if Assigned(dest_konf) then dest_konf.AddInt(nm, I, tp);	
	Result := I;
end;

procedure TKonfigReader.ReadIdentifier(const nm : String);
begin
	ReadU32(nm);
end;

procedure TKonfigReader.ReadIdentifierArray(const nm : String; const tp : String);
begin
	ReadU32Array(nm, tp);
end;

procedure TKonfigReader._ReadDebugInfo(const nm, tp : String; raiseiffail : Boolean);
var
	n, t : String;
	
	procedure Error(const msg : String);
	begin
		if raiseiffail then
			raise Exception.Create(msg)
		else
			WriteLn('Error! ' + msg);
	end;
begin
	if PrintDebugInfo then
		WriteLn(nm, ' ', tp);
		
	if (bin_flags and konfDebugInfo) <> 0 then
	begin
		n := _ReadString;
		t := _ReadString;
		if (n <> nm) or (t <> tp) then
			Error('name or type doesn''t match! ' + nm + ' : ' + tp + ' <> ' + n + ' : ' + t);
	end;
end;

function TKonfigReader._ReadString : String;
var
	index : Longword;
begin
	if (bin_flags and konfDiktionary) <> 0 then
	begin
		index := data.ReadLongword;
		if index >= Length(dictionary) then
			raise Exception.Create('word index out of range');
		_ReadString := dictionary[index];
	end else
		_ReadString := data.ReadStringZ;
end;

function TKonfigReader._ReadSection(const name : String; name_check : Boolean) : TKonfigReader;
var
	crc : Longint;
	size : Longword;
	
	n : String;
	saved_pos : Longword;
	start_pos : Longword;
begin
	if (bin_flags and konfNoSections) <> 0 then
	begin
		// TODO ReadHint(name, 'section') ?
		Result := TKonfigReader.Create(
			self,
			name,
			data,
			nil,
			nil,
			False
		);
		Exit;
	end;

	if data.size - data.pos < 8 then
	begin
		Result := nil;
		Exit;
	end;
	
	saved_pos := data.pos;

	crc := data.ReadLongint;
	if (not name_check) or (GetStringCrc(name) = crc) then
	begin
		size := data.ReadLongword;

		start_pos := data.pos;

		// TODO other kinds(LL skeleton)
		if (bin_flags and konfMultiChunk) = 0 then
		begin
			if data.ReadByte <> bin_flags then
				raise Exception.Create('buffer[0] <> bin_flags ololo blea');
		end;
		
		if (bin_flags and konfDebugInfo) <> 0 then
		begin
			n := _ReadString;
			if name_check and (name <> n) then
			begin
				data.pos := saved_pos;
				Result := nil;
				Exit;
			end;
		end;
		
		size := size - (data.pos - start_pos);

		Result := TKonfigReader.Create(
			self,
			name,
			TMemoryReader.Create((data.data + data.pos)^, size),
			nil,
			nil
		);

		Inc(data.pos, size);
	end else
	begin
		data.pos := saved_pos;
		Result := nil;
	end;
end;

// ------------------------------------
// JS part
// ------------------------------------

const
	TAG = 'IKONFIGREADER';

procedure ArgsThis(J : js_State; var this : IKonfigReader);
begin
	this := IKonfigReader(js_touserdata(J, 0, TAG));
	if this = nil then
		js_typeerror(J, 'cannot convert ''this'' to IKonfigReader');
end;

procedure ArgsThis1Str(J : js_State; var this : IKonfigReader; var s : PAnsiChar; def : PAnsiChar);
begin
	if js_gettop(J) >= 2 then 
		s := js_trystring(J, 1, def)
	else 
		s := def;

	this := IKonfigReader(js_touserdata(J, 0, TAG));
	if this = nil then
		js_typeerror(J, 'cannot convert ''this'' to IKonfigReader');
		
	if s = nil then
		js_typeerror(J, 'missing argument 1 to call function');
end;

procedure ArgsThis2Str(J : js_State; var this : IKonfigReader; var s1, s2 : PAnsiChar; def1, def2 : PAnsiChar);
begin
	if js_gettop(J) >= 2 then 
		s1 := js_trystring(J, 1, def1)
	else 
		s1 := def1;

	if js_gettop(J) >= 3 then 
		s2 := js_trystring(J, 2, def2)
	else 
		s2 := def2;

	this := IKonfigReader(js_touserdata(J, 0, TAG));
	if this = nil then
		js_typeerror(J, 'cannot convert ''this'' to IKonfigReader');
		
	if s1 = nil then
		js_typeerror(J, 'missing argument 1 to call function');
	if s2 = nil then
		js_typeerror(J, 'missing argument 2 to call function');
end;

procedure S_Size(J : js_State); cdecl;
var
	this : IKonfigReader;
begin
	ArgsThis(J, this);
	try
		//js_pushnumber(J, this.data.size)
		js_pushnumber(J, 0)
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_More(J : js_State); cdecl;
var
	this : IKonfigReader;
begin
	ArgsThis(J, this);
	
	try
		js_pushboolean(J, Longint(this.More))
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_MoreElements(J : js_State); cdecl;
var
	this : IKonfigReader;
begin
	ArgsThis(J, this);
	try
		js_pushboolean(J, Longint(this.MoreElements))
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_NextElement(J : js_State); cdecl;
var
	this : IKonfigReader;
begin
	ArgsThis(J, this);
	try
		Script_Push(J, this.NextElement);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadSection(J : js_State); cdecl;
var
	this : IKonfigReader;
	name : PAnsiChar;
	check : Boolean;
begin
	if js_gettop(J) = 1 then
	begin
		// no args
		ArgsThis(J, this);
		name := '';
		check := False;
	end else
	begin
		ArgsThis1Str(J, this, name, nil);
	
		if js_gettop(J) >= 3 then check := (js_tryboolean(J, 2, 1) = 1)
		else check := True;
	end;

	try 
		Script_Push(J, this.ReadSection(name, check));	
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadArray(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, format : PAnsiChar;
	count : Longword;
begin
	ArgsThis2Str(J, this, name, format, nil, 'rec_%.4d');
	try
		Script_Push(J, this.ReadArray(name, @count, format));
		js_pushnumber(J, count);
		js_setproperty(J, -2, 'count');	
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadArrayWithNoKey(J : js_State); cdecl;
var
	this : IKonfigReader;
	format : PAnsiChar;
	count : Longword;
begin
	ArgsThis1Str(J, this, format, 'rec_%.4d');
	try
		Script_Push(J, this.ReadArrayWithNoKey(@count, format));
		js_pushnumber(J, count);
		js_setproperty(J, -2, 'count');	
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_TryReadSection(J : js_State); cdecl;
var
	this : IKonfigReader;
	name : PAnsiChar;
begin
	ArgsThis1Str(J, this, name, nil);
	try
		Script_Push(J, this.TryReadSection(name));		
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_TryReadArray(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, format : PAnsiChar;
	count : Longword;
begin
	ArgsThis2Str(J, this, name, format, nil, 'rec_%.4d');
	try
		Script_Push(J, this.TryReadArray(name, @count, format));	
		if js_isnull(J, -1) = 0 then
		begin
			js_pushnumber(J, count);
			js_setproperty(J, -2, 'count');
		end;	
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadName(J : js_State); cdecl;
var
	this : IKonfigReader;
	name : PAnsiChar;
begin
	ArgsThis1Str(J, this, name, nil);
	try
		js_pushstring(J, PAnsiChar(this.ReadName(name)))
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

function ReadStringCrc_helper(crc : Longint; userdata : Pointer) : String;
var
	J : js_State;
begin
	J := js_State(userdata);
	
	js_copy(J, 2);					// push function to call
	js_copy(J, 0);					// push 'this'
	js_pushnumber(J, Longword(crc));	// push crc as 1st argument
	
	js_pcall(J, 1);
	Result := js_trystring(J, js_gettop(J)-1, '');
end;

procedure S_ReadStringCrc(J : js_State); cdecl;
var
	this : IKonfigReader;
	name : PAnsiChar;
	s : String;
begin
	ArgsThis1Str(J, this, name, nil);
	
	if js_iscallable(J, 2) = 0 then
		js_typeerror(J, 'TKonfigReader.ReadStringCrc: argument 2 is not callable');
	
	try
		s := this.ReadStringCrc(name, ReadStringCrc_helper, Pointer(J));
		js_pushstring(J, PAnsiChar(s));
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadHint(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, nil);
	try
		this.ReadHint(name, _type);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;

	js_pushundefined(J);
end;

procedure S_ReadHintStr(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, nil);
	try
		this.ReadHint(name, _type);
		js_pushstring(J, PAnsiChar(this.ReadString(name)));
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadS8(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 's8');
	try
		js_pushnumber(J, this.ReadS8(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadS16(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 's16');
	try
		js_pushnumber(J, this.ReadS16(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadS32(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 's32');
	try
		js_pushnumber(J, this.ReadS32(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadS64(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 's64');
	try
		this.ReadS64(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU8(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u8');
	try
		js_pushnumber(J, this.ReadU8(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU16(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u16');
	try
		js_pushnumber(J, this.ReadU16(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU32(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u32');
	try
		js_pushnumber(J, this.ReadU32(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU64(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u64');
	try
		this.ReadU64(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadFP32(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'fp32');
	try
		js_pushnumber(J, this.ReadFP32(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadString(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'stringz');
	try
		js_pushstring(J, PAnsiChar(this.ReadString(name, _type)))
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadBool(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'bool');
	try
		js_pushboolean(J, Longint(this.ReadBool(name, _type)))
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;


procedure S_ReadVec2(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'vec2f');
	try
		this.ReadVec2(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadVec3(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'vec3f');
	try
		this.ReadVec3(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadVec4(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'vec4f');
	try
		this.ReadVec4(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadVec4S16(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'vec4s16');
	try
		this.ReadVec4S16(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadVec3i(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'vec3i');
	try
		this.ReadVec3i(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadVec4i(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'vec4i');
	try
		this.ReadVec4i(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadMatrix44(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'pose, matrix');
	try
		this.ReadMatrix44(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadMatrix43(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'pose, matrix_43T');
	try
		this.ReadMatrix43(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU8Array(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
	arr : TU8Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u8_array');
	try
		arr := this.ReadU8Array(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU16Array(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
	arr : TU16Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u16_array');
	try
		arr := this.ReadU16Array(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU32Array(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
	arr : TU32Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u32_array');
	try
		arr := this.ReadU32Array(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadFP32Array(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
	arr : TFP32Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'fp32_array');
	try
		arr := this.ReadFP32Array(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU16Array16(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
	arr : TU16Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u16_array');
	try
		arr := this.ReadU16Array16(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU32Array16(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
	arr : TU32Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u32_array16');
	try
		arr := this.ReadU32Array16(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadFP32Array16(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
	arr : TFP32Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'fp32_array');
	try
		arr := this.ReadFP32Array16(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadStrArray16(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
	arr : TStrArray;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'str_array16');
	try
		arr := this.ReadStrArray16(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newstring(J, PAnsiChar(arr[I]));
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadStrArray32(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
	arr : TStrArray;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'str_array32');
	try
		arr := this.ReadStrArray32(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newstring(J, PAnsiChar(arr[I]));
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadBool8(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
	names : array of String;
	I : Longint;
begin
	ArgsThis1Str(J, this, name, nil);
	
	if (js_gettop(J) >= 3) and (js_isarray(J, 2) <> 0) then
	begin
		SetLength(names, js_getlength(J, 2));
		for I := 0 to Length(names) - 1 do
		begin
			js_getindex(J, 2, I);
			names[I] := js_trystring(J, -1, PAnsiChar('bool'+IntToStr(I)));
			js_pop(J, 1);
		end; 
	end else
		SetLength(names, 0);
		
	if js_gettop(J) >= 4 then
		_type := js_trystring(J, 3, 'bool8')
	else
		_type := 'bool8';

	try
		js_pushnumber(J, this.ReadBool8(name, names, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;
	
procedure S_ReadBool16(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
	names : array of String;
	masks : array of Word;
	I : Longint;
begin
	ArgsThis1Str(J, this, name, nil);
	
	if (js_gettop(J) >= 3) and (js_isarray(J, 2) <> 0) then
	begin
		SetLength(names, js_getlength(J, 2));
		for I := 0 to Length(names) - 1 do
		begin
			js_getindex(J, 2, I);
			names[I] := js_trystring(J, -1, PAnsiChar('bool'+IntToStr(I)));
			js_pop(J, 1);
		end; 
	end else
		SetLength(names, 0);
		
	if (js_gettop(J) >= 4) and (js_isarray(J, 3) <> 0) then
	begin
		SetLength(masks, js_getlength(J, 3));
		for I := 0 to Length(masks) - 1 do
		begin
			js_getindex(J, 3, I);
			masks[I] := js_tryinteger(J, -1, 0);
			js_pop(J, 1);
		end; 
	end else
		SetLength(masks, 0);
		
	if js_gettop(J) >= 5 then
		_type := js_trystring(J, 4, 'u16')
	else
		_type := 'u16';

	try
		js_pushnumber(J, this.ReadBool16(name, names, masks, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadBool32(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
	names : array of String;
	masks : array of Longword;
	I : Longint;
begin
	ArgsThis1Str(J, this, name, nil);
	
	if (js_gettop(J) >= 3) and (js_isarray(J, 2) <> 0) then
	begin
		SetLength(names, js_getlength(J, 2));
		for I := 0 to Length(names) - 1 do
		begin
			js_getindex(J, 2, I);
			names[I] := js_trystring(J, -1, PAnsiChar('bool'+IntToStr(I)));
			js_pop(J, 1);
		end; 
	end else
		SetLength(names, 0);
		
	if (js_gettop(J) >= 4) and (js_isarray(J, 3) <> 0) then
	begin
		SetLength(masks, js_getlength(J, 3));
		for I := 0 to Length(masks) - 1 do
		begin
			js_getindex(J, 3, I);
			masks[I] := js_tryinteger(J, -1, 0);
			js_pop(J, 1);
		end; 
	end else
		SetLength(masks, 0);
		
	if js_gettop(J) >= 5 then
		_type := js_trystring(J, 4, 'u32')
	else
		_type := 'u32';

	try
		js_pushnumber(J, this.ReadBool32(name, names, masks, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadIdentifier(J : js_State); cdecl;
var
	this : IKonfigReader;
	name : PAnsiChar;
begin
	ArgsThis1Str(J, this, name, nil);
	try
		this.ReadIdentifier(name);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadIdentifierArray(J : js_State); cdecl;
var
	this : IKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'identifier_array');
	try
		this.ReadIdentifierArray(name, _type);
		js_pushundefined(J);
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

	DefFunc(S_More, 'More', 0);
	DefFunc(S_MoreElements, 'MoreElements', 0);
	DefFunc(S_NextElement, 'NextElement', 0);
	DefFunc(S_Size, 'Size', 0);

	DefFunc(S_ReadSection, 'ReadSection', 0);
	DefFunc(S_ReadArray, 'ReadArray');
	DefFunc(S_ReadArrayWithNoKey, 'ReadArrayWithNoKey', 0);
	
	DefFunc(S_TryReadSection, 'TryReadSection');
	DefFunc(S_TryReadArray, 'TryReadArray');

	DefFunc(S_ReadName, 'ReadName');
	DefFunc(S_ReadStringCrc, 'ReadStringCrc', 2);
	DefFunc(S_ReadHint, 'ReadHint', 2);
	DefFunc(S_ReadHintStr, 'ReadHintStr', 2);

	DefFunc(S_ReadS8,	'ReadS8');
	DefFunc(S_ReadS16, 'ReadS16');
	DefFunc(S_ReadS32, 'ReadS32');
	DefFunc(S_ReadS64, 'ReadS64');

	DefFunc(S_ReadU8,	'ReadU8');
	DefFunc(S_ReadU16, 'ReadU16');
	DefFunc(S_ReadU32, 'ReadU32');
	DefFunc(S_ReadU64, 'ReadU64');

	DefFunc(S_ReadFP32, 'ReadFP32');

	DefFunc(S_ReadString, 'ReadString');
	DefFunc(S_ReadBool, 'ReadBool');

	// should they return vectors to JS ??
	DefFunc(S_ReadVec2, 'ReadVec2');
	DefFunc(S_ReadVec3, 'ReadVec3');
	DefFunc(S_ReadVec4, 'ReadVec4');
	DefFunc(S_ReadVec4S16, 'ReadVec4S16');
	DefFunc(S_ReadVec3i, 'ReadVec3i');
	DefFunc(S_ReadVec4i, 'ReadVec4i');

	DefFunc(S_ReadMatrix44, 'ReadMatrix44');
	DefFunc(S_ReadMatrix43, 'ReadMatrix43');
	
	DefFunc(S_ReadU8Array, 'ReadU8Array');
	DefFunc(S_ReadU16Array, 'ReadU16Array');
	DefFunc(S_ReadU32Array, 'ReadU32Array');
	DefFunc(S_ReadFP32Array, 'ReadFP32Array');
	DefFunc(S_ReadU32Array16, 'ReadU32Array16');
	DefFunc(S_ReadU16Array16, 'ReadU16Array16');
	DefFunc(S_ReadFP32Array16, 'ReadFP32Array16');
	
	DefFunc(S_ReadStrArray16, 'ReadStrArray16');
	DefFunc(S_ReadStrArray32, 'ReadStrArray32');
	
	DefFunc(S_ReadBool8, 'ReadBool8');
	DefFunc(S_ReadBool16, 'ReadBool16');
	DefFunc(S_ReadBool32, 'ReadBool32');
	
	DefFunc(S_ReadIdentifier, 'ReadIdentifier');
	DefFunc(S_ReadIdentifierArray, 'ReadIdentifierArray');

	js_setregistry(J, 'IKonfigReader_prototype');
end;

procedure Script_Finish(J : js_State);
begin
	js_delregistry(J, 'IKonfigReader_prototype');
end;

procedure IKonfigReader_finalize(J : js_State; p : Pointer); cdecl;
var
	kr : IKonfigReader;
begin
	kr := IKonfigReader(p);
	kr.Free;
end;

procedure Script_Push(J : js_State; K : IKonfigReader);
begin
	if Assigned(K) then
	begin
		js_getregistry(J, 'IKonfigReader_prototype');
		js_newuserdata(J, TAG, Pointer(K), IKonfigReader_finalize);
	end else
		js_pushnull(J);
end;

end.