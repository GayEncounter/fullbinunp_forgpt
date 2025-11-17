unit Lua_reader;

interface
uses Konfig, mujs, vmath, LUATable, Konfig_reader;

type
	TLuaReader = class(IKonfigReader)
		table : TLuaTable;
		dest : TSection;
		
		count_ref : TIntegerValue;

		constructor Create(L : TLuaTable; D : TSection);
		constructor CreateArray(L : TLuaTable; D : TSection; count : Longint; const fmt : String = 'rec_%.4d');
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
		
		// maybe merge these two?
		function ReadBool16(const nm : String; names : array of String; masks : array of Word; tp : String = 'u16') : Word; override;
		function ReadBool32(const nm : String; names : array of String; masks : array of Longword; tp : String = 'u32') : Longword; override;
		
		procedure ReadIdentifier(const nm : String); override;
		procedure ReadIdentifierArray(const nm : String; const tp : String = 'identifier_array'); override;
	end;

implementation
uses 
	sysutils, // uh, come on, this unit needed almost everywhere!
	uCrc, // for identifier_array
	Math, // for Min
	base64,
	chunkedFile;

constructor TLuaReader.Create(L : TLuaTable; D : TSection);
begin
	inherited Create;

	table := L;
	dest := D;
end;

constructor TLuaReader.CreateArray(L : TLuaTable; D : TSection; count : Longint; const fmt : String = 'rec_%.4d');
begin
	inherited Create;

	table := L;
	dest := D;
	
	elem_fmt := fmt;
	num_elements := count;
end;

destructor TLuaReader.Destroy;
begin
	inherited;
end;

function TLuaReader.More : Boolean;
begin
	// Yack! This function is not acceptable for lua tables in any way
	More := False;
end;

function TLuaReader.MoreElements : Boolean;
begin
	MoreElements := (table[Format(elem_fmt, [cur_element])] <> nil);
end;

function TLuaReader.NextElement : IKonfigReader;
begin
	Result := ReadSection(Format(elem_fmt, [cur_element]), False);
	Inc(cur_element);
	
	if Assigned(count_ref) then
		count_ref.num := cur_element;
end;

function TLuaReader.ReadSection(const name : String; name_check : Boolean) : IKonfigReader;
var
	sect : TLuaObject;
	D : TSection;
begin
	sect := table[name];
	if sect <> nil then
	begin
		if Assigned(dest) then
			D := dest.AddSect(name)
		else
			D := nil;
					
		Result := TLuaReader.Create(sect.AsTable, D)
	end else
		raise Exception.Create('cannot open section ''' + name + '''');
end;

function TLuaReader.ReadArray(const name : String; count : PLongword; const fmt : String) : IKonfigReader;
var
	arr : TLuaObject;
	D : TSection;
	C : TIntegerValue;
	
	data : String;
	len : Longint;
	
	bd : TLuaTable;
	r : TMemoryReader;
	k : TKonfig;
	
	lr : TLuaReader;
begin
	arr := table[name];
	if arr <> nil then
	begin
		ReadHint(name, 'array');
		
		if Assigned(dest) then
			D := dest.AddSect(name)
		else
			D := nil;
		
		if arr.AsTable['__binary_data__'] <> nil then
		begin
			bd := arr.AsTable['__binary_data__'].AsTable;
			
			data := DecodeStringBase64(bd.ItemById[1].AsTable.ItemById[0].AsString);
			len := bd.ItemById[0].ParseInt;
			
			r := TMemoryReader.Create(data[1], len);
			k := TKonfig.Create;
			k.Load(r);
			
			Result := TKonfigReader.Create(k, D);
			Result.elem_fmt := fmt;
			Result.num_elements := Result.ReadU32('count');
			
			k.Free;
			r.Free;
		end else
		begin
			lr := TLuaReader.Create(arr.AsTable, D);
			lr.elem_fmt := fmt;
			lr.num_elements := arr.AsTable.ItemCount;
			if count <> nil then
				count^ := lr.num_elements;
				
			if Assigned(dest) then
			begin
				C := TIntegerValue.Create('count', 'u32', 0);
				D.items.Add(C);
				lr.count_ref := C;
			end;
			
			Result := lr;
		end;
	end else
		raise Exception.Create('cannot open section ''' + name + '''');
end;

function TLuaReader.ReadArrayWithNoKey(count : PLongword; const fmt : String) : IKonfigReader;
var
	lr : TLuaReader;
	C : TIntegerValue;
begin
	ReadHint('array with no key', 'array');
	if count <> nil then
		count^ := table.ItemCount;

	lr := TLuaReader.CreateArray(table, dest, table.ItemCount, fmt);
	
	if Assigned(dest) then
	begin
		C := TIntegerValue.Create('count', 'u32', 0);
		dest.items.Add(C);
		lr.count_ref := C;
	end;
	
	Result := lr;
end;

function TLuaReader.TryReadSection(const name : String) : IKonfigReader;
var
	sect : TLuaObject;
	D : TSection;
begin
	sect := table[name];
	if sect <> nil then
	begin
		if Assigned(dest) then
			D := dest.AddSect(name)
		else
			D := nil;
					
		Result := TLuaReader.Create(sect.AsTable, D)
	end else
		Result := nil;
end;

function TLuaReader.TryReadArray(const name : String; count : PLongword; const fmt : String) : IKonfigReader;
var
	arr : TLuaObject;
	D : TSection;
	C : TIntegerValue;
	
	data : String;
	len : Longint;
	
	bd : TLuaTable;
	r : TMemoryReader;
	k : TKonfig;
	
	lr : TLuaReader;
begin
	arr := table[name];
	if arr <> nil then
	begin
		ReadHint(name, 'array');
		
		if Assigned(dest) then
			D := dest.AddSect(name)
		else
			D := nil;
		
		if arr.AsTable['__binary_data__'] <> nil then
		begin
			bd := arr.AsTable['__binary_data__'].AsTable;
			
			data := DecodeStringBase64(bd.ItemById[1].AsTable.ItemById[0].AsString);
			len := bd.ItemById[0].ParseInt;
			
			r := TMemoryReader.Create(data[1], len);
			k := TKonfig.Create;
			k.Load(r);
			
			Result := TKonfigReader.Create(k, D);
			Result.elem_fmt := fmt;
			Result.num_elements := Result.ReadU32('count');
			
			k.Free;
			r.Free;
		end else
		begin
			lr := TLuaReader.Create(arr.AsTable, D);
			lr.elem_fmt := fmt;
			lr.num_elements := arr.AsTable.ItemCount;
			if count <> nil then
				count^ := lr.num_elements;
				
			if Assigned(dest) then
			begin
				C := TIntegerValue.Create('count', 'u32', 0);
				D.items.Add(C);
				lr.count_ref := C;
			end;
			
			Result := lr;
		end;
	end else
		Result := nil;
end;

function TLuaReader.ReadName(const nm : String) : String;
var
	n : String;
begin
	ReadHint(nm, 'name');
	n := ReadString(nm, 'stringz');
	if Assigned(dest) then dest.name := n;
	Result := n;
end;

function TLuaReader.ReadStringCrc(const nm : String; f : TStringRecoverFunc; userdata : Pointer) : String;
begin
	Result := ReadString(nm, 'stringz');
end;

procedure TLuaReader.ReadHint(const nm : String; const tp : String);
begin
	if Assigned(dest) then dest.AddHint(nm, tp);
end;

function TLuaReader.ReadHintStr(const nm : String; const tp : String) : String;
begin
	ReadHint(nm, tp);
	Result := ReadString(nm);
end;

function TLuaReader.ReadS8(const nm : String; const tp : String) : Shortint;
var
	I : Shortint;
begin
	I := table[nm].ParseInt;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TLuaReader.ReadS16(const nm : String; const tp : String) : Smallint;
var
	I : Smallint;
begin
	I := table[nm].ParseInt;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TLuaReader.ReadS32(const nm : String; const tp : String) : Longint;
var
	I : Longint;
begin
	I := table[nm].ParseInt;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TLuaReader.ReadS64(const nm : String; const tp : String) : Int64;
var
	I : Int64;
begin
	I := table[nm].ParseInt;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TLuaReader.ReadU8(const nm : String; const tp : String) : Byte;
var
	I : Byte;
	obj : TLuaObject;
begin
	obj := table[nm];
	
	if tp = 'fp32_q8' then
	begin
		I := Trunc(Min(obj.ParseFloat, 2.0) * 0.5 * 256)
	end else
	begin
		if obj.DataType = ltBoolean then
			I := Byte(obj.AsBoolean)
		else
			I := obj.ParseInt;
	end;
	
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TLuaReader.ReadU16(const nm : String; const tp : String) : Word;
var
	I : Word;
begin
	if tp = 'entity_link, uobject_link' then
		I := table[nm + '_entity_link_id'].ParseInt
	else if tp = 'cover_link, ucover_link' then
		I := table[nm + '_cover_link_id'].ParseInt
	else
		I := table[nm].ParseInt;
		
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TLuaReader.ReadU32(const nm : String; const tp : String) : Longword;
var
	I : Longword;
	t : TLuaTable;
begin
	if tp = 'color, u32' then
	begin
		t := table[nm].AsTable;
		I := t.GetItemById(3).ParseInt and $FF;
		I := (t.GetItemById(2).ParseInt and $FF) or (I shl 8);
		I := (t.GetItemById(1).ParseInt and $FF) or (I shl 8);
		I := (t.GetItemById(0).ParseInt and $FF) or (I shl 8);
	end else
	begin
		I := table[nm].ParseInt;
	end;
	
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TLuaReader.ReadU64(const nm : String; const tp : String) : QWord;
var
	I : QWord;
begin
	I := table[nm].ParseInt;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TLuaReader.ReadFP32(const nm : String; const tp : String) : Single;
var
	I : Single;
begin
	I := table[nm].ParseFloat;
	if tp = 'angle, fp32' then I := I * (PI/180.0);
	if Assigned(dest) then dest.AddFloat(nm, I, tp);
	Result := I;
end;

function TLuaReader.ReadString(const nm : String; const tp : String) : String;
var
	I : String;
begin
	I := table[nm].AsString;
	if Assigned(dest) then dest.AddStr(nm, I, tp);
	Result := I;
end;

function TLuaReader.ReadBool(const nm : String; const tp : String) : Boolean;
var
	I : Boolean;
begin
	I := table[nm].AsBoolean;
	if Assigned(dest) then dest.AddBool(nm, I, tp);
	Result := I;
end;

function TLuaReader.ReadVec2(const nm : String; const tp : String) : TVec2;
var
	v : TVec2;
	lv : TLuaTable;
begin
	lv := table[nm].AsTable;
	v.x := lv.ItemById[0].ParseFloat;
	v.y := lv.ItemById[1].ParseFloat;
	if tp = 'ang2f' then
	begin
		v.x := v.x * (PI/180.0);
		v.y := v.y * (PI/180.0);		
	end;
	if Assigned(dest) then dest.AddVec2(nm, v, tp);
	Result := v;
end;

function TLuaReader.ReadVec3(const nm : String; const tp : String) : TVec3;
var
	v : TVec3;
	lv : TLuaTable;
begin
	lv := table[nm].AsTable;
	v.x := lv.ItemById[0].ParseFloat;
	v.y := lv.ItemById[1].ParseFloat;
	v.z := lv.ItemById[2].ParseFloat;
	if tp = 'ang3f' then
	begin
		v.x := v.x * (PI/180.0);
		v.y := v.y * (PI/180.0);
		v.z := v.z * (PI/180.0);	
	end;
	if Assigned(dest) then dest.AddVec3(nm, v, tp);
	Result := v;
end;

function TLuaReader.ReadVec4(const nm : String; const tp : String) : TVec4;
var
	v : TVec4;
	lv : TLuaTable;
begin
	lv := table[nm].AsTable;
	v.x := lv.ItemById[0].ParseFloat;
	v.y := lv.ItemById[1].ParseFloat;
	v.z := lv.ItemById[2].ParseFloat;
	v.w := lv.ItemById[3].ParseFloat;
	if Assigned(dest) then dest.AddVec4(nm, v, tp);
	Result := v;
end;

function TLuaReader.ReadVec4S16(const nm : String; const tp : String) : TVec4S16;
var
	v : TVec4S16;
	lv : TLuaTable;
begin
	lv := table[nm].AsTable;
	v.x := lv.ItemById[0].ParseInt;
	v.y := lv.ItemById[1].ParseInt;
	v.z := lv.ItemById[2].ParseInt;
	v.w := lv.ItemById[3].ParseInt;
	if Assigned(dest) then dest.AddVec4S16(nm, v, tp);
	Result := v;
end;

procedure TLuaReader.ReadVec3i(const nm : String; const tp : String);
var
	v : TIntegerArrayValue;
	lv : TLuaTable;
begin
	if Assigned(dest) then
	begin
		v := TIntegerArrayValue.Create(nm, tp);
		SetLength(v.data, 3);
		lv := table[nm].AsTable;
		v.data[0] := lv.ItemById[0].ParseInt;
		v.data[1] := lv.ItemById[1].ParseInt;
		v.data[2] := lv.ItemById[2].ParseInt;
		dest.items.Add(v);
	end;
end;

procedure TLuaReader.ReadVec4i(const nm : String; const tp : String);
var
	v : TIntegerArrayValue;
	lv : TLuaTable;
begin
	if Assigned(dest) then
	begin
		v := TIntegerArrayValue.Create(nm, tp);
		SetLength(v.data, 4);
		lv := table[nm].AsTable;
		v.data[0] := lv.ItemById[0].ParseInt;
		v.data[1] := lv.ItemById[1].ParseInt;
		v.data[2] := lv.ItemById[2].ParseInt;
		v.data[3] := lv.ItemById[3].ParseInt;
		dest.items.Add(v);
	end;
end;

procedure TLuaReader.ReadMatrix44(const nm : String; const tp : String);
var
	mat : array[0..15] of Single;
	lv : TLuaTable;
	I : Longint;
	
	v : TFloatArrayValue;
begin
	if Assigned(dest) then
	begin
		if nm = '' then
			lv := table['pose'].AsTable
		else
			lv := table[nm].AsTable;
			
		for I := 0 to 15 do
			mat[I] := lv.ItemById[I].ParseFloat;
		
		v := TFloatArrayValue.Create(nm, tp);
		SetLength(v.data, 16);
		Move(mat, v.data[0], Sizeof(mat));
		dest.items.Add(v);
	end;
end;

procedure TLuaReader.ReadMatrix43(const nm : String; const tp : String);
var
	mat : array[0..11] of Single;
	lv : TLuaTable;
	I : Longint;
	
	v : TFloatArrayValue;
begin
	if Assigned(dest) then
	begin
		if nm = '' then
			lv := table['pose'].AsTable
		else
			lv := table[nm].AsTable;
			
		for I := 0 to 11 do
			mat[I] := lv.ItemById[I].ParseFloat;
		
		v := TFloatArrayValue.Create(nm, tp);
		SetLength(v.data, 12);
		Move(mat, v.data[0], Sizeof(mat));
		dest.items.Add(v);
	end;
end;

function TLuaReader.ReadU8Array(const nm : String; const tp : String) : TU8Array;
var
	arr : TLuaTable;
	count : Longint;
	str : String;
	data : array of Byte;
	v : TByteArrayValue;
	I : Longint;
begin
	arr := table[nm].AsTable;
	count := arr.GetItemById(0).ParseInt;
	if count > 0 then
	begin
		str := DecodeStringBase64(arr.GetItemById(1).AsTable.GetItemById(0).AsString);
		SetLength(str, count);
		
		SetLength(data, count);
		Move(str[1], data[0], count);
	end else
		SetLength(data, 0);
	
	if Assigned(dest) then
	begin
		v := TByteArrayValue.Create(nm, tp);
		SetLength(v.data, Length(data));
		for I := 0 to Length(v.data) - 1 do
			v.data[I] := data[I];
		dest.items.Add(v);
	end;

	Result := data;
end;

function TLuaReader.ReadU16Array(const nm : String; const tp : String) : TU16Array;
var
	arr : TLuaTable;
	count : Longint;
	v : TIntegerArrayValue;
begin
	arr := table[nm].AsTable;
	count := arr.GetItemById(0).ParseInt;
	if count > 0 then
	begin
		WriteLn('!!! TLuaReader.ReadU16Array not implemented!');
	end;
	
	if Assigned(dest) then
	begin
		v := TIntegerArrayValue.Create(nm, tp);
		dest.items.Add(v);
	end;

	Result := nil;
end;

function TLuaReader.ReadU32Array(const nm : String; const tp : String) : TU32Array;
var
	v : TIntegerArrayValue;
begin
	if Assigned(dest) then
	begin
		v := TIntegerArrayValue.Create(nm, tp);
		dest.items.Add(v);
	end;
	WriteLn('!!! TLuaReader.ReadU32Array not implemented!');
	Result := nil;
end;

function TLuaReader.ReadFP32Array(const nm : String; const tp : String) : TFP32Array;
var
	v : TFloatArrayValue;
begin
	if Assigned(dest) then
	begin
		v := TFloatArrayValue.Create(nm, tp);
		dest.items.Add(v);
	end;
	WriteLn('!!! TLuaReader.ReadFP32Array not implemented!');
	Result := nil;
end;

function TLuaReader.ReadU16Array16(const nm : String; const tp : String) : TU16Array;
var
	arr : TLuaTable;
	count : Longint;
	v : TIntegerArrayValue;
begin
	arr := table[nm].AsTable;
	count := arr.GetItemById(0).ParseInt;
	if count > 0 then
	begin
		WriteLn('!!! TLuaReader.ReadU16Array16 not implemented!');
	end;
	
	if Assigned(dest) then
	begin
		v := TIntegerArrayValue.Create(nm, tp);
		dest.items.Add(v);
	end;

	Result := nil;
end;

function TLuaReader.ReadU32Array16(const nm : String; const tp : String) : TU32Array;
var
	v : TIntegerArrayValue;
begin
	if Assigned(dest) then
	begin
		v := TIntegerArrayValue.Create(nm, tp);
		dest.items.Add(v);
	end;
	WriteLn('!!! TLuaReader.ReadU32Array16 not implemented!');
	Result := nil;
end;

function TLuaReader.ReadFP32Array16(const nm : String; const tp : String) : TFP32Array;
var
	v : TFloatArrayValue;
begin
	if Assigned(dest) then
	begin
		v := TFloatArrayValue.Create(nm, tp);
		dest.items.Add(v);
	end;
	WriteLn('!!! TLuaReader.ReadFP32Array16 not implemented!');
	Result := nil;
end;

function TLuaReader.ReadStrArray16(const nm : String; const tp : String) : TStrArray;
var
	ret : TStrArray;
	arr : String;
	I,J,len : Longint;
begin
	ReadHint(nm, tp);
		
	arr := table[nm].AsString;
	if arr <> '' then
	begin
		len := 1;
		for I := 1 to Length(arr) do
			if arr[I] = ',' then
				Inc(len);
	end else
		len := 0;
	
	SetLength(ret, len);
	for I := 0 to len-1 do
	begin
		J := Pos(',', arr);
		if J > 0 then
		begin
			ret[I] := Copy(arr, 1, J-1);
			arr := Copy(arr, J+1);
		end else
			ret[I] := arr;
	end;
	
	if Assigned(dest) then
	begin
		dest.AddInt(nm, len, 'u16');
		for I := 0 to len-1 do
			dest.AddStr(nm, ret[I]);
	end;
	
	Result := ret;
end;

function TLuaReader.ReadStrArray32(const nm : String; const tp : String) : TStrArray;
var
	ret : TStrArray;
	arr : String;
	I,J,len : Longint;
begin
	ReadHint(nm, tp);
		
	arr := table[nm].AsString;
	if arr <> '' then
	begin
		len := 1;
		for I := 1 to Length(arr) do
			if arr[I] = ',' then
				Inc(len);
	end else
		len := 0;
	
	SetLength(ret, len);
	for I := 0 to len-1 do
	begin
		J := Pos(',', arr);
		if J > 0 then
		begin
			ret[I] := Copy(arr, 1, J-1);
			arr := Copy(arr, J+1);
		end else
			ret[I] := arr;
	end;
	
	if Assigned(dest) then
	begin
		dest.AddInt(nm, len, 'u32');
		for I := 0 to len-1 do
			dest.AddStr(nm, ret[I]);
	end;
	
	Result := ret;
end;

function TLuaReader.ReadBool8(const nm : String; names : array of String; tp : String) : Byte;
var
	ret : Byte;
	I : Longint;
	obj : TLuaObject;
begin
	ret := 0;
	for I := 0 to Min(7, Length(names)-1) do
	begin
		obj := table[names[I]];
		if obj = nil then
			WriteLn('TLuaReader.ReadBool8: Warning! ''', names[I], ''' is not found, reset to false')
		else
			ret := ret or (Byte(obj.AsBoolean) shl I);
	end;
	if Assigned(dest) then dest.AddInt(nm, ret, tp);	
	Result := ret;
end;

function TLuaReader.ReadBool16(const nm : String; names : array of String; masks : array of Word; tp : String) : Word;
var
	ret : Word;
	I : Longint;
begin
	ret := 0;
	for I := 0 to Min(15, Length(names)-1) do
		if table[names[I]].AsBoolean then
			ret := ret or masks[I];
	if Assigned(dest) then dest.AddInt(nm, ret, tp);
	Result := ret;
end;

function TLuaReader.ReadBool32(const nm : String; names : array of String; masks : array of Longword; tp : String) : Longword;
var
	ret : Longword;
	I : Longint;
begin
	ret := 0;
	for I := 0 to Min(15, Length(names)-1) do
		if table[names[I]].AsBoolean then
			ret := ret or masks[I];
	if Assigned(dest) then dest.AddInt(nm, ret, tp);
	Result := ret;
end;

procedure TLuaReader.ReadIdentifier(const nm : String);
var
	I : String;
begin
	I := table[nm].AsString;
	if Assigned(dest) then dest.AddInt(nm, GetStringCrc(I), 'u32');
end;

procedure TLuaReader.ReadIdentifierArray(const nm : String; const tp : String);
var
	ret : TStrArray;
	arr : String;
	I,J,len : Longint;
	iarr : TIntegerArrayValue;
begin
	arr := table[nm].AsString;
	if arr <> '' then
	begin
		len := 1;
		for I := 1 to Length(arr) do
			if arr[I] = ',' then
				Inc(len);
	end else
		len := 0;
	
	SetLength(ret, len);
	for I := 0 to len-1 do
	begin
		J := Pos(',', arr);
		if J > 0 then
		begin
			ret[I] := Copy(arr, 1, J-1);
			arr := Copy(arr, J+1);
		end else
			ret[I] := arr;
	end;
	
	if Assigned(dest) then
	begin
		iarr := TIntegerArrayValue.Create(nm, tp);
		SetLength(iarr.data, Length(ret));
		for I := 0 to Length(iarr.data)-1 do
			iarr.data[I] := GetStringCrc(ret[I]);
		dest.items.Add(iarr);
	end;
end;

end.