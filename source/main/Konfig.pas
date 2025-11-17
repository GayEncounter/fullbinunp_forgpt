unit Konfig;

interface
uses classes, sysutils, chunkedFile, vmath, fgl;

type
	TItemList = TFPList;

type
	EParamNotFound = class(Exception)
		public
		param_name, param_type : String;
		constructor Create(const p_name, p_type : String);
	end;

	TSimpleValue = class
		//name, vtype : String;
		nametype : Longint;
		constructor Create(const nm, tp : String); overload;
		constructor CreateCopy(nametype : Longint);

		function Copy : TSimpleValue; virtual;
		
		function ReadName : String;
		procedure WriteName(const n : String);
		function ReadVType : String;
		procedure WriteVType(const t : String);
		
		property Name : String read ReadName write WriteName;
		property VType : String read ReadVType write WriteVType;
	end;
	TSection = class(TSimpleValue)
		items: TItemList;
		constructor Create; overload;
		constructor Create(const nm : String); overload;
		constructor Create(const nm, tp : String); overload;
		constructor CreateCopy(nametype : Longint);
		destructor Destroy; override;

		function Copy : TSimpleValue; override;
		procedure Clear;
		
		function ParamCount : Longint;
		function GetParam(index : Longint) : TSimpleValue; overload;
		function GetParam(const nm, tp : String) : TSimpleValue; overload;
		
		procedure AddHint(const nm, tp : String);
		procedure AddFloat(const nm : String; f : Single; const tp : String = 'fp32');
		procedure AddStr(const nm, val : String; const tp : String = 'stringz');
		procedure AddVec2(const nm : String; const v : TVec2; const tp : String = 'vec2f');
		procedure AddVec3(const nm : String; const v : TVec3; const tp : String = 'vec3f');
		procedure AddVec4(const nm : String; const v : TVec4; const tp : String = 'vec4f');
		procedure AddVec4S16(const nm : String; const v : TVec4S16; const tp : String = 'vec4s16');
		procedure AddBool(const nm : String; b : Boolean; const tp : String = 'bool');
		procedure AddInt(const nm : String; val : Int64; const tp : String);
		function  AddSect(const nm : String) : TSection;
		
		function GetFloat(const nm : String; const tp : String = 'fp32') : Single; overload;
		function GetFloat(const nm : String; def : Single; const tp : String = 'fp32') : Single; overload;
		function GetStr(const nm : String; const tp : String = 'stringz') : String; overload;
		function GetStrDef(const nm : String; const def : String; const tp : String = 'stringz') : String; overload;
		function GetVec2(const nm : String; const tp : String = 'vec2f') : TVec2; overload;
		function GetVec2(const nm : String; const def : TVec2; const tp : String = 'vec2f') : TVec2; overload;
		function GetVec3(const nm : String; const tp : String = 'vec3f') : TVec3; overload;
		function GetVec3(const nm : String; const def : TVec3; const tp : String = 'vec3f') : TVec3; overload;
		function GetVec4(const nm : String; const tp : String = 'vec4f') : TVec4; overload;
		function GetVec4(const nm : String; const def : TVec4; const tp : String = 'vec4f') : TVec4; overload;		
		function GetBool(const nm : String; const tp : String = 'bool') : Boolean; overload;
		function GetBool(const nm : String; def : Boolean; const tp : String = 'bool') : Boolean; overload;
		function GetInt(const nm : String; const tp : String) : Int64; overload;
		function GetInt(const nm : String; def : Int64; const tp : String) : Int64; overload;
		function GetSect(const nm : String; raiseiffail : Boolean = True) : TSection;
		
		procedure Replace(old, new : TSimpleValue);
	end;
	TIntegerValue = class(TSimpleValue)
		num : Int64;
		constructor Create(const nm, tp : String; i : Int64); overload;
		constructor CreateCopy(nametype : Longint; i : Int64);
		function Copy : TSimpleValue; override;
	end;
	TSingleValue = class(TSimpleValue)
		num : Single;
		constructor Create(const nm, tp : String; f : Single); overload;
		constructor CreateCopy(nametype : Longint; f : Single);
		function Copy : TSimpleValue; override;
	end;
	TStringValue = class(TSimpleValue)
		str : String;
		constructor Create(const nm, val : String); overload;
		constructor CreateCopy(nametype : Longint; val : String);
		function Copy : TSimpleValue; override;
	end;
	TByteArrayValue = class(TSimpleValue)
		data : array of Byte;
		function Copy : TSimpleValue; override;
		procedure FromStrings(const strs : array of String);
		procedure FromReader(r : TMemoryReader; count : Integer);
	end;
	TIntegerArrayValue = class(TSimpleValue)
		data : array of Integer;
		function Copy : TSimpleValue; override;
		procedure FromStrings(const strs : array of String);
		procedure FromReaderS16(r : TMemoryReader; count : Integer);
		procedure FromReaderS32(r : TMemoryReader; count : Integer);
	end;
	TFloatArrayValue = class(TSimpleValue)
		data : array of Single;
		function Copy : TSimpleValue; override;
		procedure FromStrings(const strs : array of String);
		procedure FromReader(r : TMemoryReader; count : Integer);
		
		procedure GetMatrix44(out m : TMatrix);
		procedure GetMatrix43(out m : TMatrix);
		procedure GetMatrix43T(out m : TMatrix); // transpose
		
		procedure SetMatrix44(const m : TMatrix);
		procedure SetMatrix43(const m : TMatrix);
		procedure SetMatrix43T(const m : TMatrix); // transpose
	end;
	TBoolValue = class(TSimpleValue)
		bool : Boolean;
		function Copy : TSimpleValue; override;
		constructor Create(const nm, tp : String; val : Boolean);
		constructor CreateCopy(nametype : Longint; val : Boolean);
	end;

	TTextKonfig = class
		root : TSection;

		constructor Create; overload;
		constructor Create(root : TSection); overload;
		destructor Destroy; override;
		
		function Copy : TTextKonfig;
		
		procedure Load(const str : String);
		procedure Save(var str : String);
		procedure LoadFromFile(const filename : String);
		procedure SaveToFile(const filename : String);
		
		procedure LoadJSON(const str : String);
		procedure LoadJSONFromFile(const filename : String);
		
		procedure SaveJSON(var str : String);
		procedure SaveJSONToFile(const filename : String);
	end;

const
	konfDebugInfo       = $1;
	konfEditor          = $2;  // what does it mean ??
	konfDiktionary      = $4;
	konfPlain           = $8;  // also unknown
	konfNoSections      = $10;
	konfMultiChunk      = $20;

type
	TKonfig = class
	public
		kind : Byte; // flags (see above)
		data : array of Byte;
		dictionary : array of String;
		dictionary_map : TFPGMap<String,Longint>;

	public
		destructor Destroy; override;
	
		function Load(stream : TStream) : Boolean; overload;
		procedure Save(stream : TStream); overload;

		function Load(r : TMemoryReader) : Boolean; overload;
		procedure Save(w : TMemoryWriter); overload;

		function Load(const filename : String) : Boolean; overload;
		procedure Save(const filename : String); overload;

		procedure Compile(from : TTextKonfig; ll : Boolean = False);
		procedure Decompile(tk : TTextKonfig; ll : Boolean = False);
		procedure DecompileIntoSection(s : TSection; ll : Boolean = False);
	private
		function AddWord(const w : String) : Longint;
		procedure CompileSection(sect : TSection; w : TMemoryWriter; ll : Boolean);
		procedure CompileConfig(items : TItemList; w : TMemoryWriter; ll : Boolean);
		function DecompileSection(r : TMemoryReader; ll : Boolean) : TSection;
		procedure DecompileConfig(items : TItemList; kind : Byte; r : TMemoryReader; ll : Boolean);
	end;

implementation
uses uCrc, uHashTable, Parser, EscapedStrings;

var
	common_types_table : XHashTable;
	common_names_table : XHashTable;

function _IsHint(const vtype : String) : Boolean;
begin
	_IsHint := 
	(vtype = 'array') or
	(vtype = 'name') or
	(vtype = 'time') or
	(vtype = 'choose') or
	(vtype = 'camera_track, str_shared') or
	(vtype = 'texture, str_shared') or
	(vtype = 'choose_array, str_shared') or
	(vtype = 'particles_modifier, str_shared') or
	(vtype = 'particles_effect, str_shared') or
	(vtype = 'particles, str_shared') or
	(vtype = 'vs_ref, str_shared') or
	(vtype = 'material, str_shared') or
	(vtype = 'shader, str_shared') or
	(vtype = 'locator_id') or
	(vtype = 'locator_str') or
	(vtype = 'bone_id') or
	(vtype = 'bone_str') or
	(vtype = 'part_id') or
	(vtype = 'part_str') or
	(vtype = 'attp_str') or
	(vtype = 'animation_str') or
	(vtype = 'param_str') or
	(vtype = 'ref_coloranim') or
	(vtype = 'ref_model') or
	(vtype = 'sound') or
	(vtype = 'str_array') or
	(vtype = 'str_array16') or
	(vtype = 'str_array32') or
	(vtype = 'flags8') or
	(vtype = 'flags16') or
	(vtype = 'flags32') or
	(vtype = 'flags64') or
	(vtype = 'tex_frame, u16*, u32') or
	(vtype = 'tex_frame, u_vector<u16>')
end;

type
	TStringArray = array of String;

// Exception implementation
constructor EParamNotFound.Create(const p_name, p_type : String);
begin
	inherited Create('Parameter not found (' + p_name + ': ' + p_type + ')');
	param_name := p_name;
	param_type := p_type;
end;

// utilities
function ParseFloat(src : String) : Single;
var I : Longint;
begin
	I := Pos('.', src);
	if I > 0 then
		src[I] := DecimalSeparator;
		
	Result := StrToFloat(src);
end;

function PrintFloat(f : Single) : String;
var I : Longint;
begin
	Result := FloatToStr(f);
	I := Pos(DecimalSeparator, Result);
	if I > 0 then
		Result[I] := '.';
end;

// value implementation
constructor TSimpleValue.Create(const nm, tp: String);
begin
	name := nm;
	vtype := tp;
end;

constructor TSimpleValue.CreateCopy(nametype : Longint);
begin
	self.nametype := nametype;
end;

function TSimpleValue.Copy : TSimpleValue;
begin
	Result := TSimpleValue.CreateCopy(nametype);
end;

function TSimpleValue.ReadName : String;
var
	index : Integer;
begin
	index := (nametype and $00FFFFFF);
	ReadName := common_names_table.Get(index);
end;

procedure TSimpleValue.WriteName(const n : String);
var
	idx : Integer;
begin
	idx := common_names_table.Add(n);
	nametype := (nametype and $FF000000) or idx;
end;

function TSimpleValue.ReadVType : String;
var
	index : Integer;
begin
	index := (nametype and $FF000000) shr 24;
	ReadVType := common_types_table.Get(index);
end;

procedure TSimpleValue.WriteVType(const t : String);
var
	idx : Integer;
begin
	idx := common_types_table.Add(t);
	nametype := (nametype and $00FFFFFF) or (idx shl 24);
end;

constructor TSection.Create;
begin
	inherited Create('unnamed_section', 'section');
	items := TItemList.Create;
end;

constructor TSection.Create(const nm: String);
begin
	inherited Create(nm, 'section');
	items := TItemList.Create;
end;

constructor TSection.Create(const nm, tp: String);
begin
	inherited Create(nm, tp);
	items := TItemList.Create;
end;

constructor TSection.CreateCopy(nametype : Longint);
begin
	inherited CreateCopy(nametype);
	items := TItemList.Create;
end;

destructor TSection.Destroy;
begin
	Clear;
	items.Free;
	inherited;
end;

function TSection.Copy : TSimpleValue;
var
	sect : TSection;
	I : Integer;
begin
	sect := TSection.CreateCopy(nametype);
	sect.items.Count := items.Count;

	for I := 0 to items.Count - 1 do
		sect.items[I] := TSimpleValue(items[I]).Copy;

	Result := sect;
end;

procedure TSection.Clear;
var
	I : Integer;
begin
	for I := 0 to items.Count - 1 do
		TSimpleValue(items[I]).Free;
	items.Clear;
end;

function TSection.ParamCount : Longint;
begin
	Result := items.Count;
end;

function TSection.GetParam(index : Longint) : TSimpleValue; overload;
begin
	Result := TSimpleValue(items[index]);
end;

function TSection.GetParam(const nm: string; const tp: string) : TSimpleValue; overload;
var
	item : TSimpleValue;
	I : Integer;
begin
	for I := 0 to items.Count - 1 do
	begin
		item := TSimpleValue(items[I]);
		if (item.name = nm) and (item.vtype = tp) then
		begin
			Result := item;
			Exit;
		end;
	end;

	Result := nil;
end;

procedure TSection.AddHint(const nm: string; const tp: string);
begin
	items.Add(TSimpleValue.Create(nm, tp));
end;

procedure TSection.AddFloat(const nm: string; f: Single; const tp: string);
begin
	items.Add(TSingleValue.Create(nm, tp, f));
end;

procedure TSection.AddStr(const nm: string; const val: string; const tp: string);
var
	s : TStringValue;
begin
	s := TStringValue.Create(nm, val);
	s.vtype := tp;
	items.Add(s);
end;

procedure TSection.AddVec2(const nm: string; const v: TVec2; const tp: string);
var
	a : TFloatArrayValue;
begin
	a := TFloatArrayValue.Create(nm, tp);
	SetLength(a.data, 2);
	a.data[0] := v.x;
	a.data[1] := v.y;
	items.Add(a);
end;

procedure TSection.AddVec3(const nm: string; const v: TVec3; const tp: string);
var
	a : TFloatArrayValue;
begin
	a := TFloatArrayValue.Create(nm, tp);
	SetLength(a.data, 3);
	a.data[0] := v.x;
	a.data[1] := v.y;
	a.data[2] := v.z;
	items.Add(a);
end;

procedure TSection.AddVec4(const nm: string; const v: TVec4; const tp: string);
var
	a : TFloatArrayValue;
begin
	a := TFloatArrayValue.Create(nm, tp);
	SetLength(a.data, 4);
	a.data[0] := v.x;
	a.data[1] := v.y;
	a.data[2] := v.z;
	a.data[3] := v.w;
	items.Add(a);
end;

procedure TSection.AddVec4S16(const nm: string; const v: TVec4S16; const tp: string);
var
	a : TIntegerArrayValue;
begin
	a := TIntegerArrayValue.Create(nm, tp);
	SetLength(a.data, 4);
	a.data[0] := v.x;
	a.data[1] := v.y;
	a.data[2] := v.z;
	a.data[3] := v.w;
	items.Add(a);
end;

procedure TSection.AddBool(const nm: string; b: Boolean; const tp: string);
begin
	items.Add(TBoolValue.Create(nm, tp, b));
end;

procedure TSection.AddInt(const nm: String; val : Int64; const tp : String);
begin
	items.Add(TIntegerValue.Create(nm, tp, val));
end;

function TSection.AddSect(const nm : String) : TSection;
var
	s : TSection;
begin
	s := TSection.Create(nm);
	items.Add(s);

	Result := s;
end;

function TSection.GetFloat(const nm : String; const tp : String) : Single;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TSingleValue).num
	else raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetFloat(const nm : String; def : Single; const tp : String) : Single;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TSingleValue).num
	else Result := def;
end;

function TSection.GetStr(const nm, tp : String) : String;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TStringValue).str
	else raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetStrDef(const nm : String; const def : String; const tp : String) : String;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TStringValue).str
	else Result := def;
end;

function TSection.GetVec2(const nm, tp : String) : TVec2;
var
	v : TSimpleValue;
	f : TFloatArrayValue;
	r : TVec2;
begin
	v := GetParam(nm, tp);
	if v <> nil then
	begin 
		f := v as TFloatArrayValue;
		r.x := f.data[0];
		r.y := f.data[1];
		Result := r
	end else 
		raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetVec2(const nm : String; const def : TVec2; const tp : String) : TVec2;
var
	v : TSimpleValue;
	f : TFloatArrayValue;
	r : TVec2;
begin
	v := GetParam(nm, tp);
	if v <> nil then
	begin 
		f := v as TFloatArrayValue;
		r.x := f.data[0];
		r.y := f.data[1];
		Result := r
	end else 
		Result := def;
end;

function TSection.GetVec3(const nm, tp : String) : TVec3;
var
	v : TSimpleValue;
	f : TFloatArrayValue;
	r : TVec3;
begin
	v := GetParam(nm, tp);
	if v <> nil then
	begin 
		f := v as TFloatArrayValue;
		r.x := f.data[0];
		r.y := f.data[1];
		r.z := f.data[2];
		Result := r
	end else 
		raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetVec3(const nm : String; const def : TVec3; const tp : String) : TVec3;
var
	v : TSimpleValue;
	f : TFloatArrayValue;
	r : TVec3;
begin
	v := GetParam(nm, tp);
	if v <> nil then
	begin 
		f := v as TFloatArrayValue;
		r.x := f.data[0];
		r.y := f.data[1];
		r.z := f.data[2];
		Result := r
	end else 
		Result := def;
end;

function TSection.GetVec4(const nm, tp : String) : TVec4;
var
	v : TSimpleValue;
	f : TFloatArrayValue;
	r : TVec4;
begin
	v := GetParam(nm, tp);
	if v <> nil then
	begin 
		f := v as TFloatArrayValue;
		r.x := f.data[0];
		r.y := f.data[1];
		r.z := f.data[2];
		r.w := f.data[3];
		Result := r
	end else 
		raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetVec4(const nm : String; const def : TVec4; const tp : String) : TVec4;
var
	v : TSimpleValue;
	f : TFloatArrayValue;
	r : TVec4;
begin
	v := GetParam(nm, tp);
	if v <> nil then
	begin 
		f := v as TFloatArrayValue;
		r.x := f.data[0];
		r.y := f.data[1];
		r.z := f.data[2];
		r.w := f.data[3];
		Result := r
	end else 
		Result := def;
end;

function TSection.GetBool(const nm, tp : String) : Boolean;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TBoolValue).bool
	else raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetBool(const nm : String; def : Boolean; const tp : String) : Boolean;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TBoolValue).bool
	else Result := def;
end;

function TSection.GetInt(const nm, tp : String) : Int64;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TIntegerValue).num
	else raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetInt(const nm : String; def : Int64; const tp : String) : Int64;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TIntegerValue).num
	else Result := def;
end;

function TSection.GetSect(const nm : String; raiseiffail : Boolean) : TSection;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, 'section');
	if v <> nil then Result := v as TSection
	else Result := nil;
	
	if raiseiffail and (Result = nil) then
		raise EParamNotFound.Create(nm, 'section');
end;

procedure TSection.Replace(old, new : TSimpleValue);
var
	idx : Longint;
begin
	idx := items.IndexOf(old);
	if idx <> -1 then
	begin
		TSimpleValue(items[idx]).Free;
		items[idx] := new;
	end;
end;

constructor TIntegerValue.Create(const nm, tp: String; i: Int64);
begin
	inherited Create(nm, tp);
	num := i;
end;

constructor TIntegerValue.CreateCopy(nametype : Longint; i: Int64);
begin
	inherited CreateCopy(nametype);
	num := i;
end;

function TIntegerValue.Copy : TSimpleValue;
begin
	Result := TIntegerValue.CreateCopy(nametype, num);
end;

constructor TSingleValue.Create(const nm, tp: String; f: Single);
begin
	inherited Create(nm, tp);
	num := f;
end;

constructor TSingleValue.CreateCopy(nametype : Longint; f: Single);
begin
	inherited CreateCopy(nametype);
	num := f;
end;

function TSingleValue.Copy : TSimpleValue;
begin
	Result := TSingleValue.CreateCopy(nametype, num);
end;

constructor TStringValue.Create(const nm, val: String);
begin
	inherited Create(nm, 'stringz');
	str := val;
end;

constructor TStringValue.CreateCopy(nametype : Longint; val: String);
begin
	inherited CreateCopy(nametype);
	str := val;
end;

function TStringValue.Copy : TSimpleValue;
begin
	Result := TStringValue.CreateCopy(nametype, str);
end;

function TByteArrayValue.Copy : TSimpleValue;
var
	arr : TByteArrayValue;
begin
	arr := TByteArrayValue.CreateCopy(nametype);
	SetLength(arr.data, Length(data));
	Move(data[0], arr.data[0], Length(data)*Sizeof(Byte));

	Result := arr;
end;

procedure TByteArrayValue.FromStrings(const strs: array of string);
var I : Integer;
begin
	SetLength(data, Length(strs));
	for I := 0 to Length(data) - 1 do
		data[I] := StrToInt(strs[I]);
end;

procedure TByteArrayValue.FromReader(r: TMemoryReader; count: Integer);
var I : Integer;
begin
	SetLength(data, count);
	for I := 0 to count - 1 do
		data[I] := r.ReadByte;
end;

function TIntegerArrayValue.Copy : TSimpleValue;
var
	arr : TIntegerArrayValue;
begin
	arr := TIntegerArrayValue.CreateCopy(nametype);
	SetLength(arr.data, Length(data));
	Move(data[0], arr.data[0], Length(data)*Sizeof(Integer));

	Result := arr;
end;

procedure TIntegerArrayValue.FromStrings(const strs: array of string);
var I : Integer;
begin
	SetLength(data, Length(strs));
	for I := 0 to Length(data) - 1 do
		data[I] := StrToInt(strs[I]);
end;

procedure TIntegerArrayValue.FromReaderS16(r: TMemoryReader; count: Integer);
var I : Integer;
begin
	SetLength(data, count);
	for I := 0 to count - 1 do
		data[I] := r.ReadSmallint;
end;

procedure TIntegerArrayValue.FromReaderS32(r: TMemoryReader; count: Integer);
var I : Integer;
begin
	SetLength(data, count);
	for I := 0 to count - 1 do
		data[I] := r.ReadLongint;
end;

function TFloatArrayValue.Copy : TSimpleValue;
var
	arr : TFloatArrayValue;
begin
	arr := TFloatArrayValue.CreateCopy(nametype);
	SetLength(arr.data, Length(data));
	Move(data[0], arr.data[0], Length(data)*Sizeof(Single));

	Result := arr;
end;

procedure TFloatArrayValue.FromStrings(const strs: array of string);
var I : Integer;
begin
	SetLength(data, Length(strs));
	for I := 0 to Length(data) - 1 do
		data[I] := ParseFloat(strs[I]);
end;

procedure TFloatArrayValue.FromReader(r: TMemoryReader; count: Integer);
var I : Integer;
begin
	SetLength(data, count);
	for I := 0 to count - 1 do
		data[I] := r.ReadSingle;
end;

procedure TFloatArrayValue.GetMatrix44(out m : TMatrix);
begin
	if Length(data) <> 16 then
		raise Exception.Create('4x4 matrix must contain 16 elements');
		
	Move(data[0], m, Sizeof(TMatrix));
end;

procedure TFloatArrayValue.GetMatrix43(out m : TMatrix);
begin
	if Length(data) <> 12 then
		raise Exception.Create('4x3 matrix must contain 12 elements');
		
	m[1,1] := data[0]; m[1,2] := data[1]; m[1,3] := data[2]; m[1,4] := 0.0;
	m[2,1] := data[3]; m[2,2] := data[4]; m[2,3] := data[5]; m[2,4] := 0.0;
	m[3,1] := data[6]; m[3,2] := data[7]; m[3,3] := data[8]; m[3,4] := 0.0;
	m[4,1] := data[9]; m[4,2] := data[10]; m[4,3] := data[11]; m[4,4] := 1.0;
end;

procedure TFloatArrayValue.GetMatrix43T(out m : TMatrix);
begin
	if Length(data) <> 12 then
		raise Exception.Create('4x3 matrix must contain 12 elements');
		
	m[1,1] := data[0];
	m[1,2] := data[4];
	m[1,3] := data[8];
	m[1,4] := 0.0;

	m[2,1] := data[1];
	m[2,2] := data[5];
	m[2,3] := data[9];
	m[2,4] := 0.0;

	m[3,1] := data[2];
	m[3,2] := data[6];
	m[3,3] := data[10];
	m[3,4] := 0.0;

	m[4,1] := data[3];
	m[4,2] := data[7];
	m[4,3] := data[11];
	m[4,4] := 1.0;
end;

procedure TFloatArrayValue.SetMatrix44(const m : TMatrix);
begin
	SetLength(data, 16);
	Move(m, data[0], Sizeof(TMatrix));
end;

procedure TFloatArrayValue.SetMatrix43(const m : TMatrix);
begin
	SetLength(data, 12);
	data[0] := m[1,1]; data[1] := m[1,2]; data[2] := m[1,3];
	data[3] := m[2,1]; data[4] := m[2,2]; data[5] := m[2,3];
	data[6] := m[3,1]; data[7] := m[3,2]; data[8] := m[3,3];
	data[9] := m[4,1]; data[10] := m[4,2]; data[11] := m[4,3];	
end;

procedure TFloatArrayValue.SetMatrix43T(const m : TMatrix);
begin
	SetLength(data, 12);
	data[0] := m[1,1];
	data[4] := m[1,2];
	data[8] := m[1,3];

	data[1] := m[2,1];
	data[5] := m[2,2];
	data[9] := m[2,3];

	data[2] := m[3,1];
	data[6] := m[3,2];
	data[10] := m[3,3];

	data[3] := m[4,1];
	data[7] := m[4,2];
	data[11] := m[4,3];
end;

constructor TBoolValue.Create(const nm, tp: String; val: Boolean);
begin
	inherited Create(nm, tp);
	bool := val;
end;

constructor TBoolValue.CreateCopy(nametype : Longint; val: Boolean);
begin
	inherited CreateCopy(nametype);
	bool := val;
end;

function TBoolValue.Copy : TSimpleValue;
begin
	Result := TBoolValue.CreateCopy(nametype, bool);
end;

// TKonfig
destructor TKonfig.Destroy;
begin
	dictionary_map.Free;
	inherited;
end;

function TKonfig.Load(stream : TStream) : Boolean;
var
	r : TMemoryReader;
begin
	r := TMemoryReader.Create(stream);
	Result := Load(r);
	r.Free;
end;

procedure TKonfig.Save(stream : TStream);
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	Save(w);
	stream.Write(w.data[0], w.size);
	w.Free;
end;

function TKonfig.Load(r : TMemoryReader) : Boolean;
var
	rdata, rdict : TMemoryReader;
	count, I : Longint;
begin
	Load := False;

	kind := r.ReadByte;
	if (kind and konfDiktionary) <> 0 then
	begin
		rdata := r.OpenChunk(1, True);
		if rdata <> nil then
		begin
			SetLength(data, rdata.size);
			rdata.Read(data[0], rdata.size);

			rdict := r.OpenChunk(2, True);
			if rdict <> nil then
			begin
				count := rdict.ReadLongint;
				SetLength(dictionary, count);

				for I := 0 to count-1 do
					dictionary[I] := rdict.ReadStringZ;

				Load := True;
			end;
			rdict.Free;
		end;
		rdata.Free;
	end else
	begin
		SetLength(data, r.size-r.pos);
		r.Read(data[0], r.size-r.pos);
		Load := True;
	end;
end;

procedure TKonfig.Save(w : TMemoryWriter);
var
	I : Integer;
begin
	w.WriteByte(kind);
	if (kind and konfDiktionary) <> 0 then
	begin
		w.OpenChunk(1);
		w.Write(data[0], Length(data));
		w.CloseChunk;

		w.OpenChunk(2);
		w.WriteLongint(Length(dictionary));
		for I := 0 to Length(dictionary) - 1 do
			w.WriteStringZ(dictionary[I]);
		w.CloseChunk;
	end else
		w.Write(data[0], Length(data));
end;

function TKonfig.Load(const filename : String) : Boolean;
var
	r : TMemoryReader;
begin
	r := TMemoryReader.CreateFromFile(filename);
	Result := Load(r);
	r.Free;
end;

procedure TKonfig.Save(const filename : String);
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	Save(w);
	w.SaveTo(filename);
end;

procedure TKonfig.Compile(from: TTextKonfig; ll : Boolean);
var
	w : TMemoryWriter;
begin
	SetLength(dictionary, 0); // cleanup

	w := TMemoryWriter.Create;
	CompileConfig(from.root.items, w, ll);
	if (kind and konfMultiChunk) = 0 then
	begin
		SetLength(data, w.size);
		Move(w.data[0], data[0], w.size);
	end else
	begin
		SetLength(data, w.size+8);
		PLongint(@data[0])^ := GetStringCrc('arch_chunk_0');
		PLongint(@data[4])^ := w.size;
		Move(w.data[0], data[8], w.size);
	end;
	w.Free;
end;

procedure TKonfig.Decompile(tk: TTextKonfig; ll : Boolean);
begin
	DecompileIntoSection(tk.root, ll);
end;

procedure TKonfig.DecompileIntoSection(s : TSection; ll : Boolean);
var
	r : TMemoryReader;
begin
	if (kind = 3) or (kind = 5) then
	begin
		r := TMemoryReader.Create(data[0], Length(data));
		DecompileConfig(s.items, kind, r, ll);
		r.Free;
	end else
		raise Exception.Create('Cannot decompile config with kind='+IntToStr(kind));
end;

function TKonfig.AddWord(const w : String) : Longint;
var
	I : Integer;
begin
	if dictionary_map = nil then
	begin
		dictionary_map := TFPGMap<String,Longint>.Create;
		dictionary_map.Sorted := True;
	end;
	
	if dictionary_map.Find(w, I) then
		Result := dictionary_map.Data[I]
	else
	begin
		I := Length(dictionary);
		SetLength(dictionary, I+1);
		dictionary[I] := w;
		dictionary_map.Add(w, I);
		Result := I;
	end;
{
	Result := -1;
	for I := 0 to Length(dictionary) - 1 do
		if dictionary[I] = w then
		begin
			Result := I;
			Break;
		end;

	if Result = -1 then
	begin
		I := Length(dictionary);
		SetLength(dictionary, I+1);
		dictionary[I] := w;
		Result := I;
	end;
}
end;

procedure TKonfig.CompileSection(sect: TSection; w: TMemoryWriter; ll : Boolean);
begin
	if (kind and konfNoSections) = 0 then
	begin
		w.OpenChunk(GetStringCrc(sect.name));
		
		if (kind and konfMultiChunk) = 0 then
			w.WriteByte(kind);
			
		if (kind and konfDebugInfo) <> 0 then
		begin
			if (kind and konfDiktionary) <> 0 then
				w.WriteLongint(AddWord(sect.name))
			else
				w.WriteStringZ(sect.name)
		end;
	end;
	
	CompileConfig(sect.items, w, ll);
	
	if (kind and konfNoSections) = 0 then
		w.CloseChunk;
end;

procedure TKonfig.CompileConfig(items: TItemList; w: TMemoryWriter; ll : Boolean);
var
	I, J : Integer;
	item : TSimpleValue;
	int : TIntegerValue;
	u8arr : TByteArrayValue;
	intarr : TIntegerArrayValue;
	floatarr : TFloatArrayValue;
begin
	for I := 0 to items.Count - 1 do
	begin
		item := TSimpleValue(items[I]);
		if item is TSection then
			CompileSection(item as TSection, w, ll)
		else
		begin
			if (kind and konfDebugInfo) <> 0 then
			begin
				if (kind and konfDiktionary) <> 0 then
				begin
					w.WriteLongint(AddWord(item.name));
					w.WriteLongint(AddWord(item.vtype));
 				end else
				begin
					w.WriteStringZ(item.name);
					w.WriteStringZ(item.vtype);
				end
			end;

			if item is TIntegerValue then
			begin
				int := item as TIntegerValue;
				if int.vtype = 'u8' then
					w.WriteByte(int.num)
				else if int.vtype = 's8' then
					w.WriteShortint(int.num)
				else if (int.vtype = 'u16') or (int.vtype = 'fp32_q16') then
					w.WriteWord(int.num)
				else if int.vtype = 's16' then
					w.WriteSmallint(int.num)
				else if (int.vtype = 'u32') or (int.vtype = 'color, u32') then
					w.WriteLongword(int.num)
				else if int.vtype = 's32' then
					w.WriteLongint(int.num)
				else if (int.vtype = 'fp32_q8') or (int.vtype = 'bool8') then
					w.WriteByte(int.num)
				else if int.vtype = 'u64' then
					w.WriteQWord(int.num)
				else if int.vtype = 's64' then
					w.WriteInt64(int.num)
				else if (int.vtype = 'entity_link, uobject_link') or
					 (int.vtype = 'cover_link, ucover_link') then
					w.WriteWord(int.num)
				else
					raise Exception.Create('define integer type ' + int.vtype + ', please');
			end else
			if item is TSingleValue then
				w.WriteSingle((item as TSingleValue).num)
			else
			if item is TStringValue then
			begin
				if (kind and konfDiktionary) <> 0 then
					w.WriteLongint(AddWord((item as TStringValue).str))
				else
					w.WriteStringZ((item as TStringValue).str)
			end else
			if item is TByteArrayValue then
			begin
				u8arr := item as TByteArrayValue;
				w.WriteLongword(Length(u8arr.data));
				w.Write(u8arr.data[0], Length(u8arr.data));
			end else
			if item is TIntegerArrayValue then
			begin
				intarr := item as TIntegerArrayValue;
				if item.vtype = 'vec4s16' then
				begin
					w.WriteSmallint(intarr.data[0]);
					w.WriteSmallint(intarr.data[1]);
					w.WriteSmallint(intarr.data[2]);
					w.WriteSmallint(intarr.data[3]);
				end else
				if item.vtype = 'vec3i' then
				begin
					w.WriteLongint(intarr.data[0]);
					w.WriteLongint(intarr.data[1]);
					w.WriteLongint(intarr.data[2]);
				end else
				if item.vtype = 'vec4i' then
				begin
					w.WriteLongint(intarr.data[0]);
					w.WriteLongint(intarr.data[1]);
					w.WriteLongint(intarr.data[2]);
					w.WriteLongint(intarr.data[3]);
				end else
				if item.vtype = 'u16_array' then
				begin
					if ll then
						w.WriteWord(Length(intarr.data))
					else
						w.WriteLongword(Length(intarr.data));
					for J := 0 to Length(intarr.data) - 1 do
						w.WriteWord(intarr.data[J]);
				end else
				if item.vtype = 'u32_array' then
				begin
					if ll then
						w.WriteWord(Length(intarr.data))
					else
						w.WriteLongword(Length(intarr.data));
					for J := 0 to Length(intarr.data) - 1 do
						w.WriteLongword(intarr.data[J]);
				end else
				if item.vtype = 'identifier_array' then
				begin
					w.WriteLongword(Length(intarr.data));
					for J := 0 to Length(intarr.data) - 1 do
						w.WriteLongword(intarr.data[J]);
				end else
					raise Exception.Create('Unknown TIntegerArrayValue type! (' + item.vtype + ')');
			end else
			if item is TFloatArrayValue then
			begin
				floatarr := item as TFloatArrayValue;
				
				if item.vtype = 'fp32_array' then
				begin
					if ll then
						w.WriteWord(Length(floatarr.data))
					else
						w.WriteLongword(Length(floatarr.data));
				end;
				
				for J := 0 to Length(floatarr.data) - 1 do
					w.WriteSingle(floatarr.data[J]);
			end else
			if item is TBoolValue then
			begin
				if (item as TBoolValue).bool then
					w.WriteByte(1)
				else
					w.WriteByte(0);
			end;
		end;
	end;
end;

function TKonfig.DecompileSection(r: TMemoryReader; ll : Boolean) : TSection;
var
	s : TSection;
	skind : Byte;
	sname : String;
begin
	skind := r.ReadByte;
	
	if (skind and konfDebugInfo) <> 0 then
	begin
		if (skind and konfDiktionary) <> 0 then
			sname := dictionary[r.ReadLongint]
		else
			sname := r.ReadStringZ;
	end;

	s := TSection.Create(sname);
	DecompileConfig(s.items, skind, r, ll);
	Result := s;
end;

procedure TKonfig.DecompileConfig(items : TItemList; kind : Byte; r : TMemoryReader; ll : Boolean);
var
	vname, vtype : String;
	sectdata : TMemoryReader;
	I : Longint;

	u8arr : TByteArrayValue;
	intarr : TIntegerArrayValue;
	floatarr : TFloatArrayValue;

	function SectionComing : Boolean;
	var
		savedpos, remain, crc: Longword;
		skind : Byte;

		sname : String;
		wordid : Longword;
	begin
		SectionComing := False;
		SavedPos := r.pos;

		remain := r.size-r.pos;
		if remain > 9 then // crc+size+kind
		begin
			crc := r.ReadLongword;
			r.ReadLongword; // size
			skind := r.ReadByte;
			remain := r.size-r.pos;

			case skind of
			3 : begin
					try
						sname := r.ReadStringZ;
						if GetStringCrc(sname) = Longint(crc) then
							SectionComing := True;
					except
						SectionComing := False;
					end;
				end;
			5 : if remain >= 4 then begin
					wordid := r.ReadLongword;
					if wordid < Longword(Length(dictionary)) then
						if GetStringCrc(dictionary[wordid]) = Longint(crc) then
							SectionComing := True;
				end;
			end;
		end;

		//Writeln('crc = ', crc, ' remain = ', remain, ' sectioncoming = ', Result);

		r.pos := savedpos;
	end;
begin
	while r.pos < r.size do
	begin
		if SectionComing then
		begin
			sectdata := r.OpenChunk;
			items.Add(DecompileSection(sectdata, ll));
			sectdata.Free;
		end else
		begin
		
			if (kind and konfDebugInfo) <> 0 then
			begin
				if (kind and konfDiktionary) <> 0 then
				begin
					vname := dictionary[r.ReadLongword];
					vtype := dictionary[r.ReadLongword];
				end else 
				begin
					vname := r.ReadStringZ;
					vtype := r.ReadStringZ;
				end;
				
				// DEBUG
				// WriteLn(vname, ' ', vtype);
			end;

			if _IsHint(vtype) then
			begin
				items.Add(TSimpleValue.Create(vname, vtype));
			end else
			begin
				if (vtype = 'u8') or (vtype = 'bool8') then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadByte))
				else
				if vtype = 's8' then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadShortint))
				else
				if (vtype = 'u16') or (vtype = 'fp32_q16') then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadWord))
				else
				if vtype = 's16' then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadSmallint))
				else
				if (vtype = 'u32') or (vtype = 'color, u32') then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadLongword))
				else
				if vtype = 's32' then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadLongint))
				else
				if (vtype = 'fp32') or (vtype = 'angle, fp32') then
					items.Add(TSingleValue.Create(vname, vtype, r.ReadSingle))
				else
				if vtype = 'fp32_q8' then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadByte))
				else
				if vtype = 'fp32_array' then
				begin
					floatarr := TFloatArrayValue.Create(vname, vtype);
					if ll then
						floatarr.FromReader(r, r.ReadWord)
					else
						floatarr.FromReader(r, r.ReadLongword);
					items.Add(floatarr);
				end else
				if vtype = 'u8_array' then
				begin
					u8arr := TByteArrayValue.Create(vname, vtype);
					u8arr.FromReader(r, r.ReadLongword);
					items.Add(u8arr);
				end else
				if vtype = 'u16_array' then
				begin
					intarr := TIntegerArrayValue.Create(vname, vtype);
					if ll then
						SetLength(intarr.data, r.ReadWord)
					else
						SetLength(intarr.data, r.ReadLongword);
					for I := 0 to Length(intarr.data) - 1 do
						intarr.data[I] := r.ReadWord;
				end else
				if vtype = 'u32_array' then
				begin
					intarr := TIntegerArrayValue.Create(vname, vtype);
					if ll then
						SetLength(intarr.data, r.ReadWord)
					else
						SetLength(intarr.data, r.ReadLongword);
					for I := 0 to Length(intarr.data) - 1 do
						intarr.data[I] := r.ReadLongword;
				end else
				if vtype = 'stringz' then
				begin
					if (kind and konfDiktionary) <> 0 then
						items.Add(TStringValue.Create(vname, dictionary[r.ReadLongint]))
					else
						items.Add(TStringValue.Create(vname, r.ReadStringZ));			
				end else
				if vtype = 'pose, matrix' then
				begin
					floatarr := TFloatArrayValue.Create(vname, vtype);
					if ll then // in Last Light matrices became 4x3
						floatarr.FromReader(r, 12)
					else
						floatarr.FromReader(r, 16);
					items.Add(floatarr);
				end else
				if vtype = 'pose, matrix_43T' then
				begin
					floatarr := TFloatArrayValue.Create(vname, vtype);
					floatarr.FromReader(r, 12);
					items.Add(floatarr);
				end else
				if vtype = 'bool' then
					items.Add(TBoolValue.Create(vname, vtype, r.ReadByte <> 0))
				else
				if (vtype = 'vec2f') or (vtype = 'ang2f') then
				begin
					floatarr := TFloatArrayValue.Create(vname, vtype);
					floatarr.FromReader(r, 2);
					items.Add(floatarr);
				end else
				if (vtype = 'vec3f') or (vtype = 'ang3f') then
				begin
					floatarr := TFloatArrayValue.Create(vname, vtype);
					floatarr.FromReader(r, 3);
					items.Add(floatarr);
				end else
				if (vtype = 'vec4f') or (vtype = 'color, vec4f') then
				begin
					floatarr := TFloatArrayValue.Create(vname, vtype);
					floatarr.FromReader(r, 4);
					items.Add(floatarr);
				end else
				if vtype = 'vec4s16' then
				begin
					intarr := TIntegerArrayValue.Create(vname, vtype);
					intarr.FromReaderS16(r, 4);
					items.Add(intarr)
				end else
				if vtype = 'vec3i' then
				begin
					intarr := TIntegerArrayValue.Create(vname, vtype);
					intarr.FromReaderS32(r, 3);
					items.Add(intarr)
				end else
				if vtype = 'vec4i' then
				begin
					intarr := TIntegerArrayValue.Create(vname, vtype);
					intarr.FromReaderS32(r, 4);
					items.Add(intarr)
				end else
				if (vtype = 'entity_link, uobject_link') or
					 (vtype = 'cover_link, ucover_link') then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadWord))
				else
					raise Exception.Create('Unknown type ' + vtype + ' name=' + vname);
			end;

		end;
	end;
end;

// TTextKonfig utils
function ParseValue(parser : TParser) : String;
var
	t2 : String;
begin
	parser.NextToken(t2);
	Result := t2;
end;

function ParseInteger(parser : TParser) : Int64;
begin
	Result := StrToInt64(ParseValue(parser));
end;

function ParseArray(parser : TParser) : TStringArray;
var
	t : String;
	count : Longint;
	arr : TStringArray;
begin
	parser.NextToken(t);
	if t <> '[' then
		parser.Error('''['' expected');

	SetLength(arr, 0);
	count := 0;

	parser.NextToken(t);
	while t <> ']' do
	begin
		Inc(count);
		SetLength(arr, count);
		arr[count-1] := t;

		parser.NextToken(t);
		if (t <> ',') and (t <> ']') then
			parser.Error('expected '','' or '']''');

		if t = ',' then
			parser.NextToken(t);
	end;

	Result := arr;
end;

procedure ParseConfig(sect : Longword; items : TItemList; parser : TParser);
var
	t1, t2, t3, t4 : String;
	arr : TStringArray;

	section : TSection;
	u8arr : TByteArrayValue;
	intarr : TIntegerArrayValue;
	floatarr : TFloatArrayValue;
begin
	while parser.NextToken(t1, True) do
	begin
		parser.NextToken(t2);
		if t2 = ':' then // variable
		begin
			parser.NextToken(t3); // type

			if _IsHint(t3) then
			begin
				items.Add(TSimpleValue.Create(t1, t3))
			end else
			begin
				parser.NextToken(t4);
				if t4 <> '=' then
					parser.Error('''='' expected');
						
				if (t3 = 'u8') or
					 (t3 = 's8') or
					 (t3 = 'u16') or
					 (t3 = 's16') or
					 (t3 = 'u32') or
					 (t3 = 's32') or
					 (t3 = 'u64') or
					 (t3 = 's64') or
					 (t3 = 'entity_link, uobject_link') or
					 (t3 = 'cover_link, ucover_link') or
					 (t3 = 'bool8') or
					 (t3 = 'fp32_q8') or 
					 (t3 = 'fp32_q16') or 
					 (t3 = 'color, u32') then
				begin
					items.Add(TIntegerValue.Create(t1, t3, ParseInteger(parser)));
				end else
				if (t3 = 'fp32') or (t3 = 'angle, fp32') then
					items.Add(TSingleValue.Create(t1, t3, ParseFloat(ParseValue(parser))))
				else
				if t3 = 'fp32_array' then
				begin
					arr := ParseArray(parser);
					floatarr := TFloatArrayValue.Create(t1, t3);
					floatarr.FromStrings(arr);
					items.Add(floatarr);
				end else
				if t3 = 'u8_array' then
				begin
					arr := ParseArray(parser);
					u8arr := TByteArrayValue.Create(t1, t3);
					u8arr.FromStrings(arr);
					items.Add(u8arr);
				end else
				if (t3 = 'u16_array') or 
				   (t3 = 'u32_array') or 
				   (t3 = 'identifier_array') then
				begin
					arr := ParseArray(parser);
					intarr := TIntegerArrayValue.Create(t1, t3);
					intarr.FromStrings(arr);
					items.Add(intarr);
				end else
				if t3 = 'stringz' then
				begin
					items.Add(TStringValue.Create(t1, ParseValue(parser)));
				end else
				if t3 = 'pose, matrix' then
				begin
					arr := ParseArray(parser);
					if (Length(arr) <> 16) and (Length(arr) <> 12) then
						parser.Error('matrix must contain 12 or 16 elements');
					floatarr := TFloatArrayValue.Create(t1, t3);
					floatarr.FromStrings(arr);
					items.Add(floatarr);
				end else
				if t3 = 'pose, matrix_43T' then
				begin
					arr := ParseArray(parser);
					if Length(arr) <> 12 then
						parser.Error('matrix43_T must contain 12 elements');
					floatarr := TFloatArrayValue.Create(t1, t3);
					floatarr.FromStrings(arr);
					items.Add(floatarr);
				end else
				if t3 = 'bool' then
				begin
					items.Add(TBoolValue.Create(t1, t3,
					AnsiCompareText(ParseValue(parser), 'True') = 0));
				end else
				if (t3 = 'vec2f') or (t3 = 'ang2f') then
				begin
					arr := ParseArray(parser);
					if Length(arr) <> 2 then
						parser.Error('vec2f must contain 2 elements');
					floatarr := TFloatArrayValue.Create(t1, t3);
					floatarr.FromStrings(arr);
					items.Add(floatarr);
				end else
				if (t3 = 'vec3f') or (t3 = 'ang3f') then
				begin
					arr := ParseArray(parser);
					if Length(arr) <> 3 then
						parser.Error('vec3f must contain 3 elements');
					floatarr := TFloatArrayValue.Create(t1, t3);
					floatarr.FromStrings(arr);
					items.Add(floatarr);
				end else
				if (t3 = 'vec4f') or (t3 = 'color, vec4f') then
				begin
					arr := ParseArray(parser);
					if Length(arr) <> 4 then
						parser.Error('vec4f must contain 4 elements');
					floatarr := TFloatArrayValue.Create(t1, t3);
					floatarr.FromStrings(arr);
					items.Add(floatarr);
				end else
				if t3 = 'vec4s16' then
				begin
					arr := ParseArray(parser);
					if Length(arr) <> 4 then
						parser.Error('vec4s16 must contain 4 elements');
					intarr := TIntegerArrayValue.Create(t1, t3);
					intarr.FromStrings(arr);
					items.Add(intarr);
				end else
				if t3 = 'vec3i' then
				begin
					arr := ParseArray(parser);
					if Length(arr) <> 3 then
						parser.Error('vec3i must contain 3 elements');
					intarr := TIntegerArrayValue.Create(t1, t3);
					intarr.FromStrings(arr);
					items.Add(intarr);
				end else
				if t3 = 'vec4i' then
				begin
					arr := ParseArray(parser);
					if Length(arr) <> 4 then
						parser.Error('vec4i must contain 4 elements');
					intarr := TIntegerArrayValue.Create(t1, t3);
					intarr.FromStrings(arr);
					items.Add(intarr);
				end else
					parser.Error('unknown type ''' + t3 + '''');
			end;
			
			parser.NextToken(t4);
			if t4 <> ';' then
				parser.Error('syntax error, '';'' expected');
		end else
		if t2 = '=' then // section
		begin
			parser.NextToken(t3);
			if t3 <> 'begin' then
				parser.Error('''begin'' expected');

			section := TSection.Create(t1);
			ParseConfig(sect+1, section.items, parser);
			items.Add(section);
		end else
		begin
			if (t1 = 'end') and (t2 = ';')	then
			begin
				if sect > 0 then
					Exit
				else
					parser.Error('unexpected ''end''');
			end else
				parser.Error('syntax error');
		end;
	end;

	if sect > 0 then
		parser.Error('''end'' expected');
end;

procedure ParseConfigJSON(sect : Longword; items : TItemList; parser : TParser);
var
	I : Longint;
	
	t1, t2, t3, t4 : String;
	ttype : TTokenType;
	arr : TStringArray;

	section : TSection;
	u8arr : TByteArrayValue;
	intarr : TIntegerArrayValue;
	floatarr : TFloatArrayValue;
begin
	while parser.NextToken(t1, False, @ttype) do
	begin
		I := Length(t1);
		
		if (ttype = tokDelimiter) and (t1 = '}') then
		begin
			// empty section case
			Break;
		end else
		if (I > 0) and (t1[I] = ']') then
		begin
			// this thing has type, it's variable
			Dec(I);
			
			while (I > 0) and (t1[I] <> '[') do
				Dec(I);
				
			if I = 0 then
				parser.Error('cannot find ''['' in name of a parameter');
						
			t3 := Copy(t1, I+1, Length(t1)-(I+1)); // t3 = type of variable
			t1 := Copy(t1, 1, I-1); // t1 = name of variable
			
			parser.NextToken(t2, False, @ttype);
			if (ttype <> tokDelimiter) or (t2 <> ':') then
				parser.Error(''':'' expected');
				
			if _IsHint(t3) then
			begin
				parser.NextToken(t2, False, @ttype);
				if (ttype <> tokIdentifier) or (t2 <> 'null') then
					parser.Error('''null'' expected');
			
				items.Add(TSimpleValue.Create(t1, t3))
			end else
			if (t3 = 'u8') or
				 (t3 = 's8') or
				 (t3 = 'u16') or
				 (t3 = 's16') or
				 (t3 = 'u32') or
				 (t3 = 's32') or
				 (t3 = 'u64') or
				 (t3 = 's64') or
				 (t3 = 'entity_link, uobject_link') or
				 (t3 = 'cover_link, ucover_link') or
				 (t3 = 'bool8') or
				 (t3 = 'fp32_q8') or 
				 (t3 = 'fp32_q16') or 
				 (t3 = 'color, u32') then
			begin
				items.Add(TIntegerValue.Create(t1, t3, ParseInteger(parser)));
			end else
			if (t3 = 'fp32') or (t3 = 'angle, fp32') then
				items.Add(TSingleValue.Create(t1, t3, ParseFloat(ParseValue(parser))))
			else
			if t3 = 'fp32_array' then
			begin
				arr := ParseArray(parser);
				floatarr := TFloatArrayValue.Create(t1, t3);
				floatarr.FromStrings(arr);
				items.Add(floatarr);
			end else
			if t3 = 'u8_array' then
			begin
				arr := ParseArray(parser);
				u8arr := TByteArrayValue.Create(t1, t3);
				u8arr.FromStrings(arr);
				items.Add(u8arr);
			end else
			if (t3 = 'u16_array') or 
			   (t3 = 'u32_array') or 
			   (t3 = 'identifier_array') then
			begin
				arr := ParseArray(parser);
				intarr := TIntegerArrayValue.Create(t1, t3);
				intarr.FromStrings(arr);
				items.Add(intarr);
			end else
			if t3 = 'stringz' then
			begin
				items.Add(TStringValue.Create(t1, ParseValue(parser)));
			end else
			if t3 = 'pose, matrix' then
			begin
				arr := ParseArray(parser);
				if (Length(arr) <> 16) and (Length(arr) <> 12) then
					parser.Error('matrix must contain 12 or 16 elements');
				floatarr := TFloatArrayValue.Create(t1, t3);
				floatarr.FromStrings(arr);
				items.Add(floatarr);
			end else
			if t3 = 'pose, matrix_43T' then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 12 then
					parser.Error('matrix43_T must contain 12 elements');
				floatarr := TFloatArrayValue.Create(t1, t3);
				floatarr.FromStrings(arr);
				items.Add(floatarr);
			end else
			if t3 = 'bool' then
			begin
				parser.NextToken(t2, False, @ttype);
				if (ttype <> tokIdentifier) or ((t2 <> 'true') and (t2 <> 'false')) then
					parser.Error('boolean value expected');
				items.Add(TBoolValue.Create(t1, t3, (t2 = 'true')));
			end else
			if (t3 = 'vec2f') or (t3 = 'ang2f') then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 2 then
					parser.Error('vec2f must contain 2 elements');
				floatarr := TFloatArrayValue.Create(t1, t3);
				floatarr.FromStrings(arr);
				items.Add(floatarr);
			end else
			if (t3 = 'vec3f') or (t3 = 'ang3f') then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 3 then
					parser.Error('vec3f must contain 3 elements');
				floatarr := TFloatArrayValue.Create(t1, t3);
				floatarr.FromStrings(arr);
				items.Add(floatarr);
			end else
			if (t3 = 'vec4f') or (t3 = 'color, vec4f') then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 4 then
					parser.Error('vec4f must contain 4 elements');
				floatarr := TFloatArrayValue.Create(t1, t3);
				floatarr.FromStrings(arr);
				items.Add(floatarr);
			end else
			if t3 = 'vec4s16' then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 4 then
					parser.Error('vec4s16 must contain 4 elements');
				intarr := TIntegerArrayValue.Create(t1, t3);
				intarr.FromStrings(arr);
				items.Add(intarr);
			end else
			if t3 = 'vec3i' then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 3 then
					parser.Error('vec3i must contain 3 elements');
				intarr := TIntegerArrayValue.Create(t1, t3);
				intarr.FromStrings(arr);
				items.Add(intarr);
			end else
			if t3 = 'vec4i' then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 4 then
					parser.Error('vec4i must contain 4 elements');
				intarr := TIntegerArrayValue.Create(t1, t3);
				intarr.FromStrings(arr);
				items.Add(intarr);
			end else
				parser.Error('unknown type ''' + t3 + '''');	
		end else
		begin
			// has no type, it must be section
			parser.NextToken(t2, False, @ttype);
			if (ttype <> tokDelimiter) or (t2 <> ':') then
				parser.Error(''':'' expected');
				
			parser.NextToken(t3, False, @ttype);
			if (ttype <> tokDelimiter) or (t3 <> '{') then
				parser.Error('''{'' expected');	
				
			section := TSection.Create(t1);
			ParseConfigJSON(sect+1, section.items, parser);
			items.Add(section);		
		end;
		
		parser.NextToken(t4, False, @ttype);
		if (ttype = tokDelimiter) and (t4 = ',') then
			Continue
		else if (ttype = tokDelimiter) and (t4 = '}') then
			Break
		else
			parser.Error('syntax error');
	end;
end;

function Quote(s : String ; bforcequote : Boolean = False) : String;
var
	bquote : Boolean;
	qch : Char;
	I : Integer;
begin
	bquote := False;
	qch := '''';
	for I := 1 to Length(s) do
	begin
		if s[I] in [' ', ',', #10] then
			bquote := True;
		if s[I] = '''' then
			qch := '"';
	end;

	if bquote or bforcequote or (s = '') then
		Result := qch + s + qch
	else
		Result := s;
end;

procedure PrintConfig(ident : Integer; w : TMemoryWriter; items : TItemList);
var
	I, J : Integer;
	item : TSimpleValue;

	u8arr : TByteArrayValue;
	intarr : TIntegerArrayValue;
	floatarr : TFloatArrayValue;

	procedure Print(const l : String);
	begin
		w.WriteString(l);
	end;
	procedure Println(const l : String);
	begin
		w.WriteString(l);
		w.WriteWord($0A0D);
	end;
begin
	for I := 0 to items.Count - 1 do
	begin
		item := TSimpleValue(items[I]);
		if item is TSection then
		begin
			Println(StringOfChar(#9, ident) + Quote(item.name) + ' = begin');
			PrintConfig(ident+1, w, TSection(item).items);
			Println(StringOfChar(#9, ident) + 'end;');
		end else
		begin
			Print(StringOfChar(#9, ident) + Quote(item.name) + ' : ' + Quote(item.vtype));
			if item is TIntegerValue then
				Print(' = ' + IntToStr((item as TIntegerValue).num))
			else if item is TSingleValue then
				Print(' = ' + PrintFloat((item as TSingleValue).num))
			else if item is TStringValue then
				Print(' = ' + Quote((item as TStringValue).str, True))
			else if item is TByteArrayValue then
			begin
				Print(' = [');
				u8arr := item as TByteArrayValue;
				for J := 0 to Length(u8arr.data) - 1 do
				begin
					if J > 0 then
						Print(', ');
					Print(IntToStr(u8arr.data[J]));
				end;
				Print(']');
			end
			else if item is TIntegerArrayValue then
			begin
				Print(' = [');
				intarr := item as TIntegerArrayValue;
				for J := 0 to Length(intarr.data) - 1 do
				begin
					if J > 0 then
						Print(', ');
					Print(IntToStr(intarr.data[J]));
				end;
				Print(']');
			end
			else if item is TFloatArrayValue then
			begin
				Print(' = [');
				floatarr := item as TFloatArrayValue;
				for J := 0 to Length(floatarr.data) - 1 do
				begin
					if J > 0 then
						Print(', ');
					Print(PrintFloat(floatarr.data[J]));
				end;
				Print(']');
			end
			else if item is TBoolValue then
			begin
				if (item as TBoolValue).bool then
					Print(' = True')
				else
					Print(' = False')
			end;

			Println(';');
		end;
	end;
end;

procedure PrintConfigJSON(ident : Integer; w : TMemoryWriter; items : TItemList);
var
	I, J : Integer;
	item : TSimpleValue;

	u8arr : TByteArrayValue;
	intarr : TIntegerArrayValue;
	floatarr : TFloatArrayValue;

	procedure Print(const l : String);
	begin
		w.WriteString(l);
	end;
	procedure Println(const l : String);
	begin
		w.WriteString(l);
		w.WriteWord($0A0D);
	end;
begin
	for I := 0 to items.Count - 1 do
	begin
		item := TSimpleValue(items[I]);
		if item is TSection then
		begin
			Print(StringOfChar(#9, ident) + '"');
			PrintEscapedString(w, item.name, '"');
			Println('" : {');
			
			PrintConfigJSON(ident+1, w, TSection(item).items);
			
			if I < items.Count - 1 then
				Println(StringOfChar(#9, ident) + '},')
			else
				Println(StringOfChar(#9, ident) + '}')
		end else
		begin
			Print(StringOfChar(#9, ident) + '"');
			PrintEscapedString(w, item.name + '[' + item.vtype + ']', '"');
			Print('"');
			
			if _IsHint(item.vtype) then
				Print(' : null')
			else if item is TIntegerValue then
				Print(' : ' + IntToStr((item as TIntegerValue).num))
			else if item is TSingleValue then
				Print(' : ' + PrintFloat((item as TSingleValue).num))
			else if item is TStringValue then
			begin
				Print(' : "');
				PrintEscapedString(w, (item as TStringValue).str, '"');
				Print('"');
			end 
			else if item is TByteArrayValue then
			begin
				Print(' : [');
				u8arr := item as TByteArrayValue;
				for J := 0 to Length(u8arr.data) - 1 do
				begin
					if J > 0 then
						Print(', ');
					Print(IntToStr(u8arr.data[J]));
				end;
				Print(']');
			end
			else if item is TIntegerArrayValue then
			begin
				Print(' : [');
				intarr := item as TIntegerArrayValue;
				for J := 0 to Length(intarr.data) - 1 do
				begin
					if J > 0 then
						Print(', ');
					Print(IntToStr(intarr.data[J]));
				end;
				Print(']');
			end
			else if item is TFloatArrayValue then
			begin
				Print(' : [');
				floatarr := item as TFloatArrayValue;
				for J := 0 to Length(floatarr.data) - 1 do
				begin
					if J > 0 then
						Print(', ');
					Print(PrintFloat(floatarr.data[J]));
				end;
				Print(']');
			end
			else if item is TBoolValue then
			begin
				if (item as TBoolValue).bool then
					Print(' : true')
				else
					Print(' : false')
			end;

			if I < items.Count - 1 then
				Println(',')
			else
				Println('')
		end;
	end;
end;

// TTextKonfig
constructor TTextKonfig.Create;
begin
	inherited;
	root := TSection.Create('root');
end;

constructor TTextKonfig.Create(root : TSection);
begin
	inherited Create;
	self.root := root;
end;

destructor TTextKonfig.Destroy;
begin
	root.Free;
	inherited;
end;

function TTextKonfig.Copy : TTextKonfig;
begin
	if self = nil then
		Result := nil
	else
		Result := TTextKonfig.Create(TSection(root.Copy));
end;

procedure TTextKonfig.Load(const str: String);
var
	parser : TParser;
begin
	parser := TParser.Create(str, [',', ':', ';', '=', '[', ']'], False);
	try
		parser.StripComments;
		ParseConfig(0, root.items, parser);
	finally
		parser.Free;
	end;
end;

procedure TTextKonfig.Save(var str: string);
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	w.delta := 512*1024;
	PrintConfig(0, w, root.items);
	w.WriteByte(0);
	
	str := PAnsiChar(w.data);
	
	w.Free;
end;

procedure TTextKonfig.LoadFromFile(const filename : String);
var
	f : TFileStream;
	S : String;
begin
	f := TFileStream.Create(filename, fmOpenRead);
	try
		SetLength(S, f.Size);
		f.ReadBuffer(S[1], Length(S));
		Load(S);
	finally
		f.Free;
	end;
end;

procedure TTextKonfig.SaveToFile(const filename : String);
var
	f : TFileStream;
	S : String;
begin
	Save(S);
	
	f := TFileStream.Create(filename, fmCreate);
	try
		f.WriteBuffer(S[1], Length(S));
	finally
		f.Free;
	end;
end;

procedure TTextKonfig.LoadJSON(const str: String);
var
	parser : TParser;
	t1 : String;
begin
	parser := TParser.Create(str, [',', ':', '{', '}', '[', ']'], True);
	try
		parser.StripCommentsCPlusPlus;
		
		parser.NextToken(t1);
		if t1 <> '{' then
			parser.Error('object expected');
		
		ParseConfigJSON(0, root.items, parser);
	finally
		parser.Free;
	end;
end;

procedure TTextKonfig.LoadJSONFromFile(const filename : String);
var
	f : TFileStream;
	S : String;
begin
	f := TFileStream.Create(filename, fmOpenRead);
	try
		SetLength(S, f.Size);
		f.ReadBuffer(S[1], Length(S));
		LoadJSON(S);
	finally
		f.Free;
	end;
end;

procedure TTextKonfig.SaveJSON(var str : String);
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	w.delta := 512*1024;
	
	w.WriteString('{'#10);
	PrintConfigJSON(1, w, root.items);
	w.WriteString('}'#10);
	
	w.WriteByte(0);
	
	str := PAnsiChar(w.data);
	
	w.Free;
end;

procedure TTextKonfig.SaveJSONToFile(const filename : String);
var
	f : TFileStream;
	S : String;
begin
	SaveJSON(S);
	
	f := TFileStream.Create(filename, fmCreate);
	try
		f.WriteBuffer(S[1], Length(S));
	finally
		f.Free;
	end;
end;

initialization
	common_types_table := XHashTable.Create('Common types table', 255);
	common_names_table := XHashTable.Create('Common names table', 16777215);
end.