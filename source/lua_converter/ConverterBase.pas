unit ConverterBase;

interface
uses fgl, LUATable, Konfig, vmath;

type
	TConverterProc = procedure(l : TLuaTable; s : TSection);
	TConverterProcMap = TFPGMap<String,TConverterProc>;
	
function ParseLuaFloat(t : TLuaObject) : Single;
function ParseLuaInt(t : TLuaObject) : Longint;
function ParseLuaVec2(t : TLuaTable) : TVec2;
function ParseLuaVec3(t : TLuaTable) : TVec3;
function ParseLuaVec4(t : TLuaTable) : TVec4;
function ParseLuaVec4S16(t : TLuaTable) : TVec4S16;
function ParseLuaMatrix43(t : TLuaTable) : TMatrix;
function ParseLuaMatrix43T(t : TLuaTable) : TMatrix;

function RecStr(const pref : String; num : Longint; digits : Longint) : String;

implementation
uses sysutils;

function ParseLuaFloat(t : TLuaObject) : Single;
begin
	if t.DataType = ltString then
		Result := Single(Longint(StrToInt(t.AsString)))
	else if t.DataType = ltNumber then
		Result := t.AsNumber
	else
		Result := t.AsReal;
end;

function ParseLuaInt(t : TLuaObject) : Longint;
begin
	if t.DataType = ltString then
		Result := Longint(StrToInt(t.AsString))
	else
		Result := t.AsNumber;
end;

function ParseLuaVec2(t : TLuaTable) : TVec2;
begin
	Result.x := t.ItemById[0].ParseFloat;
	Result.y := t.ItemById[1].ParseFloat;
end;

function ParseLuaVec3(t : TLuaTable) : TVec3;
begin
	Result.x := t.ItemById[0].ParseFloat;
	Result.y := t.ItemById[1].ParseFloat;
	Result.z := t.ItemById[2].ParseFloat;
end;

function ParseLuaVec4(t : TLuaTable) : TVec4;
begin
	Result.x := t.ItemById[0].ParseFloat;
	Result.y := t.ItemById[1].ParseFloat;
	Result.z := t.ItemById[2].ParseFloat;
	Result.w := t.ItemById[3].ParseFloat;
end;

function ParseLuaVec4S16(t : TLuaTable) : TVec4S16;
begin
	Result.x := t.ItemById[0].ParseInt;
	Result.y := t.ItemById[1].ParseInt;
	Result.z := t.ItemById[2].ParseInt;
	Result.w := t.ItemById[3].ParseInt;
end;

function ParseLuaMatrix43(t : TLuaTable) : TMatrix;
begin
	Result[1,1] := ParseLuaFloat(t.ItemById[0]);
	Result[1,2] := ParseLuaFloat(t.ItemById[1]);
	Result[1,3] := ParseLuaFloat(t.ItemById[2]);
	Result[1,4] := 0.0;
	
	Result[2,1] := ParseLuaFloat(t.ItemById[3]);
	Result[2,2] := ParseLuaFloat(t.ItemById[4]);
	Result[2,3] := ParseLuaFloat(t.ItemById[5]);
	Result[2,4] := 0.0;
	
	Result[3,1] := ParseLuaFloat(t.ItemById[6]);
	Result[3,2] := ParseLuaFloat(t.ItemById[7]);
	Result[3,3] := ParseLuaFloat(t.ItemById[8]);
	Result[3,4] := 0.0;	
	
	Result[4,1] := ParseLuaFloat(t.ItemById[9]);
	Result[4,2] := ParseLuaFloat(t.ItemById[10]);
	Result[4,3] := ParseLuaFloat(t.ItemById[11]);
	Result[4,4] := 0.0;
end;

function ParseLuaMatrix43T(t : TLuaTable) : TMatrix;
begin
	Result[1,1] := ParseLuaFloat(t.ItemById[0]);
	Result[1,2] := ParseLuaFloat(t.ItemById[4]);
	Result[1,3] := ParseLuaFloat(t.ItemById[8]);
	Result[1,4] := 0.0;
	
	Result[2,1] := ParseLuaFloat(t.ItemById[1]);
	Result[2,2] := ParseLuaFloat(t.ItemById[5]);
	Result[2,3] := ParseLuaFloat(t.ItemById[9]);
	Result[2,4] := 0.0;
	
	Result[3,1] := ParseLuaFloat(t.ItemById[2]);
	Result[3,2] := ParseLuaFloat(t.ItemById[6]);
	Result[3,3] := ParseLuaFloat(t.ItemById[10]);
	Result[3,4] := 0.0;	
	
	Result[4,1] := ParseLuaFloat(t.ItemById[3]);
	Result[4,2] := ParseLuaFloat(t.ItemById[7]);
	Result[4,3] := ParseLuaFloat(t.ItemById[11]);
	Result[4,4] := 0.0;
end;

function RecStr(const pref : String; num : Longint; digits : Longint) : String;
var
	n : String;
begin
	n := IntToStr(num);
	RecStr := pref + StringOfChar('0', digits-Length(n)) + n;
end;

end.