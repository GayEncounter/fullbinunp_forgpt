unit uCrc;

interface

function GetStringCrc(const Data: String): LongInt;

implementation

var
  CrcTable: array[0..255] of LongInt;

procedure InitTable;
var
  I, J: Integer;
begin
for I := 0 to 255 do
  begin
  CrcTable[I] := I;
  for J := 0 to 7 do if Odd(CrcTable[I]) then
    CrcTable[I] := (CrcTable[I] shr 1) xor $EDB88320
  else CrcTable[I] := CrcTable[I] shr 1;
  end;
end;

function GetStringCrc(const Data: String): LongInt;
var
  I, Index: Integer;
begin
Result := -1;
for I := 1 to Length(Data) do
  begin
  Index := (Ord(Data[I]) xor Result) and $000000FF;
  Result := (Result shr 8) xor CrcTable[Index];
  end;
Result := not Result;
GetStringCrc := Result;
end;

initialization
InitTable;
end.