program saycrc;
uses uCrc, sysutils, chunkedFile;

var
  str : String;
  I, count, CRC : Longint;
  f : TMemoryReader;
begin

  if ParamCount = 1 then
    Writeln('crc32 = ', IntToHex(GetStringCrc(ParamStr(1)), 8));
  if ParamCount = 2 then
  begin
    CRC := StrToInt('$' + ParamStr(2));
    f := TMemoryReader.CreateFromFile(ParamStr(1));
    count := f.ReadLongint;
    for I := 0 to count - 1 do
    begin
      str := f.ReadStringZ;
      if GetStringCrc(str) = CRC then
        WriteLn(I, ' = ', str);
    end;
    f.Free;
  end;
{
  f := TMemoryReader.CreateFromFile('typed_strings.bin');
  f.ReadLongword;
  f.ReadLongword;
  while f.pos < f.size do
  begin
    f.ReadWord;
    str := f.ReadStringZ;
    WriteLn('$', IntToHex(GetStringCrc(str), 8), ' = ', str);
  end;
  f.Free;
}
end.