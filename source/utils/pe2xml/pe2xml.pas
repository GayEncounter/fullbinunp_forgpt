program pe2xml;
uses sysutils, classes, chunkedFile;

type
  TAttributeType = ( atStringZ = 1, atLongint = 2, atSmallint = 3, atShortint = 4, atLongword = 5, atWord = 6, atByte = 7 );

procedure RestoreXml(var f : TextFile; r : TMemoryReader);
var
  node_names : array[1..256] of String;
  attr_names : array[1..256] of String;
  attr_types : array[1..256] of TAttributeType;

  count : Longint;
  at : Byte;
  str : String;

  procedure PrintNodes(indent : Integer = 0);
  var
    attrid : Byte;
    nameid : Byte;
  begin
    while r.pos < r.size do
    begin
      nameid := r.ReadByte;
      if nameid <> 0 then
      begin
        Write(f, StringOfChar(#9, indent), '<', node_names[nameid], ' ');
        attrid := r.ReadByte;
        while attrid <> 0 do
        begin
          Write(f, attr_names[attrid], '="');
          case attr_types[attrid] of
            atStringZ: Write(f, r.ReadStringZ);
            atLongint: Write(f, r.ReadLongint);
            atSmallint: Write(f, r.ReadSmallint);
            atShortint: Write(f, r.ReadShortint);
            atLongword: Write(f, r.ReadLongword);
            atWord: Write(f, r.ReadWord);
            atByte: Write(f, r.ReadByte);
            else raise Exception.Create('unknown attr type ' + IntToStr(Integer(attr_types[attrid])));
          end;
          Write(f, '" ');

          attrid := r.ReadByte;
        end;
        WriteLn(f, '>');

        PrintNodes(indent + 1);

        WriteLn(f, StringOfChar(#9, indent), '</', node_names[nameid], '>');
      end else
        Exit;
    end;
  end;
begin

  count := 0;
  str := r.ReadStringZ;
  while str <> '' do
  begin
    Inc(count);
    node_names[count] := str;
    str := r.ReadStringZ;
  end;

  count := 0;
  at := r.ReadByte;
  while at <> 0 do
  begin
    Inc(count);
    attr_names[count] := r.ReadStringZ;
    attr_types[count] := TAttributeType(at);
    at := r.ReadByte;
  end;

  PrintNodes;
end;

var
  I, len : Longint;

  nav_map : TMemoryReader;
  mesh : TMemoryReader;

  tf : TextFile;
begin
  nav_map := TMemoryReader.CreateFromFile('nav_map.pe');

  len := nav_map.ReadLongint;
  mesh := TMemoryReader.CreateSlice(nav_map, nav_map.pos, len);
  Inc(nav_map.pos, len);

  Assign(tf, 'mesh3D.xml');
  ReWrite(tf);
  RestoreXml(tf, mesh);

  CloseFile(tf);
  mesh.Free;

  WriteLn(nav_map.ReadLongint); // preprocess count ?

  for I := 1 to 3 do
  begin
    WriteLn('unknown', I, ' = ', nav_map.ReadSingle); // unknown

    // collision preprocess
    len := nav_map.ReadLongint;
    mesh := TMemoryReader.CreateSlice(nav_map, nav_map.pos, len);
    Inc(nav_map.pos, len);

    Assign(tf, 'collisionPreprocess'+IntToStr(I)+'.xml');
    ReWrite(tf);
    RestoreXml(tf, mesh);

    CloseFile(tf);
    mesh.Free;

    // pathfind preprocess
    len := nav_map.ReadLongint;
    mesh := TMemoryReader.CreateSlice(nav_map, nav_map.pos, len);
    Inc(nav_map.pos, len);

    Assign(tf, 'pathfindPreprocess'+IntToStr(I)+'.xml');
    ReWrite(tf);
    RestoreXml(tf, mesh);

    CloseFile(tf);
    mesh.Free;
  end;

  nav_map.Free;

//  mesh := TMemoryReader.CreateFromFile('collisionPreprocess1.xml');
//  RestoreXml(mesh);
//  mesh.Free;
end.