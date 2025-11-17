uses Konfig, chunkedFile, sysutils;

var
  r, kr : TMemoryReader;
  w, kw : TMemoryWriter;

  K : TKonfig;
  tk : TTextKonfig;
  I, count : Longint;

  len : Longword;
  tex_name : String;

	texture_type : TIntegerValue;
	source_name : TStringValue;
  det_name : TStringValue;
  bump_name : TStringValue;
  width, height : TIntegerValue;
  streamable : TBoolValue;
  
  aliases : TTextKonfig;
  sect, sect2 : TSection;
begin
  r := TMemoryReader.CreateFromFile('textures_handles_storage.bin');
  w := TMemoryWriter.Create;
  
  aliases := TTextKonfig.Create;
	sect := aliases.root.AddSect('texture_aliases');

  // header
  r.ReadLongword;
  r.ReadWord;
  count := r.ReadLongint;

  w.WriteLongword($52455641); // ASCII AVER
  w.WriteWord(0);
  w.WriteLongint(count);
  
  sect.AddInt('count', count, 'u32');

  for I := 0 to count-1 do
  begin
    if r.ReadLongint <> I then
      WriteLn('!!! something went wrong');
    len := r.ReadLongword;
    tex_name := r.ReadStringZ;

    K := TKonfig.Create;
    tk := TTextKonfig.Create;

    kr := TMemoryReader.Create(r.data[r.pos], len-(Length(tex_name)+1));
    r.pos := r.pos + (len-(Length(tex_name)+1));
    K.Load(kr);
    kr.Free;

    K.Decompile(tk);

		//texture_type := tk.root.GetParam('texture_type', 'u8') as TIntegerValue;
		//if texture_type.num = 0 then
		//begin
		//	source_name := tk.root.GetParam('source_name', 'stringz') as TStringValue;
		//	source_name.str := 'concrete\concrete_broken_0';
		//end;

    //bump_name := tk.root.GetParam('bump_name', 'stringz') as TStringValue;
    //if bump_name.str <> '' then
    //	bump_name.str := 'concrete\concrete_broken_0_bump';

		//width := tk.root.GetParam('width', 'u32') as TIntegerValue;
		//if width.num > 256 then
		//	width.num := 256;
		//height := tk.root.GetParam('height', 'u32') as TIntegerValue;
		//if height.num > 256 then
		//	height.num := 256;
		
		streamable := tk.root.GetParam('streamable', 'bool') as TBoolValue;
		streamable.bool := False;

    det_name := tk.root.GetParam('det_name', 'stringz') as TStringValue;
    if det_name.str <> '' then
    	det_name.str := 'det\det_snow1';

    K.Compile(tk);

    kw := TMemoryWriter.Create;
    K.Save(kw);

    K.Free;
    tk.Free;

    w.WriteLongint(I);
    w.WriteLongword(kw.size+Length(tex_name)+1);
    w.WriteStringZ(tex_name);
    w.Write(kw.data[0], kw.size);

    kw.Free;
    
    if tex_name <> 'concrete\concrete_broken_0' then
    begin
    	sect2 := sect.AddSect('rec_'+IntToStr(I));
    	sect2.AddStr('src', tex_name);
    	sect2.AddStr('dst', 'concrete\concrete_broken_0');
    end;
  end;

  w.SaveTo('textures_handles_storage.bin.new');
  
  w.Free;
  r.Free;
  
  w := TMemoryWriter.Create;
  K := TKonfig.Create;
  K.kind := 4;
  
  K.Compile(aliases);
  K.Save(w);
  
  w.SaveTo('texture_aliases.bin.new');
  
  K.Free;
  aliases.Free;
  w.Free;
end.