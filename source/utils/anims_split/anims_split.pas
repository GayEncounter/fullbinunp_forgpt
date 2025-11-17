program anims_split;
uses classes, sysutils, chunkedFile, uCrc;

procedure SplitAnims(const filename : String);
var
	f : TMemoryReader;
	w : TMemoryWriter;
	
	ver : Longint;
	count : Longint;
	id, size : Longint;
	anim_name : String;
	anim_size : Longint;
	
	I : Longint;
begin
	f := TMemoryReader.CreateFromFile(filename);
	
	ver := f.ReadLongint;
	if ver <> 1 then
		WriteLn('Warning: version is not 1');
		
	count := f.ReadLongint;
	
	for I := 0 to count - 1 do
	begin
		id := f.ReadLongint;
		size := f.ReadLongint;
		anim_name := f.ReadStringZ;
		
		ForceDirectories('anims\' + ExtractFilePath(anim_name)); // create directory
		anim_size := size - (Length(anim_name) + 1);
		
		w := TMemoryWriter.Create;
		w.Write((f.data + f.pos)^, anim_size);
		w.SaveTo('anims\' + anim_name);
		w.Free;
		
		Inc(f.pos, anim_size);
	end;
	
	f.Free;
end;

procedure ListFilesInDir(sl : TStringList; const basedir, subdir : String);
var
	sr : TSearchRec;	
begin
	if FindFirst(basedir+subdir+'*', faDirectory, sr) = 0 then
	begin
		repeat
			if not ((sr.Name = '.') or (sr.Name = '..')) then
				ListFilesInDir(sl, basedir, subdir + sr.Name + '\')
		until FindNext(sr) <> 0;
		FindClose(sr);
	end;
	
	if FindFirst(basedir+subdir+'*.', faAnyFile xor faDirectory, sr) = 0 then
	begin
		repeat              
			sl.Add(subdir+sr.Name);
		until FindNext(sr) <> 0;
		FindClose(sr);
	end;	
end;

function SortByCrc(list : TStringList; index1, index2 : Integer) : Integer;
var
	crc1 : Longword;
	crc2 : Longword;
begin
	crc1 := Longword(list.Objects[index1]);
	crc2 := Longword(list.Objects[index2]);
	
	if crc1 < crc2 then
		Result := -1
	else if crc1 = crc2 then
		Result := 0
	else
		Result := 1;
end;

procedure MergeDir(const animsdir : String; const resultfile : String);
var
	sl : TStringList;
	w : TMemoryWriter;
	I : Longint;
	
	anim : TMemoryReader;
	anim_name : String;
begin
	// list files in specified directory
	sl := TStringList.Create;
	ListFilesInDir(sl, animsdir+'\', '');
	
	// make CRCs of strings and store in as their 'Object's
	for I := 0 to sl.Count - 1 do
		sl.Objects[I] := TObject( GetStringCrc(sl[I]) );
		
	// sort strings by their CRCs, ascending
	sl.CustomSort(SortByCrc);
	
	// create new anims.bin	file
	w := TMemoryWriter.Create;
	w.WriteLongword(1); // version.. or something
	w.WriteLongword(sl.Count);
	
	for I := 0 to sl.Count - 1 do
	begin
		anim := TMemoryReader.CreateFromFile(animsdir+'\'+sl[I]);
		anim_name := sl[I];
		
		w.WriteLongint(I);
		w.WriteLongint(anim.size + Length(anim_name) + 1);
		w.WriteStringZ(anim_name);
		w.Write(anim.data^, anim.size);
		
		anim.Free;
	end;
	
	w.SaveTo(resultfile);
	
	w.Free;
	sl.Free;
end;

begin
	if (ParamCount = 1) or (ParamCount = 2) then
	begin
		if ParamStr(1) = '-d' then
		begin
			if ParamCount >= 2 then
				SplitAnims(ParamStr(2))
			else
				SplitAnims('anims.bin');
		end else
		if ParamStr(1) = '-c' then
		begin
			if ParamCount >= 2 then
				MergeDir(ParamStr(2), 'anims.bin.new')
			else
				MergeDir('anims', 'anims.bin.new');
		end;
	end else
	begin
		WriteLn('usage:');
		WriteLn(#9'anim_split.exe -d <filename> - split anims.bin into separate files. If filename is not specified anims.bin is assumed');
		WriteLn(#9'anim_split.exe -c <directory> - merge separate files from specified directory into anims.bin. If directory is not specified ''anims'' is assumed'); 
	end;
end.