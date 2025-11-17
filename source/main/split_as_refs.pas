program split;
uses windows, classes, sysutils, uCrc, Konfig, framework;

type
	TFastStrinkKontainer = class
		strings_arr : array of String;
		alloc_step : Longint;
		count : Longint;
		
		constructor Create;
		
		procedure Clear;
		function  Add(const str : String) : Longint;
		procedure Merge(k : TFastStrinkKontainer);
		function  GetString(index : Longint) : String;
		
		property Strings[index : Longint] : String read GetString; default;
	end;
	
constructor TFastStrinkKontainer.Create;
begin
	inherited;
	
	SetLength(strings_arr, 0);
	alloc_step := 256;
	count := 0;
end;

procedure TFastStrinkKontainer.Clear;
begin
	SetLength(strings_arr, 0);
	count := 0;
end;
	
function TFastStrinkKontainer.Add(const str : String) : Longint;
begin
	if count >= Length(strings_arr) then
		SetLength(strings_arr, Length(strings_arr) + alloc_step);
		
	strings_arr[count] := str;
	Inc(count);
	
	Result := count - 1;
end;

procedure TFastStrinkKontainer.Merge(k : TFastStrinkKontainer);
var
	I : Longint;
begin
	// could be optimised
	for I := 0 to k.count - 1 do
		Add(k[I]);
end;

function TFastStrinkKontainer.GetString(index : Longint) : String;
begin
	Result := strings_arr[index];
end;

var
	names_db : TFastStrinkKontainer;

procedure GartherPossibleNames(const basedir, dir : String);
var
	sr : TSearchRec;
	str : String;
begin
	if FindFirst(dir + '\*', faDirectory, sr) = 0 then
	begin
		repeat
			if not ((sr.Name = '.') or (sr.Name = '..')) then
				GartherPossibleNames(basedir, dir + '\' + sr.Name)
		until FindNext(sr) <> 0;
		FindClose(sr);
	end;

	// list all .model files, for AS of builtin skeletons
	if FindFirst(dir + '\*.model', faAnyFile xor faDirectory, sr) = 0 then
	begin
		repeat
			str := ChangeFileExt(dir + '\' + sr.Name, ''); // remove .model extension
			str := ExtractRelativePath(basedir, str);      // remove the 'content\meshes\' part
			
			names_db.Add(str);
		until FindNext(sr) <> 0;
		FindClose(sr);
	end;
	
	// list .skeleton.bin files, for AS of external skeleton
	if FindFirst(dir + '\*.skeleton.bin', faAnyFile xor faDirectory, sr) = 0 then
	begin
		repeat
			str := ChangeFileExt(dir + '\' + sr.Name, ''); // remove .bin extension
			str := ChangeFileExt(str, '');                 // remove .skeleton extension
			str := ExtractRelativePath(basedir, str);      // remove the 'content\meshes\' part
			
			names_db.Add(str);
		until FindNext(sr) <> 0;
		FindClose(sr);
	end;
end;

function GuessName(db : TFastStrinkKontainer; var name : String; hash : Longint) : Boolean;
var
	I : Longint;
begin
	for I := 0 to db.count - 1 do
		if GetStringCrc('content\scripts\' + StringReplace(db[I], ':', '_', [rfReplaceAll]) + '.bin') = hash then
		begin
			name := db[I];
			Result := True;
			Exit;
		end;
		
	name := '#' + IntToHex(hash, 8);
	Result := False;
end;

procedure SearchForStateRefsInState(db : TFastStrinkKontainer; const script_name : String; state : TSection);
var
	blocks : TSection;
	block : TSection;
	
	I : Longint;
	
	clsid : String;
	ref : String;
begin
	blocks := state.GetSect('blocks', False);
	if blocks <> nil then
	begin
		for I := 0 to blocks.ParamCount - 1 do
		begin
			if blocks.GetParam(I) is TSection then
			begin
				block := TSection(blocks.GetParam(I));
				
				clsid := block.GetStrDef('clsid', '');
				
				if clsid = 'state/ref' then
				begin
					ref := block.GetStrDef('ref', '');
					if ref <> '' then
					begin
						db.Add(script_name + ':' + ref);
					end;
				end;
				
				if clsid = 'state/state' then
				begin
					SearchForStateRefsInState(db, script_name, block);
				end;
			end;
		end;
	end;
end;

procedure SearchForStateRefsInScript(db : TFastStrinkKontainer; const script_name : String; script : TTextKonfig);
var
	skeleton : TSection;
begin
	skeleton := script.root.GetSect('skeleton', False);
	if skeleton <> nil then
		SearchForStateRefsInState(db, script_name, skeleton);
end;

procedure SearchForStateRefs(const filename : String);
var
	hash, len : Longint;
	scripts : TFileStream;
	name : String;
	
	buffer : TMemoryStream;
	K : TKonfig;
	fw : TFramework;
	tk : TTextKonfig;
	
	refs : TFastStrinkKontainer;
	refs2 : TFastStrinkKontainer;
	
	indir : Longint;
begin
	refs := TFastStrinkKontainer.Create;

	scripts := TFileStream.Create(filename, fmOpenRead);
	
	while scripts.Position < scripts.Size do
	begin
		scripts.ReadBuffer(hash, sizeof(hash));
		scripts.ReadBuffer(len, sizeof(len));

		buffer := TMemoryStream.Create;
		buffer.CopyFrom(scripts, len);
		buffer.Seek(0, soBeginning); // rewind

		if GuessName(names_db, name, hash) then
		begin
			// It's a script connected to .model or an external skeleton! Search for state refs in it
			K := TKonfig.Create;
			K.Load(buffer);
			
			fw := TFramework.Create;
			tk := fw.DecompileKonfig(K, 'js\exodus\animationscript.js');
			fw.Free;
			
			K.Free;
			
			SearchForStateRefsInScript(refs, name, tk);
			
			tk.Free;
		end;
		
		buffer.Free;
	end;
	
	refs2 := TFastStrinkKontainer.Create;
	indir := 1;
	
	repeat
		Write('Searhing for ', indir, '-indirection refs...');
		
		// search for I-th indirection refs
		refs2.Clear;
		scripts.Seek(0, soBeginning);
		
		while scripts.Position < scripts.Size do
		begin
			scripts.ReadBuffer(hash, sizeof(hash));
			scripts.ReadBuffer(len, sizeof(len));
	
			buffer := TMemoryStream.Create;
			buffer.CopyFrom(scripts, len);
			buffer.Seek(0, soBeginning); // rewind
	
			if GuessName(refs, name, hash) then
			begin
				// It's a script referenced by another script! Search for state refs in it
				K := TKonfig.Create;
				K.Load(buffer);
				
				fw := TFramework.Create;
				tk := fw.DecompileKonfig(K, 'js\exodus\animationscript.js');
				fw.Free;
				
				K.Free;
				
				SearchForStateRefsInScript(refs2, Copy(name, 1, Pos(':', name) - 1) { extract base script name }, tk);
				
				tk.Free;
			end;
			
			buffer.Free;
		end;
		
		// add refs we found into names_db
		names_db.Merge(refs);
		
		// move refs2 into refs
		refs.Clear;
		refs.Merge(refs2);
		
		WriteLn(' ', refs2.count, ' found');
		Inc(indir);
			
	until refs2.count = 0;
	refs2.Free;
	
	scripts.Free;
	refs.Free;
end;

procedure SplitScripts(const filename : String; path_type : Longint);
var
	hash, len : Longint;
	scripts, one : TFileStream;
	name, path : String;
	buffer : array of byte;
	
	names_found : Longint;
	names_unknown : Longint;
	
	colon : Longint;
	dups : Longint;
begin
	names_found := 0;
	names_unknown := 0;

	scripts := TFileStream.Create(filename, fmOpenRead);
	while scripts.Position < scripts.Size do
	begin
		scripts.ReadBuffer(hash, sizeof(hash));
		scripts.ReadBuffer(len, sizeof(len));

		SetLength(buffer, len);
		scripts.Read(buffer[0], len);

		if GuessName(names_db, name, hash) then
			Inc(names_found)
		else
			Inc(names_unknown);
	
			if (path_type = 1) or (path_type = 2) then
			begin
				colon := Pos(':', name);
				if colon >= 1 then
					path := 'as_refs\' + Copy(name,colon+1) + '.bin'
				else
					path := 'as_refs\' + name + '.bin';
					
					
				if (path_type = 2) and FileExists(path) then // for -p3, preserve all duplicates
				begin
					dups := 1;
					while FileExists(path + IntToStr(dups)) do
						Inc(dups);
					path := path + IntToStr(dups);
				end;
			end else
			path := 'as_refs\' + StringReplace(name, ':', '_', [rfReplaceAll]) + '.bin';
		
		ForceDirectories(ExtractFilePath(path));

		one := TFileStream.Create(path, fmCreate);
		one.WriteBuffer(buffer[0], len);
		one.Free;
	end;
	scripts.Free;
	
	WriteLn(names_found, ' names found');
	WriteLn(names_unknown, ' names unknown');
end;


// Entrypoint
var
	path_type : Longint;
begin
	names_db := TFastStrinkKontainer.Create;
	
	if ParamCount >= 3 then
	begin
		path_type := 0;
		if ParamStr(1) = '-p1' then
			path_type := 0;
		if ParamStr(1) = '-p2' then
			path_type := 1;		
		if ParamStr(1) = '-p3' then
			path_type := 2;
		
			WriteLn('Garthering possible file names...');
			GartherPossibleNames(ParamStr(2), ParamStr(2));
			WriteLn('Done.');
			WriteLn;
	
			WriteLn('Searhing for state/ref''s...');
			SearchForStateRefs(ParamStr(3));
			WriteLn('Done.');
			WriteLn;
			
			WriteLn('Unpacking scripts...');
			SplitScripts(ParamStr(3), path_type);
			WriteLn('Done.');
			WriteLn;
		end;
		
		names_db.Free;
end.