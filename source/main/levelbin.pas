unit levelbin;

interface
uses Konfig;

const
	//ENTITY_VER_2033   = 17; // or 16
	ENTITY_VER_LL       = 29;
	ENTITY_VER_REDUX    = 30;
	ENTITY_VER_ARKTIKA1 = 43;
	ENTITY_VER_EXODUS   = 50; // or 49

function GetDefaultKindForLevelBin(tk : TTextKonfig) : Byte;

// automatic version detection
function LoadLevelBin(const fn : String; build_15_10_2012 : Boolean = False) : TTextKonfig;
procedure SaveLevelBin(const fn : String; tk : TTextKonfig; kind : Integer);

// manual version select
function LoadLevelBin2033(const fn : String) : TTextKonfig;
function LoadLevelBinLL(const fn : String; build_15_10_2012 : Boolean) : TTextKonfig;
function LoadLevelBinA1(const fn : String) : TTextKonfig;

procedure SaveLevelBin2033(const fn : String; tk : TTextKonfig; kind : Integer);
procedure SaveLevelBinLL(const fn : String; tk : TTextKonfig; kind : Integer);
procedure SaveLevelBinA1(const fn : String; tk : TTextKonfig; kind : Integer);

implementation
uses m2033unp, classes, sysutils, chunkedFile, uCrc, fgl, framework;

const 
	LEVEL_BIN_MAGIC = $6C76656C; // ASCII levl

function GetDefaultKindForLevelBin(tk : TTextKonfig) : Byte;
var
	entities_params : TSection;
	entity_ver : Word;
begin
	entities_params := tk.root.GetSect('entities_params', False);
	if entities_params <> nil then
	begin
		entity_ver := entities_params.GetInt('version', 0, 'u16');
		if entity_ver >= ENTITY_VER_REDUX then
			Result := konfDiktionary or konfMultiChunk
		else
			Result := konfDiktionary; // Last Light
	end else
		Result := konfDebugInfo or konfDiktionary; // 2033
end;
	

function LoadLevelBin(const fn : String; build_15_10_2012 : Boolean) : TTextKonfig;
var
	f : TFileStream;
	magic, dword, filesize : Longword;
begin
	f := TFileStream.Create(fn, fmOpenRead);
	filesize := f.Size;
	f.ReadBuffer(magic, Sizeof(magic));
	f.ReadBuffer(dword, Sizeof(dword));
	f.Free;

	if magic = LEVEL_BIN_MAGIC then
	begin
		if dword = $00000124 then
			Result := LoadLevelBinA1(fn) // exodus too
		else 
		if dword = (filesize-8) then
			Result := LoadLevelBinLL(fn, build_15_10_2012)
		else
			Result := LoadLevelBin2033(fn);
	end else
	begin
		WriteLn('"', fn, '" is not level bin file!');
		Result := nil;
	end;
end;

procedure SaveLevelBin(const fn : String; tk : TTextKonfig; kind : Integer);
var
	entities_params : TSection;
	entity_ver : Word;
begin
	entities_params := tk.root.GetSect('entities_params', False);
	if entities_params <> nil then
	begin
		entity_ver := entities_params.GetInt('version', 0, 'u16');
		if entity_ver >= ENTITY_VER_ARKTIKA1 then
			SaveLevelBinA1(fn, tk, kind)
		else
			SaveLevelBinLL(fn, tk, kind);
	end else
		SaveLevelBin2033(fn, tk, kind);
end;

procedure RecoverCrc(root : TSection);
var
	entities, e : TSection;
	_class, _sdk : TStringValue;
	
	_vss, _v, _blocks, _block : TSection;
	_clsid : TStringValue;
	
	I, J, K : Longint;
begin
	entities := root.GetSect('entities', False);
	if entities <> nil then
	begin
		for I := 1 to entities.items.Count - 1 do
		begin
			e := TObject(entities.items[I]) as TSection;
			
			_class := e.GetParam('class', 'stringz') as TStringValue;
			_sdk := e.GetParam('static_data_key', 'stringz') as TStringValue;
			
			e.Replace(_class, TIntegerValue.Create('class', 'u32', GetStringCrc(_class.str)));
			e.Replace(_sdk, TIntegerValue.Create('static_data_key', 'u32', GetStringCrc(_sdk.str)));
			
			_vss := e.GetSect('vss_ver_7', False);
			if _vss = nil then
				_vss := e.GetSect('vss_ver_6');
			
			for J := 1 to _vss.items.Count - 1 do
			begin
				if TObject(_vss.items[J]) is TSection then
				begin
					_v := TSection(_vss.items[J]);
					_blocks := _v.GetSect('blocks');
				
					for K := 0 to _blocks.items.Count - 1 do
					begin
						if TObject(_blocks.items[K]) is TSection then
						begin
							_block := TSection(_blocks.items[K]);
							_clsid := _block.GetParam('clsid', 'stringz') as TStringValue;
							
							_block.Replace(_clsid, TIntegerValue.Create('clsid', 'u32', GetStringCrc(_clsid.str)));
						end;
					end;
				end;
			end;
			
			// commons_vs also may contain blocks (Arktika.1 and further)
			_vss := e.GetSect('commons_vs', False);
			if _vss <> nil then
			begin
				for J := 1 to _vss.items.Count - 1 do
				begin
					if TObject(_vss.items[J]) is TSection then
					begin
						_v := TSection(_vss.items[J]);
						_blocks := _v.GetSect('exposed_blocks', False);
						
						if _blocks <> nil then
						begin
							for K := 0 to _blocks.items.Count - 1 do
							begin
								if TObject(_blocks.items[K]) is TSection then
								begin
									_block := TSection(_blocks.items[K]);
									_clsid := _block.GetParam('clsid', 'stringz') as TStringValue;
									
									_block.Replace(_clsid, TIntegerValue.Create('clsid', 'u32', GetStringCrc(_clsid.str)));
								end;
							end;
						end;
					end;
				end;
			end;
			
			_vss := e.GetSect('removed_vs', False);
			if _vss <> nil then
			begin
				for J := 1 to _vss.items.Count - 1 do
				begin
					if TObject(_vss.items[J]) is TSection then
					begin
						_v := TSection(_vss.items[J]);
						_blocks := _v.GetSect('exposed_blocks', False);
					
						if _blocks <> nil then
						begin
							for K := 0 to _blocks.items.Count - 1 do
							begin
								if TObject(_blocks.items[K]) is TSection then
								begin
									_block := TSection(_blocks.items[K]);
									_clsid := _block.GetParam('clsid', 'stringz') as TStringValue;
									
									_block.Replace(_clsid, TIntegerValue.Create('clsid', 'u32', GetStringCrc(_clsid.str)));
								end;
							end;
						end;
					end;
				end;
			end;
			
		end;
	end;
end;

// Metro 2033

function LoadLevelBin2033(const fn : String) : TTextKonfig;
var
	infile : TFileStream;
	temp1, temp2 : array of byte;
	magic, size1, size2 : Longword;

	mem : TMemoryStream;
	K : TKonfig;
	TK : TTextKonfig;
	js : TFramework;
begin
	TK := nil;

	infile := TFileStream.Create(fn, fmOpenRead);
	infile.Read(magic, Sizeof(magic));
	if magic = LEVEL_BIN_MAGIC then // ASCII levl
	begin
		infile.Read(size1, Sizeof(size1));
		SetLength(temp1, size1);
		size2 := infile.Size - 8;
		SetLength(temp2, size2);
		infile.Read(temp2[0], size2);
		//Writeln('Decompressing...');
		m2033unp.decompress(@temp2[0], @temp1[0], size2);

		mem := TMemoryStream.Create;
		mem.Write(temp1[0], size1);
		mem.Seek(0, soBeginning);
//		mem.SaveToFile('aaa.bin');
		K := TKonfig.Create;
		if K.Load(mem) then
		begin
			//TK := TTextKonfig.Create;
			//K.Decompile(TK);
      
			js := TFramework.Create;
			TK := js.DecompileKonfig(K, 'js\2033\levelbin.js');
			js.Free;
		end;
		K.Free;
		mem.Free;
	end;

	infile.Free;
	Result := TK;
end;

procedure SaveLevelBin2033(const fn : String; tk : TTextKonfig; kind : Integer);
var
	outfile : TFileStream;
	mem : TMemoryStream;
	K : TKonfig;

	magic : Longword;
	flags : Byte;
	len : Longword;

	blocksize, remain, blocksizeplus9, remainplus9 : Longword;
	blocks : Longword;
	buf : array[0..$1FFFF] of Byte;
begin
	K := TKonfig.Create;
	K.kind := kind;
	K.Compile(tk);

	mem := TMemoryStream.Create;
	K.Save(mem);
	K.Free;

	mem.Seek(0, soBeginning);

//	mem := TMemoryStream.Create;
//	mem.LoadFromFile('level.cfg.bin');

	outfile := TFileStream.Create(fn, fmCreate);
	magic := LEVEL_BIN_MAGIC;
	flags := 2;
	len := mem.Size;
	outfile.Write(magic, Sizeof(magic));
	outfile.Write(len, Sizeof(len));

	blocksize := $020000;
	blocksizeplus9 := blocksize + 9;
	blocks := len div blocksize;
	remain := len mod blocksize;
	remainplus9 := remain + 9;
	while blocks > 0 do
	begin
		outfile.Write(flags, Sizeof(flags));
		outfile.Write(blocksizeplus9, Sizeof(blocksizeplus9));
		outfile.Write(blocksize, Sizeof(blocksize));
		mem.Read(buf[0], blocksize);
		outfile.Write(buf[0], blocksize);
		Dec(blocks);
	end;

	outfile.Write(flags, Sizeof(flags));
	outfile.Write(remainplus9, Sizeof(remainplus9));
	outfile.Write(remain, Sizeof(remain));
	mem.Read(buf[0], remain);
	outfile.Write(buf[0], remain);

	outfile.Free;
	mem.Free;
end;

// end Metro 2033

// Metro Last Light
function LoadLevelBinLL(const fn : String; build_15_10_2012 : Boolean) : TTextKonfig;
var
	r : TMemoryReader;
	K : TKonfig;
	js : TFramework;
	
	magic, size : Longword;
begin
	r := TMemoryReader.CreateFromFile(fn);

	magic := r.ReadLongword;
	size := r.ReadLongword;

	if (magic = LEVEL_BIN_MAGIC) and (size = r.size-8) then
	begin
		K := TKonfig.Create;
		K.Load(r);

		js := TFramework.Create;
		
		js.DefineGlobal('g_build_15_10_2012', build_15_10_2012);
		Result := js.DecompileKonfig(K, 'js\levelbin.js');
		
		js.Free;		

	end else
	begin
		WriteLn('"', fn, '" is not level bin file!');
		Result := nil;
	end;
end;

procedure SaveLevelBinLL(const fn : String; tk : TTextKonfig; kind : Integer);
var
	K : TKonfig;
	C : TTextKonfig;
	w : TMemoryWriter;

	outf : TFileStream;
	magic, size : Longword;
begin
	C := tk.Copy;
	RecoverCrc(C.root);

	K := TKonfig.Create;
	K.kind := kind;
	K.Compile(C, True);

	w := TMemoryWriter.Create;
	K.Save(w);

	outf := TFileStream.Create(fn, fmCreate);
	magic := LEVEL_BIN_MAGIC;
	size := w.size;
	outf.Write(magic, Sizeof(magic));
	outf.Write(size, Sizeof(size));
	outf.Write(w.data[0], w.size);
	outf.Free;

	w.Free;
	K.Free;
	C.Free;
end;
// end Metro Last Light

// Arktika.1 (and exodus)
function LoadLevelBinA1(const fn : String) : TTextKonfig;
var
	r : TMemoryReader;
	K : TKonfig;
	js : TFramework;
begin
	r := TMemoryReader.CreateFromFile(fn);

	if r.ReadLongword = LEVEL_BIN_MAGIC then
	begin
		K := TKonfig.Create;
		K.Load(r);

		js := TFramework.Create;
		Result := js.DecompileKonfig(K, 'js\levelbin.js');
		js.Free;	

	end else
	begin
		WriteLn('"', fn, '" is not level bin file!');
		Result := nil;
	end;
end;

procedure SaveLevelBinA1(const fn : String; tk : TTextKonfig; kind : Integer);
var
	K : TKonfig;
	C : TTextKonfig;

	outf : TFileStream;
	magic : Longword;
begin
	C := tk.Copy;
	RecoverCrc(C.root);
	
	K := TKonfig.Create;
	K.kind := kind;
	K.Compile(C, True);

	outf := TFileStream.Create(fn, fmCreate);
	magic := LEVEL_BIN_MAGIC;
	outf.Write(magic, Sizeof(magic));
	K.Save(outf);
	outf.Free;

	K.Free;
	C.Free;
end;
// end Arktika.1 (and exodus)

end.