unit TextureImport;

interface
uses texturePrefs, crnlib;

procedure ImportTexture(const src_dds, dst_name : String; options : PTextureParams);
procedure ImportTextureLL(const src_dds, dst_name : String; options : PTextureParams; progress_func : crn_progress_callback_func; progress_userdata : Pointer);

procedure MakeDDS_2033(const tex_name : String);
procedure MakeStreamable_2033(const src_dds, dst_name : String);

procedure MakeDDS_LL(const tex_name : String);
procedure MakeStreamable_LL(const src_dds, dst_name : String; var dxt1a : Boolean; progress_func : crn_progress_callback_func; progress_userdata : Pointer);

implementation
uses sysutils, classes, chunkedFile, FileApi, ImageLibraryOptions;

const
  DDPF_ALPHAPIXELS  = $1;
  DDPF_FOURCC       = $4;
  DDPF_RGB          = $40;

  FOURCC_DXT1       = $31545844;
  FOURCC_DXT3       = $33545844;
  FOURCC_DXT5       = $35545844;
  FOURCC_DX10       = $30315844; { ASCII DX10 }

  DDSD_CAPS         = $1;
  DDSD_HEIGHT       = $2;
  DDSD_WIDTH        = $4;
  DDSD_PIXELFORMAT  = $1000;
  DDSD_MIPMAPCOUNT  = $20000;
  DDSD_LINEARSIZE   = $80000;

  DDSCAPS_COMPLEX   = $8;
  DDSCAPS_TEXTURE   = $1000;
  DDSCAPS_MIPMAP    = $400000;

  DDSCAPS2_CUBEMAP  = $200;
  DDSCAPS2_CUBEMAP_POSITIVEX = $400;
  DDSCAPS2_CUBEMAP_NEGATIVEX = $800;
  DDSCAPS2_CUBEMAP_POSITIVEY = $1000;
  DDSCAPS2_CUBEMAP_NEGATIVEY = $2000;
  DDSCAPS2_CUBEMAP_POSITIVEZ = $4000;
  DDSCAPS2_CUBEMAP_NEGATIVEZ = $8000;

  DXGI_FORMAT_BC7_TYPELESS    = 97;
  DXGI_FORMAT_BC7_UNORM       = 98;
  DXGI_FORMAT_BC7_UNORM_SRGB  = 99;

  DDS_DIMENSION_TEXTURE2D = 3;
  DDS_ALPHA_MODE_STRAIGHT = 1;
type
  TDDSPixelFormat = record
    size,
    flags,
    fourcc,
    rgbbitcount,
    redmask,
    greenmask,
    bluemask,
    alphamask : Longword;
  end;

  TDDSHeader = record
    size,
    flags,
    height,
    width,
    pitchorlinearsize,
    depth,
    mipmapcount : Longword;
    reserved1 : array[1..11] of Longword;
    pf : TDDSPixelFormat;
    caps : array[1..4] of Longword;
    reserved2 : Longword;
  end;

  TDDSHeaderDX10 = record
    dxgiformat,
    resourcedimension,
    miscflag,
    arraysize,
    miscflags2 : Longword;
  end;
  
function IsPow2(size : Longword) : Boolean;
begin
	IsPow2 := (size = 512) or (size = 1024) or (size = 2048);
end;

function MipSize(res : Longword; fmt : Longword) : Longword;
begin
	Result := 0;
	if fmt = FOURCC_DXT1 then
		Result := (res div 4) * (res div 4) * 8;
	if fmt = FOURCC_DXT5 then
		Result := (res div 4) * (res div 4) * 16;
end;

function MipCount(res : Longword) : Longword;
begin
	case res of
		512: MipCount := 10;
		1024: MipCount := 11;
		2048: MipCount := 12;
		else MipCount := 0;
	end;
end;

function IsDXT1A(r : TMemoryReader) : Boolean;
var
	c1, c2 : Word;
	bits : Longword;
	I : Longint;
begin
	IsDXT1A := False;
	while r.More do
	begin
		c1 := r.ReadWord;
		c2 := r.ReadWord;
		bits := r.ReadLongword;
		
		if c1 <= c2 then
			for I := 0 to 15 do
				if ((bits shr (I*2)) and 3) = 3 then
				begin
					IsDXT1A := True;
					Exit;
				end;
	end;
end;

procedure ImportTexture(const src_dds, dst_name : String; options : PTextureParams);
var
	r : TMemoryReader;
	magic : String[4];
	hdr : TDDSHeader;
begin
	r := TMemoryReader.CreateFromFile(src_dds);
	
	try
		SetLength(magic, 4);
		r.Read(magic[1], 4);
		if magic <> 'DDS ' then
			raise Exception.Create('Invalid DDS file!');
			
		r.Read(hdr, Sizeof(hdr));
		if hdr.size <> Sizeof(hdr) then
			raise Exception.Create('Invalid DDS file!');
			
		if 
			options.streamable and 
			((hdr.caps[2] and DDSCAPS2_CUBEMAP) = 0) and
			(hdr.width = hdr.height) and IsPow2(hdr.width) and (hdr.mipmapcount = MipCount(hdr.width)) and
			((hdr.pf.flags and DDPF_FOURCC) <> 0) and ((hdr.pf.fourcc = FOURCC_DXT1) or (hdr.pf.fourcc = FOURCC_DXT5)) then
		begin
			MakeStreamable_2033(src_dds, dst_name);
		end else
		begin
			options.streamable := False;
			CopyFile(src_dds, dst_name + '.dds', False);
		end;
		
		options.r_width := hdr.width;
		options.r_height := hdr.height;
		case hdr.pf.fourcc of
			FOURCC_DXT1: options.fmt := 0;
			FOURCC_DXT5: options.fmt := 1;
			else options.fmt := 2;
		end;
		
		if (hdr.caps[2] and DDSCAPS2_CUBEMAP) <> 0 then options.ttype := 3;
	finally
		r.Free;
	end;
end;

procedure ImportTextureLL(const src_dds, dst_name : String; options : PTextureParams; progress_func : crn_progress_callback_func; progress_userdata : Pointer);
var
	r : TMemoryReader;
	magic : String[4];
	hdr : TDDSHeader;
	dxt1a : Boolean;
begin
	r := TMemoryReader.CreateFromFile(src_dds);
	
	try
		SetLength(magic, 4);
		r.Read(magic[1], 4);
		if magic <> 'DDS ' then
			raise Exception.Create('Invalid DDS file!');
			
		r.Read(hdr, Sizeof(hdr));
		if hdr.size <> Sizeof(hdr) then
			raise Exception.Create('Invalid DDS file!');

		dxt1a := False;
			
		if 
			options.streamable and 
			((hdr.caps[2] and DDSCAPS2_CUBEMAP) = 0) and
			(hdr.width = hdr.height) and IsPow2(hdr.width) and (hdr.mipmapcount = MipCount(hdr.width)) and
			((hdr.pf.flags and DDPF_FOURCC) <> 0) and ((hdr.pf.fourcc = FOURCC_DXT1) or (hdr.pf.fourcc = FOURCC_DXT5)) then
		begin
			MakeStreamable_LL(src_dds, dst_name, dxt1a, progress_func, progress_userdata);
		end else
		begin
			options.streamable := False;
			CopyFile(src_dds, dst_name + '.dds', False);
		end;
		
		options.r_width := hdr.width;
		options.r_height := hdr.height;
		case hdr.pf.fourcc of
			FOURCC_DXT1: options.fmt := 0;
			FOURCC_DXT5: options.fmt := 1;
			else options.fmt := 2;
		end;
		
		if dxt1a then options.fmt := 1;
		if (hdr.caps[2] and DDSCAPS2_CUBEMAP) <> 0 then options.ttype := 3;
	finally
		r.Free;
	end;
end;

// no mips
function FormatBySize(res, size : Longword) : Longword;
begin
	if MipSize(res, FOURCC_DXT1) = size then
		FormatBySize := FOURCC_DXT1
	else if MipSize(res, FOURCC_DXT5) = size then
		FormatBySize := FOURCC_DXT5
	else
		FormatBySize := 0
end;

// w/ mips
function FormatBySize512(size : Longword) : Longword;
const
  DXT1_512_SIZE = 174776;
  DXT5_512_SIZE = 349552;
begin
	if DXT1_512_SIZE = size then
		FormatBySize512 := FOURCC_DXT1
	else if DXT5_512_SIZE = size then
		FormatBySize512 := FOURCC_DXT5
	else
		FormatBySize512 := 0
end;

procedure MakeDDS_2033(const tex_name : String);
var
	r512, r1024, r2048 : TMemoryReader;
	fmt : Longword;
	res : Longword;
	
	magic : String[4] = 'DDS ';
	hdr : TDDSHeader;
	
	fdds : TFileStream;
begin
	r512 := nil;
	r1024 := nil;
	r2048 := nil;
	fdds := nil;
	
	try
		if FileExists(tex_name + '.2048') then
			r2048 := TMemoryReader.CreateFromFile(tex_name + '.2048');
		if FileExists(tex_name + '.1024') then
			r1024 := TMemoryReader.CreateFromFile(tex_name + '.1024');
		
		r512 := TMemoryReader.CreateFromFile(tex_name + '.512');
		
		fmt := FormatBySize512(r512.size);
		
		if fmt = 0 then
			raise Exception.Create('Unknown .512 file pixelformat!');
		if Assigned(r1024) and (fmt <> FormatBySize(1024, r1024.size)) then
			raise Exception.Create('Pixelformat of .1024 file doesn''t match to .512');
		if Assigned(r2048) and (fmt <> FormatBySize(2048, r2048.size)) then
			raise Exception.Create('Pixelformat of .2048 file doesn''t match to .512');
			
		if Assigned(r2048) then
			res := 2048
		else if Assigned(r1024) then
			res := 1024
		else
			res := 512;
			
		FillChar(hdr, Sizeof(hdr), #0);
		hdr.size := Sizeof(hdr);
		hdr.flags := DDSD_CAPS or DDSD_WIDTH or DDSD_HEIGHT or DDSD_PIXELFORMAT or DDSD_LINEARSIZE or DDSD_MIPMAPCOUNT;
		hdr.width := res;
		hdr.height := res;
		hdr.pitchorlinearsize := MipSize(res, fmt);
		hdr.mipmapcount := MipCount(res);
		hdr.pf.size := Sizeof(hdr.pf);
		hdr.pf.flags := DDPF_FOURCC;
		hdr.pf.fourcc := fmt;
		hdr.caps[1] := DDSCAPS_TEXTURE or DDSCAPS_COMPLEX or DDSCAPS_MIPMAP;
		
		fdds := TFileStream.Create(tex_name + '.dds', fmCreate);
		
		fdds.Write(magic[1], Length(magic));
		fdds.Write(hdr, Sizeof(hdr));
		
		if res >= 2048 then
			fdds.Write(r2048.data^, r2048.size);
		if res >= 1024 then
			fdds.Write(r1024.data^, r1024.size);
		
		fdds.Write(r512.data^, r512.size);
		
	finally
		r2048.Free;
		r1024.Free;
		r512.Free;
		fdds.Free;
	end;
end;

procedure MakeStreamable_2033(const src_dds, dst_name : String);
var
	r : TMemoryReader;
	f : TFileStream;

	magic : String[4];
	hdr : TDDSHeader;
begin
	f := nil;
	r := TMemoryReader.CreateFromFile(src_dds);
	
	try
		SetLength(magic, 4);
		r.Read(magic[1], 4);
		if magic <> 'DDS ' then
			raise Exception.Create('Invalid DDS file!');
			
		r.Read(hdr, Sizeof(hdr));
		if hdr.size <> Sizeof(hdr) then
			raise Exception.Create('Invalid DDS file!');
			
		if 
			((hdr.caps[2] and DDSCAPS2_CUBEMAP) = 0) and
			(hdr.width = hdr.height) and IsPow2(hdr.width) and (hdr.mipmapcount = MipCount(hdr.width)) and
			((hdr.pf.flags and DDPF_FOURCC) <> 0) and ((hdr.pf.fourcc = FOURCC_DXT1) or (hdr.pf.fourcc = FOURCC_DXT5)) then
		begin
			if hdr.width >= 2048 then
			begin
				f := TFileStream.Create(dst_name + '.2048', fmCreate);
				f.Write((r.data+r.pos)^, MipSize(2048, hdr.pf.fourcc));
				Inc(r.pos, MipSize(2048, hdr.pf.fourcc));
				FreeAndNil(f);
			end;
			
			if hdr.width >= 1024 then
			begin
				f := TFileStream.Create(dst_name + '.1024', fmCreate);
				f.Write((r.data+r.pos)^, MipSize(1024, hdr.pf.fourcc));
				Inc(r.pos, MipSize(1024, hdr.pf.fourcc));
				FreeAndNil(f);
			end;
			
			f := TFileStream.Create(dst_name + '.512', fmCreate);
			f.Write((r.data+r.pos)^, r.size-r.pos);
			FreeAndNil(f);	
		end else
			raise Exception.Create('This texture not acceptable for streaming (cubemap, non-square 512/1024/2048, not DXT1 or DXT5, without mipmaps)');
	finally
		r.Free;
		f.Free;
	end;
end;

procedure MakeDDS_LL(const tex_name : String);
var
	r512c, r1024c : TMemoryReader;
	data512, data1024 : Pointer;
	size512, size1024 : Longword;
	
	r512, r1024, r2048 : TMemoryReader;
	fmt : Longword;
	res : Longword;
	
	magic : String[4] = 'DDS ';
	hdr : TDDSHeader;
	
	fdds : TFileStream;
begin
	data512 := nil;
	data1024 := nil;

	try
		r512c := nil;
		r1024c := nil; 
		
		try
			if FileExists(tex_name + '.1024c') then
			begin
				r1024c := TMemoryReader.CreateFromFile(tex_name + '.1024c');
				size1024 := r1024c.size;
				data1024 := crn_decompress_crn_to_dds(r1024c.data, @size1024);
				if data1024 = nil then
					raise Exception.Create('crn_decompress_crn_to_dds failed!');
			end;
				
			r512c := TMemoryReader.CreateFromFile(tex_name + '.512c');
			size512 := r512c.size;
			data512 := crn_decompress_crn_to_dds(r512c.data, @size512);
			if data512 = nil then
				raise Exception.Create('crn_decompress_crn_to_dds failed!');
		finally
			r512c.Free;
			r1024c.Free;
		end;
	
		r512 := nil;
		r1024 := nil;
		r2048 := nil;
		fdds := nil;
		
		try
			if FileExists(tex_name + '.2048') then
				r2048 := TMemoryReader.CreateFromFile(tex_name + '.2048');
			if data1024 <> nil then
				r1024 := TMemoryReader.Create((data1024 + $80)^, size1024 - $80);
			
			r512 := TMemoryReader.Create((data512 + $80)^, size512 - $80);
			
			fmt := FormatBySize512(r512.size);
			
			if fmt = 0 then
				raise Exception.Create('Unknown .512 file pixelformat!');
			if Assigned(r1024) and (fmt <> FormatBySize(1024, r1024.size)) then
				raise Exception.Create('Pixelformat of .1024 file doesn''t match to .512');
			if Assigned(r2048) and (fmt <> FormatBySize(2048, r2048.size)) then
				raise Exception.Create('Pixelformat of .2048 file doesn''t match to .512');
				
			if Assigned(r2048) then
				res := 2048
			else if Assigned(r1024) then
				res := 1024
			else
				res := 512;
				
			FillChar(hdr, Sizeof(hdr), #0);
			hdr.size := Sizeof(hdr);
			hdr.flags := DDSD_CAPS or DDSD_WIDTH or DDSD_HEIGHT or DDSD_PIXELFORMAT or DDSD_LINEARSIZE or DDSD_MIPMAPCOUNT;
			hdr.width := res;
			hdr.height := res;
			hdr.pitchorlinearsize := MipSize(res, fmt);
			hdr.mipmapcount := MipCount(res);
			hdr.pf.size := Sizeof(hdr.pf);
			hdr.pf.flags := DDPF_FOURCC;
			hdr.pf.fourcc := fmt;
			hdr.caps[1] := DDSCAPS_TEXTURE or DDSCAPS_COMPLEX or DDSCAPS_MIPMAP;
			
			fdds := TFileStream.Create(tex_name + '.dds', fmCreate);
			
			fdds.Write(magic[1], Length(magic));
			fdds.Write(hdr, Sizeof(hdr));
			
			if res >= 2048 then
				fdds.Write(r2048.data^, r2048.size);
			if res >= 1024 then
				fdds.Write(r1024.data^, r1024.size);
			
			fdds.Write(r512.data^, r512.size);
			
		finally
			r2048.Free;
			r1024.Free;
			r512.Free;
			fdds.Free;
		end;
	finally
		if data512 <> nil then crn_free_block(data512);
		if data1024 <> nil then crn_free_block(data1024);
	end;
end;

procedure MakeStreamable_LL(const src_dds, dst_name : String; var dxt1a : Boolean; progress_func : crn_progress_callback_func; progress_userdata : Pointer);
var
	I, J, M : Longint; // M - index of current mipmap in 'image' array

	r : TMemoryReader;
	f : TFileStream;

	magic : String[4];
	hdr : TDDSHeader;
	
	slice : TMemoryReader;
	
	image : array[0..63] of PLongword;
	image_desc : crn_texture_desc;
	
	crn : crn_comp_params;
	
	data : Pointer;
	data_size : Longword;
begin
	FillChar(image, Sizeof(image), #0);

	f := nil;
	r := TMemoryReader.CreateFromFile(src_dds);
	
	try
		SetLength(magic, 4);
		r.Read(magic[1], 4);
		if magic <> 'DDS ' then
			raise Exception.Create('Invalid DDS file!');
			
		r.Read(hdr, Sizeof(hdr));
		if hdr.size <> Sizeof(hdr) then
			raise Exception.Create('Invalid DDS file!');

		dxt1a := False;
			
		if 
			((hdr.caps[2] and DDSCAPS2_CUBEMAP) = 0) and
			(hdr.width = hdr.height) and IsPow2(hdr.width) and (hdr.mipmapcount = MipCount(hdr.width)) and
			((hdr.pf.flags and DDPF_FOURCC) <> 0) and ((hdr.pf.fourcc = FOURCC_DXT1) or (hdr.pf.fourcc = FOURCC_DXT5)) then
		begin
			M := 0;
		
			if crn_decompress_dds_to_images(r.data, r.size, @image, image_desc) = False then
				raise Exception.Create('crn_decompress_dds_to_images failed!');
		
			// если текстура имеет формат DXT1A то её необходимо преобразовать в DXT5
			// так как crunch не может сжимать DXT1A, и следующие мипмапы сохранит как DXT5
			if hdr.pf.fourcc = FOURCC_DXT1 then
			begin
				slice := TMemoryReader.CreateSlice(r, r.pos, MipSize(hdr.width, hdr.pf.fourcc));
				dxt1a := IsDXT1A(slice);
				slice.Free;
			end;
		
			if hdr.width >= 2048 then
			begin
				if dxt1a then
				begin
					crn.Clear;
					crn.m_file_type := cCRNFileTypeDDS;
					crn.m_width := 2048;
					crn.m_height := 2048;
					crn.m_format := cCRNFmtDXT5;
					crn.m_pImages[0,0] := image[M];
					crn.m_quality_level := CrunchTextureQuality;
					crn.m_pProgress_func := progress_func;
					crn.m_pProgress_func_data := progress_userdata;
					
					data := crn_compress(crn, data_size);
					if data = nil then
						raise Exception.Create('crn_compress failed (2048)');
					
					try	
						f := TFileStream.Create(dst_name + '.2048', fmCreate);
						f.Write((data + $80)^, MipSize(2048, hdr.pf.fourcc));
						FreeAndNil(f);
					finally
						crn_free_block(data);
					end;
				end else
				begin
					f := TFileStream.Create(dst_name + '.2048', fmCreate);
					f.Write((r.data+r.pos)^, MipSize(2048, hdr.pf.fourcc));
					FreeAndNil(f);
				end;
				
				// в любом случае переходим к следующему мипмапу
				Inc(r.pos, MipSize(2048, hdr.pf.fourcc));
				Inc(M);
			end;
			
			if hdr.width >= 1024 then
			begin
				crn.Clear;
				crn.m_file_type := cCRNFileTypeCRN;
				crn.m_width := 1024;
				crn.m_height := 1024;
				
				if dxt1a or (hdr.pf.fourcc = FOURCC_DXT5) then
					crn.m_format := cCRNFmtDXT5
				else
					crn.m_format := cCRNFmtDXT1;
				
				crn.m_pImages[0,0] := image[M];
				crn.m_quality_level := CrunchTextureQuality;
				crn.m_pProgress_func := progress_func;
				crn.m_pProgress_func_data := progress_userdata;
				
				data := crn_compress(crn, data_size);
				if data = nil then
					raise Exception.Create('crn_compress failed (1024)');
				
				try	
					f := TFileStream.Create(dst_name + '.1024c', fmCreate);
					f.Write(data^, data_size);
					FreeAndNil(f);
				finally
					crn_free_block(data);
				end;
				
				Inc(M);
			end;
			
			crn.Clear;
			crn.m_file_type := cCRNFileTypeCRN;
			crn.m_width := 512;
			crn.m_height := 512;
			crn.m_levels := 10;
			
			if dxt1a or (hdr.pf.fourcc = FOURCC_DXT5) then
				crn.m_format := cCRNFmtDXT5
			else
				crn.m_format := cCRNFmtDXT1;
			
			for J := 0 to 9 do
			begin
				crn.m_pImages[0,J] := image[M];
				Inc(M);
			end;
			
			crn.m_quality_level := CrunchTextureQuality;
			crn.m_pProgress_func := progress_func;
			crn.m_pProgress_func_data := progress_userdata;
			
			data := crn_compress(crn, data_size);
			if data = nil then
				raise Exception.Create('crn_compress failed (512)');
			
			try	
				f := TFileStream.Create(dst_name + '.512c', fmCreate);
				f.Write(data^, data_size);
				FreeAndNil(f);
			finally
				crn_free_block(data);
			end;
		end else
			raise Exception.Create('This texture not acceptable for streaming (cubemap, non-square 512/1024/2048, not DXT1 or DXT5, without mipmaps)');
	finally
		r.Free;
		f.Free;
		
		for I := 0 to Length(image)-1 do
			if image[I] <> nil then
				crn_free_block(image[I]);
	end;
end;

end.