unit Texture;

interface
uses texturePrefs;

var
	textureQuality : Integer = 512;
	texturesCompressed : Boolean = False;  // hack to support both 2033 and exodus
	
	texture_params : TTexturesBin;
	texture_params2 : TTexturesBin2;
	texture_aliases : TTextureAliases;

function LoadTexture(const path : String) : Longword;
function GetRealTextureName(const name : String) : String;

implementation
uses classes, sysutils, GL, GLExt, XBox360Swizzle;

function LZ4_decompress_fast(src: Pointer; dst: Pointer; originalsize: Longint): Longint;
cdecl; external 'liblz4.so.1.8.3.dll';

{$IFDEF WIN64}
const
	crn_free_block_Proc = '?crn_free_block@@YAXPEAX@Z';
	crn_decompress_crn_to_dds_Proc = '?crn_decompress_crn_to_dds@@YAPEAXPEBXAEAI@Z';
{$ELSE}
const
	crn_free_block_Proc = '?crn_free_block@@YAXPAX@Z';
	crn_decompress_crn_to_dds_Proc = '?crn_decompress_crn_to_dds@@YAPAXPBXAAI@Z';
{$ENDIF}

procedure crn_free_block(block : Pointer); cdecl; 
external 'crnlib.dll' name crn_free_block_Proc;

function  crn_decompress_crn_to_dds(data : Pointer; file_size : PLongint) : Pointer; cdecl; 
external 'crnlib.dll' name crn_decompress_crn_to_dds_Proc;

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
	
	DDSCAPS_COMPLEX   = $8;
	DDSCAPS_TEXTURE   = $1000;
	
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

type
	TByteArray = array of Byte;

function ImageSize(format : Longword; width, height : Longword) : Longword;
	function Max(a, b : Longword) : Longword;
	begin
		if a > b then
			Max := a
		else
			Max := b;
	end;
begin
	case format of
		GL_RGB, GL_BGR:
			ImageSize := width * height * 3;
		GL_RGBA, GL_BGRA:
			ImageSize := width * height * 4;
		GL_COMPRESSED_RGB_S3TC_DXT1_EXT, GL_COMPRESSED_RGBA_S3TC_DXT1_EXT:
			ImageSize := Max(width div 4, 1) * Max(height div 4, 1) * 8;
		GL_COMPRESSED_RGBA_S3TC_DXT3_EXT, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,
		GL_COMPRESSED_RGBA_BPTC_UNORM_ARB:
			ImageSize := Max(width div 4, 1) * Max(height div 4, 1) * 16;
		else
			raise Exception.Create('Unknown image format!');
	end;
end;

procedure LoadDDS(const path : String; texture : GLuint);
var
	sign : string[4];
	h : TDDSHeader;
	
	format, internalformat, datatype : GLenum;
	mipmaps : Integer;
	compressed : Boolean;
	
	f : TFileStream;
	
	procedure Error;
	begin
		raise Exception.Create('Invalid DDS file: ' + path);
	end;
	
	procedure Unsupp;
	begin
		raise Exception.Create('Unsupported DDS pixelformat, file: ' + path);
	end;
	
	function FormatIs(bpp, rm, gm, bm, am : Longword) : Boolean;
	begin
		Result :=
		(h.pf.rgbbitcount = bpp) and
		(h.pf.redmask = rm) and
		(h.pf.greenmask = gm) and
		(h.pf.bluemask = bm) and
		(h.pf.alphamask = am);
	end;
	
	procedure LoadImage(target : GLenum);
	var
		level, width, height: Longint;
		buffer : array of Byte;
	begin
		width := h.width;
		height := h.height;
		for level := 0 to mipmaps-1 do
		begin
			SetLength(buffer, ImageSize(format, width, height));
			f.ReadBuffer(buffer[0], Length(buffer));
			//Writeln('loadingloading');
			if compressed then
				glCompressedTexImage2D(target, level, format, width, height, 0,
				Length(buffer), Pointer(buffer))
			else
				glTexImage2D(target, level, internalformat, width, height, 0, format,
				datatype, Pointer(buffer));
			
			width := width div 2; if width = 0 then width := 1;
			height := height div 2; if height = 0 then height := 1;
		end;
	end;
begin
	f := TFileStream.Create(path, fmOpenRead);
	try
		SetLength(sign, 4);
		f.ReadBuffer(sign[1], 4);
		if sign <> 'DDS ' then Error;
	
		f.ReadBuffer(h, Sizeof(h));
		if h.size <> Sizeof(h) then Error;
	
		if h.pf.flags and DDPF_FOURCC <> 0 then
		begin
			compressed := True;
	
			case h.pf.fourcc of
				FOURCC_DXT1:
					if h.pf.rgbbitcount = 32 then
						format := GL_COMPRESSED_RGB_S3TC_DXT1_EXT
					else
						format := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
				FOURCC_DXT3:
					format := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
				FOURCC_DXT5:
					format := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
				else
					Unsupp;
			end;
		end else
		begin
			compressed := False;
		
			if h.pf.flags and DDPF_RGB = 0 then
				Unsupp;
		
			if h.pf.flags and DDPF_ALPHAPIXELS <> 0 then
			begin
				internalformat := GL_RGBA;
			
				if FormatIs(32, $00FF0000, $0000FF00, $000000FF, $FF000000) then
				begin
					format := GL_BGRA;
					datatype := GL_UNSIGNED_INT_8_8_8_8_REV;
				end else
				if FormatIs(32, $000000FF, $0000FF00, $00FF0000, $FF000000) then
				begin
					format := GL_RGBA;
					datatype := GL_UNSIGNED_INT_8_8_8_8_REV;
				end else
					Unsupp;
			end else
			begin
				internalformat := GL_RGB;
			
				if FormatIs(24, $00FF0000, $0000FF00, $000000FF, 0) then
				begin
					format := GL_BGR;
					datatype := GL_UNSIGNED_BYTE;
				end else
				if FormatIs(24, $000000FF, $0000FF00, $00FF0000, 0) then
				begin
					format := GL_RGB;
					datatype := GL_UNSIGNED_BYTE;
				end else
					Unsupp;
			end;
		end;
		
		if (h.flags and DDSD_MIPMAPCOUNT) <> 0 then
			mipmaps := h.mipmapcount
		else
			mipmaps := 1;
	
		if (h.caps[2] and DDSCAPS2_CUBEMAP) <> 0 then
		begin
			glBindTexture(GL_TEXTURE_CUBE_MAP, texture);
			
			if (h.caps[2] and DDSCAPS2_CUBEMAP_POSITIVEX) <> 0 then
				LoadImage(GL_TEXTURE_CUBE_MAP_POSITIVE_X);
			if (h.caps[2] and DDSCAPS2_CUBEMAP_NEGATIVEX) <> 0 then
				LoadImage(GL_TEXTURE_CUBE_MAP_NEGATIVE_X);
			if (h.caps[2] and DDSCAPS2_CUBEMAP_POSITIVEY) <> 0 then
				LoadImage(GL_TEXTURE_CUBE_MAP_POSITIVE_Y);
			if (h.caps[2] and DDSCAPS2_CUBEMAP_NEGATIVEY) <> 0 then
				LoadImage(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y);
			if (h.caps[2] and DDSCAPS2_CUBEMAP_POSITIVEZ) <> 0 then
				LoadImage(GL_TEXTURE_CUBE_MAP_POSITIVE_Z);
			if (h.caps[2] and DDSCAPS2_CUBEMAP_NEGATIVEZ) <> 0 then
				LoadImage(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z);
			
			glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP);
			glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP);
			glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);  
			if mipmaps = 1 then glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
			else glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
			
		end else
		begin
			glBindTexture(GL_TEXTURE_2D, texture);
			LoadImage(GL_TEXTURE_2D);
			
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
			if mipmaps = 1 then glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
			else glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
		end;
	finally
		f.Free;
	end;
end;

function LoadImageRaw(const path : String) : TByteArray;
var
	f : TFileStream;
begin
	Result := nil;

	f := TFileStream.Create(path, fmOpenRead);
	try
		SetLength(Result, f.Size);
		f.ReadBuffer(Result[0], f.Size);
	finally
		f.Free;
	end;
end;

function LoadImageCrunch(const path : String) : TByteArray;
var
	f : TFileStream;
	buffer : array of Byte;
	ptr : Pointer;
	size : Longint;
begin
	Result := nil;
	
	f := TFileStream.Create(path, fmOpenRead);
	try
		SetLength(buffer, f.Size);
		f.ReadBuffer(buffer[0], f.Size);
		
		size := f.Size;
		ptr := crn_decompress_crn_to_dds(@buffer[0], @size);
		
		// skip DDS header
		size := size - (4 + Sizeof(TDDSHeader));
		SetLength(Result, size);
		Move((ptr + (4 + Sizeof(TDDSHeader)))^, Result[0], size);
		
		crn_free_block(ptr);
	finally
		f.Free;
	end;
end;

procedure LoadRaw(const path : String);
const
	DXT1_512_SIZE = 174776;
	DXT5_512_SIZE = 349552;
var
	level : GLuint;
	f512, f1024, f2048 : TByteArray;
	I, size, offset, len : Integer;
	format : GLenum;
begin
	if FileExists(path + '.512c') then
		f512 := LoadImageCrunch(path + '.512c')
	else
		f512 := LoadImageRaw(path + '.512');

	// TODO: use format from textures.bin, not guess by file size
	case Length(f512) of
		DXT1_512_SIZE:  format := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
		DXT5_512_SIZE:  format := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
		else raise Exception.Create('Invalid .512 file size. Compressed?');
	end;

	level := 0;
	if (textureQuality >= 2048) then
	begin
		if FileExists(path + '.2048c') then
			f2048 := LoadImageCrunch(path + '.2048c')
		else if FileExists(path + '.2048') then
			f2048 := LoadImageRaw(path + '.2048')
		else
			f2048 := nil;

		if Length(f2048) > 0 then
		begin
			if Length(f2048) <> ImageSize(format, 2048, 2048) then
				raise Exception.Create('Invalid .2048 file size');

			glCompressedTexImage2D(GL_TEXTURE_2D, level, format, 2048, 2048, 0, Length(f2048), Pointer(f2048));
			Inc(level);
		end;
  end;

	if (textureQuality >= 1024) then
	begin
		if FileExists(path + '.1024c') then
			f1024 := LoadImageCrunch(path + '.1024c')
		else if FileExists(path + '.1024') then
			f1024 := LoadImageRaw(path + '.1024')
		else
			f1024 := nil;
		
		if Length(f1024) > 0 then
		begin
			if Length(f1024) <> ImageSize(format, 1024, 1024) then
				raise Exception.Create('Invalid .1024 file size');
		
			glCompressedTexImage2D(GL_TEXTURE_2D, level, format, 1024, 1024, 0, Length(f1024), Pointer(f1024));
			Inc(level);
		end;
	end;

	offset := 0;
	size := 512;
	for I := 1 to 10 do
	begin
		len := ImageSize(format, size, size);
		if size <= textureQuality then
		begin
			glCompressedTexImage2D(GL_TEXTURE_2D, level, format, size, size, 0, len, @f512[offset]);
			Inc(level);
		end;
		
		offset := offset + len;
		size := size div 2;
	end;
end;

procedure LoadRawBC7(const path : String);
const
	BC7_512_SIZE  = 349552;
	BC7_1024_SIZE = 1048576;
	BC7_2048_SIZE = 4194304;
var
	f512, f1024, f2048 : TFileStream;
	level : GLuint;
	compressed, uncompressed : array of Byte;
	
	I, size, res, offset : Longint;
begin
	f512 := nil;
	f1024 := nil;
	f2048 := nil;

	try
		if FileExists(path + '.2048') then
			f2048 := TFileStream.Create(path + '.2048', fmOpenRead);
		if FileExists(path + '.1024') then
			f1024 := TFileStream.Create(path + '.1024', fmOpenRead);
		
		f512 := TFileStream.Create(path + '.512', fmOpenRead);
		
		level := 0;
		if (f2048 <> nil) and (textureQuality >= 2048) then
		begin
			SetLength(compressed, f2048.Size);
			SetLength(uncompressed, BC7_2048_SIZE);
			
			f2048.ReadBuffer(compressed[0], f2048.Size);
			size := LZ4_decompress_fast(@compressed[0], @uncompressed[0], BC7_2048_SIZE);
			if size <> Length(compressed) then
				raise Exception.Create('LZ4_decompress_fast failed');
			
			glCompressedTexImage2D(GL_TEXTURE_2D, level, GL_COMPRESSED_RGBA_BPTC_UNORM_ARB,
			2048, 2048, 0, BC7_2048_SIZE, @uncompressed[0]);
			Inc(level);
		end;
		
		if (f1024 <> nil) and (textureQuality >= 1024) then
		begin
			SetLength(compressed, f1024.Size);
			SetLength(uncompressed, BC7_1024_SIZE);
			
			f1024.ReadBuffer(compressed[0], f1024.Size);
			size := LZ4_decompress_fast(@compressed[0], @uncompressed[0], BC7_1024_SIZE);
			if size <> Length(compressed) then
				raise Exception.Create('LZ4_decompress_fast failed');
			
			glCompressedTexImage2D(GL_TEXTURE_2D, level, GL_COMPRESSED_RGBA_BPTC_UNORM_ARB,
			1024, 1024, 0, BC7_1024_SIZE, @uncompressed[0]);
			Inc(level);
		end;
		
		SetLength(compressed, f512.Size);
		SetLength(uncompressed, BC7_512_SIZE);
		
		f512.ReadBuffer(compressed[0], f512.Size);
		size := LZ4_decompress_fast(@compressed[0], @uncompressed[0], BC7_512_SIZE);
		if size <> Length(compressed) then
			raise Exception.Create('LZ4_decompress_fast failed');
		
		res := 512;
		offset := 0;
		for I := 1 to 10 do
		begin
			size := ImageSize(GL_COMPRESSED_RGBA_BPTC_UNORM_ARB, res, res);
		
			if res <= textureQuality then
			begin
				glCompressedTexImage2D(GL_TEXTURE_2D, level,
				GL_COMPRESSED_RGBA_BPTC_UNORM_ARB,
				res, res, 0, size, @uncompressed[offset]);
				Inc(level);
			end;
		
			res := res div 2;
			offset := offset + size;
		end;
	finally
		f512.Free;
		f1024.Free;
		f2048.Free;
	end;
end;

procedure LoadCubemapXbox(const path : String);
const
	CUBE_DIM = 512;
type
	DXT5_BLOCK = array[0..1] of QWord;
var
	I, X, Y, B : Longint;
	f : TFileStream;
	buffer_src : array of DXT5_BLOCK;
	buffer_dst : array of DXT5_BLOCK;
begin
	f := TFileStream.Create(path, fmOpenRead);
	try
		if f.Size <> (ImageSize(GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, CUBE_DIM, CUBE_DIM)*6) then
			raise Exception.Create('Invalid xbox cubemap size: ' + IntToStr(f.Size));
			
		SetLength(buffer_src, (CUBE_DIM div 4)*(CUBE_DIM div 4));
		SetLength(buffer_dst, (CUBE_DIM div 4)*(CUBE_DIM div 4));
		
		for I := 0 to 5 do
		begin
			f.ReadBuffer(buffer_src[0], Length(buffer_src)*Sizeof(DXT5_BLOCK));

			for X := 0 to (CUBE_DIM div 4) - 1 do
			begin
				for Y := 0 to (CUBE_DIM div 4) - 1 do
				begin					
					B := TiledBlockCoord(X, Y, (CUBE_DIM div 4), DXT5_BLOCK_SZ);
					buffer_dst[Y*(CUBE_DIM div 4)+X] := buffer_src[B];
				end;
			end;
			
			glCompressedTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X+I, 0, 
			GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, CUBE_DIM, CUBE_DIM, 0,
			Length(buffer_dst)*Sizeof(DXT5_BLOCK), Pointer(buffer_dst));
		end;
		
		glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP);
		glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP);
		glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP);
		glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
	finally
		f.Free;
	end;
end;

function LoadTexture(const path : String) : Longword;
var
	tex : GLuint;
begin
	glGenTextures(1, @tex);

	Result := 0;

	if FileExists(path + '.dds') then
	begin
		try
			LoadDDS(path + '.dds', tex);
		except
			on E : Exception do
				WriteLn('DDS texture ''', path, ''' loading failed. ', E.ClassName, ': ', E.Message);
		end;
	
		//WriteLn('dds loaded ', path);
	
		Result := tex;
	end else
	if FileExists(path + '.512') or FileExists(path + '.512c') then
	begin
		glBindTexture(GL_TEXTURE_2D, tex);
		//WriteLn('loading texture ''', path, '''');
		// Arktika.1, Exodus
		// how to correctly determine if texture LZ4 compressed?
		try
			if not texturesCompressed then
				LoadRaw(path)
			else
				LoadRawBC7(path);
		except
			on E : Exception do
				WriteLn('texture ''', path, ''' loading failed. ', E.ClassName, ': ', E.Message);
		end;
		
		Result := tex;
	end else
	if FileExists(path + '.xbox') then
	begin
		glBindTexture(GL_TEXTURE_CUBE_MAP, tex);
		LoadCubemapXbox(path + '.xbox');
		
		Result := tex;
	end else
		WriteLn('can''t find texture ''', path, '''');
end;

function GetRealTextureName(const name : String) : String;
var
	file_name : String;
	p : PTextureParams2;
begin
	file_name := name;
	
	if texture_params <> nil then
		texture_params.GetRealName(file_name)
	else
	if texture_aliases <> nil then
		texture_aliases.GetRealName(file_name);

	if texture_params2 <> nil then
	begin
		p := texture_params2[file_name];
		if p <> nil then
			file_name := p^.source_name;
	end;
		
	Result := file_name;
end;

end.
