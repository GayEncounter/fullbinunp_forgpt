unit SGIImage;

interface

procedure SaveSGI(const n_out : String; w, h : Longint; const pixels : array of Longword);

implementation
uses classes, sysutils;

const
	SGI_SIGN = $1DA;
	
	SGI_DIM_ONELINE_MONOCHROME   = $1;
	SGI_DIM_MULTILINE_MONOCHROME = $2;
	SGI_DIM_MULTILINE_RGB        = $3;
	
	SGI_CMAP_NONE       = $0;
	SGI_CMAP_PACKED_RGB = $1;
	SGI_CMAP_INDEXED    = $2;
	SGI_CMAP_PALETTE    = $3;
	
type	
	TSGIHeader = packed record
		sign       : Word;
		rle        : Boolean;
		bpp        : Byte;
		dimension  : Word;
		width      : Word;
		height     : Word;
		channels   : Word;
		min_value  : Longword;
		max_value  : Longword;
		unused1    : Longword;
		image_name : array[0..79] of AnsiChar;
		cmap_type  : Longword;
		unused2    : array[0..403] of Byte;
	end;
	
procedure SaveSGI(const n_out : String; w, h : Longint; const pixels : array of Longword);
var
	f_out : TFileStream;
	I, J : Longint;
	
	sgi : TSGIHeader;
	pixel : Longword;
	p_red, p_green, p_blue, p_alpha : array of Byte; // planes (for SGI format)
	image_name : String;
begin
	// save SGI file
	f_out := TFileStream.Create(n_out, fmCreate);
	
	FillChar(sgi, Sizeof(sgi), #0);
	sgi.sign := NtoBE(SGI_SIGN);
	sgi.bpp := 1;
	sgi.dimension := NtoBE(SGI_DIM_MULTILINE_RGB);
	sgi.width := NtoBE(Word(w));
	sgi.height := NtoBE(Word(h));
	sgi.channels := NtoBE(4);
	sgi.min_value := NtoBE(Longword($00000000));
	sgi.max_value := NtoBE(Longword($000000FF));
	image_name := Copy(ExtractFileName(n_out), 1, 79);
	sgi.cmap_type := NtoBE(Longword(SGI_CMAP_NONE));
	Move(image_name[1], sgi.image_name, Length(image_name));
	
	f_out.WriteBuffer(sgi, Sizeof(sgi));
	
	SetLength(p_red, w*h);
	SetLength(p_green, w*h);
	SetLength(p_blue, w*h);
	SetLength(p_alpha, w*h);
	
	for I := 1 to h do
		for J := 0 to w-1 do
		begin
			pixel := pixels[(h-I)*w+J];
			p_red  [(I-1)*w+J] := (pixel and $000000FF);
			p_green[(I-1)*w+J] := (pixel and $0000FF00) shr 8;
			p_blue [(I-1)*w+J] := (pixel and $00FF0000) shr 16;
			p_alpha[(I-1)*w+J] := (pixel and $FF000000) shr 24;
		end;
			
	f_out.WriteBuffer(p_red[0], w*h);
	f_out.WriteBuffer(p_green[0], w*h);
	f_out.WriteBuffer(p_blue[0], w*h);
	f_out.WriteBuffer(p_alpha[0], w*h);
	
	f_out.Free;
end; 

end.