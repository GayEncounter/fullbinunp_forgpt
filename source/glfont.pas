unit glfont;

interface
uses GL, GLU;

type TGLFont = class
	tex_width, tex_height : Longint;
	cell_width, cell_height : Longint;
	bpp : Byte;
	base : Byte;
	width : array[0..255] of Byte;
	
	texture : GLuint;
	
	constructor Create(const fn : String);
	destructor Destroy; override;
	
	function StringWidth(const str : String) : Longint;
	
	procedure DrawText2D(x, y : Longint; const str : String);
end;

implementation
uses sysutils, chunkedFile;

const
	WIDTH_DATA_OFFSET = 20;
	MAP_DATA_OFFSET 	= 276;

constructor TGLFont.Create(const fn : String);
var
	r : TMemoryReader;
	fmt : GLenum;
begin
	r := TMemoryReader.CreateFromFile(fn);
	try
		// magic
		if r.ReadWord <> $F2BF then
			raise Exception.CreateFmt('Invalid BFF file %s', [fn]);
			
		tex_width := r.ReadLongint;
		tex_height := r.ReadLongint;
		cell_width := r.ReadLongint;
		cell_height := r.ReadLongint;
		bpp := r.ReadByte;
		base := r.ReadByte;
		r.Read(width, SizeOf(width));
		
		if (r.size-r.pos) < (tex_width*tex_height*(bpp div 8)) then
			raise Exception.CreateFmt('Invalid BFF file %s', [fn]);
			
		case bpp of
			8:  
				fmt := GL_LUMINANCE;
			24: 
				fmt := GL_RGB;
			32: 
				fmt := GL_RGBA;
			else
				raise Exception.CreateFmt('Error loading BFF %s, unsupprted BPP %d', [fn, bpp]);
		end;
				
		glGenTextures(1, @texture);
		glBindTexture(GL_TEXTURE_2D, texture);
		
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		//glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		//glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
		
		gluBuild2DMipmaps(GL_TEXTURE_2D, fmt, tex_width, tex_height, fmt, GL_UNSIGNED_BYTE, r.data+r.pos);
	finally
		r.Free;
	end;
end;

destructor TGLFont.Destroy;
begin
	glDeleteTextures(1, @texture);
end;

function TGLFont.StringWidth(const str : String) : Longint;
var
	I, w : Longint;
begin
	w := 0;
	for I := 1 to Length(str) do
		w := w + width[Ord(str[I])];
	
	StringWidth := w;
end;

procedure TGLFont.DrawText2D(x, y : Longint; const str : String);
var
	c : Byte;
	u, v : Single;
	I : Longint;
	
	x_offs : Longint;
	row_pitch : Longint;
begin
	glEnable(GL_TEXTURE_2D);
	glEnable(GL_ALPHA_TEST);
	
	glBindTexture(GL_TEXTURE_2D, texture);
	
	x_offs := 0;
	row_pitch := tex_width div cell_width;
	
	glBegin(GL_QUADS);
	for I := 0 to Length(str)-1 do
	begin
		c := Ord(str[I+1])-base;
		u := ((c mod row_pitch) * cell_width) / tex_width;
		v := ((c div row_pitch) * cell_height) / tex_height;
	
		glTexCoord2f(u, v + (cell_height/tex_height));
		glVertex2f(x + x_offs, y);
		glTexCoord2f(u + (cell_width/tex_width), v + (cell_height/tex_height));
		glVertex2f(x + x_offs + cell_width, y);
		glTexCoord2f(u + (cell_width/tex_width), v);
		glVertex2f(x + x_offs + cell_width, y - cell_height);	
		glTexCoord2f(u, v);
		glVertex2f(x + x_offs, y - cell_height);
		
		x_offs := x_offs + width[Ord(str[I+1])];
	end;
	glEnd;
	
	glDisable(GL_ALPHA_TEST);
	glDisable(GL_TEXTURE_2D);
end;

end.
