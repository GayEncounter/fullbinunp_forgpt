unit common_flags;

interface

procedure InitializeFlags;
procedure DrawFlag(idx : Longint);
procedure GetFlagsForClass(const classname : String; out normal_flag, selected_flag : Longint);

implementation
uses StrUtils, Math, sysutils, Inifiles, GL;

type
	TFlagsByClass = record
		classname : String;
		normal_flag : Longint;
		selected_flag : Longint;
	end;

var
	gl_lists : GLuint;
	gl_lists_count : GLuint;
	
	flags_by_class : array of TFlagsByClass;

procedure MakeRoundFlag(r, g, b, _r, _g, _b : Single);
var
	I : Longint;
begin
	// fill
	glColor3f(r, g, b);
	glBegin(GL_TRIANGLE_FAN);
	glVertex3f(0.0, 0.5, 0.0);
	for I := 1 to 10 do
		glVertex3f(0.0, Cos(PI*I/10) * 0.125 + 0.375, Sin(PI*I/10) * -0.2);
	glEnd;
	glBegin(GL_TRIANGLE_FAN);
	glVertex3f(0.0, 0.25, 0.0);
	for I := 1 to 10 do
		glVertex3f(0.0, 0.375 - Cos(PI*I/10) * 0.125, Sin(PI*I/10) * -0.2);
	glEnd;
	
	// outline
	glColor3f(_r, _g, _b);
	glBegin(GL_LINES);
	glVertex3f(0.0, 0.0, 0.0);
	glVertex3f(0.0, 0.5, 0.0);
	glEnd;
	glBegin(GL_LINE_LOOP);
	glVertex3f(0.0, 0.5, 0.0);
	for I := 1 to 10 do
		glVertex3f(0.0, Cos(PI*I/10) * 0.125 + 0.375, Sin(PI*I/10) * -0.2);
	glEnd;	
	
	glColor3f(1, 1, 1);
end;

procedure MakeTriangleFlag(r, g, b, _r, _g, _b : Single);
begin
	// fill
	glColor3f(r, g, b);
	glBegin(GL_TRIANGLE_STRIP);
	glVertex3f(0.0, 0.5, 0.0);
	glVertex3f(0.0, 0.375, -0.25);
	glVertex3f(0.0, 0.250, 0.0);
	glVertex3f(0.0, 0.5, 0.0);
	glEnd;
	
	// outline
	glColor3f(_r, _g, _b);
	glBegin(GL_LINE_STRIP);
	glVertex3f(0.0, 0.0, 0.0);
	glVertex3f(0.0, 0.5, 0.0);
	glVertex3f(0.0, 0.375, -0.25);
	glVertex3f(0.0, 0.250, 0.0);
	glEnd;	
	
	glColor3f(1, 1, 1);
end;

procedure MakeQuadFlag(r, g, b, _r, _g, _b : Single);
begin
	// fill
	glColor3f(r, g, b);
	glBegin(GL_QUAD_STRIP);
	glVertex3f(0.0, 0.5, 0.0);
	glVertex3f(0.0, 0.5, -0.25);
	glVertex3f(0.0, 0.250, 0.0);
	glVertex3f(0.0, 0.250, -0.25);
	glVertex3f(0.0, 0.5, 0.0);
	glVertex3f(0.0, 0.5, -0.25);
	glEnd;
	
	// outline
	glColor3f(_r, _g, _b);
	glBegin(GL_LINE_STRIP);
	glVertex3f(0.0, 0.0, 0.0);
	glVertex3f(0.0, 0.5, 0.0);
	glVertex3f(0.0, 0.5, -0.25);
	glVertex3f(0.0, 0.250, -0.25);
	glVertex3f(0.0, 0.250, 0.0);
	glEnd;	
	
	glColor3f(1, 1, 1);
end;

procedure InitializeFlags;
var
	I, count : Longint;
	F : TIniFile;
	
	str : String;
	
	hex_clr : Longint;
	r, g, b : Single;
	_r, _g, _b : Single;
begin
	F := TIniFile.Create('editor_data\flags_by_class.ini');
	
	count := 0;
	while F.ValueExists('flag_types', 'flag_' + IntToStr(count+1) + '_type') do
		Inc(count);
		
	gl_lists := glGenLists(count);
	gl_lists_count := count;
	
	for I := 1 to count do
	begin
		str := F.ReadString('flag_types', 'flag_' + IntToStr(I) + '_fill', '#FFFFFF');	
		if (Length(str) > 0) and (str[1] = '#') then
			str[1] := '$';
			
		hex_clr := StrToInt(str);
		r := ((hex_clr and $FF0000) shr 16) / 255.0;
		g := ((hex_clr and $FF00) shr 8) / 255.0;
		b := (hex_clr and $FF) / 255.0;
		
		str := F.ReadString('flag_types', 'flag_' + IntToStr(I) + '_outline', '#FFFFFF');
		if (Length(str) > 0) and (str[1] = '#') then
			str[1] := '$';
			
		hex_clr := StrToInt(str);
		_r := ((hex_clr and $FF0000) shr 16) / 255.0;
		_g := ((hex_clr and $FF00) shr 8) / 255.0;
		_b := (hex_clr and $FF) / 255.0;
		
		glNewList(gl_lists + (I-1), GL_COMPILE);
		
		str := F.ReadString('flag_types', 'flag_' + IntToStr(I) + '_type', 'triangle');
		if AnsiCompareText(str, 'round') = 0 then
		begin
			MakeRoundFlag(r,g,b,_r,_g,_b);
		end else
		if AnsiCompareText(str, 'quad') = 0 then
		begin
			MakeQuadFlag(r,g,b,_r,_g,_b);
		end else
		begin
			MakeTriangleFlag(r,g,b,_r,_g,_b);
		end;
		
		glEndList
	end;
	
	count := 0;
	while F.SectionExists('class_' + IntToStr(count+1)) do
		Inc(count);
		
	SetLength(flags_by_class, count);
	
	for I := 0 to count - 1 do
	begin
		flags_by_class[I].classname := F.ReadString('class_' + IntToStr(I+1), 'classname', '_');
		flags_by_class[I].normal_flag := F.ReadInteger('class_' + IntToStr(I+1), 'normal_flag', -1) - 1;
		flags_by_class[I].selected_flag := F.ReadInteger('class_' + IntToStr(I+1), 'selected_flag', -1) - 1;
		
		if flags_by_class[I].selected_flag >= gl_lists_count then
			flags_by_class[I].selected_flag := -1;
		if flags_by_class[I].normal_flag >= gl_lists_count then
			flags_by_class[I].normal_flag := -1;
	end;
	
	F.Free;
end;

procedure DrawFlag(idx : Longint);
begin
	if (idx >= 0) and (idx < gl_lists_count) then
		glCallList(gl_lists + idx);
end;

procedure GetFlagsForClass(const classname : String; out normal_flag, selected_flag : Longint);
var
	I : Longint;
begin
	for I := 0 to Length(flags_by_class) - 1 do
	begin
		if flags_by_class[I].classname = classname then
		begin
			normal_flag := flags_by_class[I].normal_flag;
			selected_flag := flags_by_class[I].selected_flag;
			Exit;
		end;
	end;
	
	normal_flag := -1;
	selected_flag := -1;
end;

end.