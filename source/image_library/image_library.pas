program image_library;

uses 
	sysutils, 
	classes,
	common,           // для предпросмотра текстур
	GL,               // для предпросмотра текстур
	GLU,              // для предпросмотра текстур
	Iup, 
	Engine, 
	Texture,          // для предпросмотра текстур
	texturePrefs, 
	uChooseTexture,   // для выбора бампов и детейлов
	Konfig,           // для сохранения textures.bin
	TextureImport,    // для импорта текстур
	uCrc,             // для импорта текстур 
	FileApi,          // для импорта текстур 
	ImageLibraryOptions,
	uImages,
	uEditorUtils,     // для работы с деревом
	SGIImage,         // для сохранения текстуры в .sgi
	uProgressDlg;
	
var
	Modified : Boolean = False;

var
	current_texture : String;
	current_options : PTextureParams;

type
	TViewMode = (vm2D, vm3DCube);

var
	gl_texture : GLuint;
	gl_texture_name : String;
	gl_viewmode : TViewMode;
	gl_width : Longint;
	gl_height : Longint;
	gl_viewscale : Single = 1.0;
	gl_viewoffsetx, gl_viewoffsety : Single;

var
	tree_textures : Ihandle;
	gl_canvas : Ihandle;
	
	field_type : Ihandle;
	field_fmt : Ihandle;
	
	field_width : Ihandle;
	field_height : Ihandle;
	
	field_bump_name : Ihandle;
	field_bump_height : Ihandle;
	
	field_displ_mode : Ihandle;
	field_parr_height : Ihandle;
	
	field_det_name : Ihandle;
	field_det_u_scale : Ihandle;
	field_det_v_scale : Ihandle;
	field_det_int : Ihandle;
	
	field_animated    : Ihandle;
	field_mip_enabled : Ihandle;
	field_streamable : Ihandle;
	field_priority   : Ihandle;
	field_deprecated : Ihandle;
	
	field_avgcolor : Ihandle;
	
	menu_tree_textures : Ihandle;

procedure SelectTexture(const path : String);
var
	s : String[32];
	fmt_str : String;
begin
	current_texture := path;
	current_options := texture_params[current_texture];
	
	if current_options <> nil then
	begin
		case current_options.fmt of
			0: fmt_str := 'DXT1';
			1: fmt_str := 'DXT5';
			2: fmt_str := 'Uncompressed';
			else fmt_str := IntToStr(current_options.fmt);
		end;
		
		iup.SetInt(field_type, 'VALUE', current_options.ttype+1);
		iup.SetStrAttribute(field_fmt, 'TITLE', fmt_str);
		iup.SetStrAttribute(field_width, 'TITLE', IntToStr(current_options.r_width));
		iup.SetStrAttribute(field_height, 'TITLE', IntToStr(current_options.r_height));
		
		iup.SetStrAttribute(field_bump_name, 'VALUE', current_options.bump_name);
		
		WriteStr(s, current_options.bump_height:1:5, #0);
		iup.SetStrAttribute(field_bump_height, 'VALUE', s);
		
		iup.SetInt(field_displ_mode, 'VALUE', current_options.displ_mode+1);
		iup.SetInt(field_parr_height, 'SPINVALUE', current_options.parr_height);	
		
		iup.SetStrAttribute(field_det_name, 'VALUE', current_options.det_name);
		
		WriteStr(s, current_options.det_u_scale:1:5, #0);
		iup.SetStrAttribute(field_det_u_scale, 'VALUE', s);
		
		WriteStr(s, current_options.det_v_scale:1:5, #0);
		iup.SetStrAttribute(field_det_v_scale, 'VALUE', s);
		
		WriteStr(s, current_options.det_int:1:5, #0);
		iup.SetStrAttribute(field_det_int, 'VALUE', s);
		
		IupSetInt(field_animated,    'VALUE', Integer(current_options.animated));
		IupSetInt(field_mip_enabled, 'VALUE', Integer(current_options.mip_enabled));
		IupSetInt(field_streamable,  'VALUE', Integer(current_options.streamable));
		IupSetInt(field_priority,    'VALUE', Integer(current_options.priority));
		IupSetInt(field_deprecated,  'VALUE', Integer(current_options.isdeprecated));
		
		WriteStr(s, 
			(current_options.avg_color and $FF0000) shr 16, ' ',
			(current_options.avg_color and $FF00) shr 8, ' ',
			(current_options.avg_color and $FF), ' ', 
			(current_options.avg_color and $FF000000) shr 24, #0
		);
		
		iup.SetStrAttribute(field_avgcolor, 'BGCOLOR', s);
	end else
	begin
		iup.SetInt(field_type, 'VALUE', 0);
		iup.SetStrAttribute(field_fmt, 'TITLE', '');
		iup.SetStrAttribute(field_width, 'TITLE', '0');
		iup.SetStrAttribute(field_height, 'TITLE', '0');
		iup.SetAttribute(field_bump_name, 'VALUE', '');
		iup.SetStrAttribute(field_bump_height, 'VALUE', '0.00000');
		iup.SetInt(field_displ_mode, 'VALUE', 0);
		iup.SetInt(field_parr_height, 'SPINVALUE', 0);	
		iup.SetStrAttribute(field_det_name, 'VALUE', '');
		iup.SetStrAttribute(field_det_u_scale, 'VALUE', '0.00000');
		iup.SetStrAttribute(field_det_v_scale, 'VALUE', '0.00000');
		iup.SetStrAttribute(field_det_int, 'VALUE', '0.00000');
		
		IupSetInt(field_animated,    'VALUE', 0);
		IupSetInt(field_mip_enabled, 'VALUE', 0);
		IupSetInt(field_streamable,  'VALUE', 0);
		IupSetInt(field_priority,    'VALUE', 0);
		IupSetInt(field_deprecated,  'VALUE', 0);
		iup.SetAttribute(field_avgcolor, 'BGCOLOR', '0 0 0 0');
	end;
	
	IupRedraw(gl_canvas, 0);
end;

procedure InitializeTextureList(tree : Ihandle);
var
	I : Longint;
	sl : TStringList;
begin
	IupSetAttribute(tree, 'AUTOREDRAW', 'NO');

	sl := TStringList.Create;
	for I := 0 to Length(texture_params.options) - 1 do
		sl.Add(texture_params.options[I].name);
	FillTreeFromList(tree, 0, sl);
	sl.Free;
		
	IupSetAttribute(tree, 'AUTOREDRAW', 'YES');
		
	IupSetAttribute(tree, 'TITLE0', 'Textures');
	IupSetAttribute(tree, 'STATE0', 'EXPANDED');
end;

procedure SaveTexturesBin;
var
	K : TKonfig;
	TK : TTextKonfig;
begin
	TK := TTextKonfig.Create;
	texture_params.Save(TK);

	K := TKonfig.Create;
	K.kind := konfDiktionary;
	K.Compile(TK);
	K.Save(ResourcesPath + '\textures\' + 'textures.bin');
	
	if PatchDirectory <> '' then
	begin
		if FileExists(ResourcesPath + '\textures\textures.bin') then
		begin
			ForceDirectories(PatchDirectory + '\textures');
			CopyFile(ResourcesPath + '\textures\textures.bin', PatchDirectory + '\textures\textures.bin', False);  
		end;   		
	end;
	
	K.Free;
	TK.Free;
	
	Modified := False;
end;

procedure RemoveCurrentTexture;
var
	I : Longint;
begin
	I := 0;
	while I < Length(texture_params.options) - 1 do
	begin
		if current_texture = texture_params.options[I].name then
		begin
			Delete(texture_params.options, I, 1);
			IupSetAttribute(tree_textures, 'DELNODE', 'SELECTED');
			
			DeleteFile(ResourcesPath + '\textures\' + current_texture + '.dds');
			DeleteFile(ResourcesPath + '\textures\' + current_texture + '.512');
			DeleteFile(ResourcesPath + '\textures\' + current_texture + '.1024');
			DeleteFile(ResourcesPath + '\textures\' + current_texture + '.2048');
			DeleteFile(ResourcesPath + '\textures\' + current_texture + '.512c');
			DeleteFile(ResourcesPath + '\textures\' + current_texture + '.1024c');
			DeleteFile(ResourcesPath + '\textures\' + current_texture + '.2048c');
			DeleteFile(PatchDirectory + '\textures\' + current_texture + '.dds');
			DeleteFile(PatchDirectory + '\textures\' + current_texture + '.512');
			DeleteFile(PatchDirectory + '\textures\' + current_texture + '.1024');
			DeleteFile(PatchDirectory + '\textures\' + current_texture + '.2048');
			DeleteFile(PatchDirectory + '\textures\' + current_texture + '.512c');
			DeleteFile(PatchDirectory + '\textures\' + current_texture + '.1024c');
			DeleteFile(PatchDirectory + '\textures\' + current_texture + '.2048c');
			
			SelectTexture('');
			
			Modified := True;
			
			Break;
		end;
		
		Inc(I);
	end;
end;

function CalculateAvgColor(const path : String) : Longword;
var
	texture : GLuint;
	
	red   : QWord;
	green : QWord;
	blue  : QWord;
	alpha : QWord;
	
	average : Longword;
	
	I : Longint;
	width, height : GLint;
	buffer : array of array[1..4] of Byte;
begin
	average := 0;
	texture := LoadTexture(path);
	
	if texture <> 0 then
	begin
		glBindTexture(GL_TEXTURE_2D, texture);
		glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @width);
		glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @height);
		
		SetLength(buffer, width*height);
		
		WriteLn('glGetError1 = ', glGetError);
		glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, @buffer[0]);
		WriteLn('glGetError2 = ', glGetError);
		
		red   := 0;
		green := 0;
		blue  := 0;
		alpha := 0;
		
		for I := 0 to (width*height)-1 do
		begin
			Inc(red,   buffer[I,1]);
			Inc(green, buffer[I,2]);
			Inc(blue,  buffer[I,3]);
			Inc(alpha, buffer[I,4]);
		end;
		
		average := 
			(blue   div (width*height)) or
			((green div (width*height)) shl 8) or
			((red   div (width*height)) shl 16) or
			((alpha div (width*height)) shl 24);
		
		glBindTexture(GL_TEXTURE_2D, 0);
		glDeleteTextures(1, @texture);
	end;
	
	Result := average;
end;

procedure SaveImage(const tex_fn, out_fn : String);
var
	texture : GLuint;
	I : Longint;
	width, height : GLint;
	buffer : array of Longword;
begin
	texture := LoadTexture(tex_fn);
	
	if texture <> 0 then
	begin
		glBindTexture(GL_TEXTURE_2D, texture);
		glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @width);
		glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @height);
		
		SetLength(buffer, width*height);
		
		WriteLn('glGetError1 = ', glGetError);
		glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, @buffer[0]);
		WriteLn('glGetError2 = ', glGetError);
		
		SaveSGI(out_fn, width, height, buffer);
		
		glBindTexture(GL_TEXTURE_2D, 0);
		glDeleteTextures(1, @texture);
	end;
end;

{ Importing stuff }
function crunch_progress_callback(phase_index, total_phases, subphase_index, total_subphases : Longword; pUserData_ptr : Pointer) : Longint; cdecl;
var
	dlg : TProgressDlg;
begin
	dlg := TProgressDlg(pUserData_ptr);
	
	dlg.SetText(1, 'Crunch phase [' + IntToStr(phase_index) + '/' + IntToStr(total_phases) + ']...');
	dlg.SetAmount(1, total_phases);
	dlg.SetProgress(1, phase_index);
	
	dlg.SetText(2, 'Crunch subphase [' + IntToStr(subphase_index) + '/' + IntToStr(total_subphases) + ']...');
	dlg.SetAmount(2, total_subphases);
	dlg.SetProgress(2, subphase_index);	
	
	Result := 1;
end;

procedure ListFilesForImport(sl : TStringList; const dir : String);
var
	sr : TSearchRec;
begin
  if FindFirst(dir+'\*', faDirectory, sr) = 0 then
  begin
    repeat
      if not ((sr.Name = '.') or (sr.Name = '..')) then
        ListFilesForImport(sl, dir + '\' + sr.Name);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
  
  if FindFirst(dir + '\*.dds', faAnyFile xor faDirectory, sr) = 0 then
  begin
    repeat
    	sl.Add(dir + '\' + sr.Name);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure ImportTextures(const dir : String);
var
	sl : TStringList;
	n, dds : String;
	I, T : Longint;
	
	new_options : TTextureParams;
	
	//.dlg : Ihandle;
	dlg : TProgressDlg;
begin
	sl := TStringList.Create;
	ListFilesForImport(sl, dir);
	
	//.dlg := IupProgressDlg;
	//.IupSetAttribute(dlg, 'TITLE', 'Importing textures...');
	//.IupSetAttributeHandle(dlg, 'PARENTDIALOG', IupGetHandle('MAINDIALOG'));
	//.IupSetInt(dlg, 'TOTALCOUNT', sl.Count);
	
	//.IupShowXY(dlg, IUP_CENTER, IUP_CENTER);
	
	dlg := TProgressDlg.CreateMultiline(3);
	dlg.Title := 'Importing textures...';
	dlg.ParentDialog := IupGetHandle('MAINDIALOG');
	dlg.SetAmount(0, sl.Count);
	
	IupShowXY(dlg.dlg, IUP_CENTER, IUP_CENTER);
	
	for T := 0 to sl.Count - 1 do
	begin
		//.if IupGetAttribute(dlg, 'STATE') = 'ABORTED' then
		//.	Break;
		//.IupSetStrAttribute(dlg, 'DESCRIPTION', @sl[T][1]);

		IupFlush;		
		if dlg.CancelAsserted then
			Break;
		dlg.SetText(0, sl[T]);
		IupFlush;
	
		// 1. Получим имя текстуры для textures.bin                             
		n := sl[T];
	  
		if Copy(n, 1, Length(ImportDirectory)) = ImportDirectory then
			n := Copy(n, Length(ImportDirectory) + 2); // ImportDirectory\tex\tex_1.dds -> tex\tex_1.dds
	  	
		n := LowerCase(n); // имя текстуры обязательно в нижнем регистре
		n := ChangeFileExt(n, '');
	  
		// 2. Попробуем найти запись в textures.bin
		I := 0;
	  
		while (I < Length(texture_params.options)) and (texture_params.options[I].name < n) do
			Inc(I);
	  
		// 3. Добавим или изменим запись и импортируем текстуру
		if (I < Length(texture_params.options)) and (texture_params.options[I].name = n) then
		begin
			ForceDirectories(ExtractFileDir(ResourcesPath + '\textures\' + n));
	  
			if Engine.version >= eVerLL then
				ImportTextureLL(sl[T], ResourcesPath + '\textures\' + n, @texture_params.options[I],
					@crunch_progress_callback, Pointer(dlg))
			else
				ImportTexture(sl[T], ResourcesPath + '\textures\' + n, @texture_params.options[I]);
				
			texture_params.options[I].avg_color := CalculateAvgColor(ResourcesPath + '\textures\' + n);
	  	
			Modified := True;
		end else
		begin
			FillChar(new_options, Sizeof(new_options), #0);
			Insert(new_options, texture_params.options, I);
	  	
			// установим некоторые стандартные занчения
			texture_params.options[I].name := n;
			texture_params.options[I].namecrc := GetStringCrc(n);
			texture_params.options[I].streamable := True;
			texture_params.options[I].mip_enabled := True;
			texture_params.options[I].bump_height := 0.1;
			texture_params.options[I].parr_height := 22;
			texture_params.options[I].displ_mode := 2;
			texture_params.options[I].det_u_scale := 5.0;
			texture_params.options[I].det_v_scale := 5.0;
			texture_params.options[I].det_int := 0.5;
	  	
			ForceDirectories(ExtractFileDir(ResourcesPath + '\textures\' + n));
			
			if Engine.version >= eVerLL then
				ImportTextureLL(sl[T], ResourcesPath + '\textures\' + n, @texture_params.options[I],
					@crunch_progress_callback, Pointer(dlg))
			else
				ImportTexture(sl[T], ResourcesPath + '\textures\' + n, @texture_params.options[I]);
			
			texture_params.options[I].avg_color := CalculateAvgColor(ResourcesPath + '\textures\' + n);
			
			AddTreeNode(tree_textures, n);
			Modified := True;
		end;
	  
		dds := sl[T];
		MoveFile(dds, ChangeFileExt(dds, '.~dds'), True);
	  
		// 4. Скопируем текстуру в папку патча
		if PatchDirectory <> '' then
		begin
			ForceDirectories(ExtractFileDir(PatchDirectory + '\textures\' + n));
		
			if texture_params.options[I].streamable then
			begin
				if FileExists(ResourcesPath + '\textures\' + n + '.2048') then
					CopyFile(ResourcesPath + '\textures\' + n + '.2048', PatchDirectory + '\textures\' + n + '.2048', False);
				if FileExists(ResourcesPath + '\textures\' + n + '.1024') then
					CopyFile(ResourcesPath + '\textures\' + n + '.1024', PatchDirectory + '\textures\' + n + '.1024', False);
				if FileExists(ResourcesPath + '\textures\' + n + '.512') then
					CopyFile(ResourcesPath + '\textures\' + n + '.512', PatchDirectory + '\textures\' + n + '.512', False);
				if FileExists(ResourcesPath + '\textures\' + n + '.2048c') then
					CopyFile(ResourcesPath + '\textures\' + n + '.2048c', PatchDirectory + '\textures\' + n + '.2048c', False);
				if FileExists(ResourcesPath + '\textures\' + n + '.1024c') then
					CopyFile(ResourcesPath + '\textures\' + n + '.1024c', PatchDirectory + '\textures\' + n + '.1024c', False);
				if FileExists(ResourcesPath + '\textures\' + n + '.512c') then
					CopyFile(ResourcesPath + '\textures\' + n + '.512c', PatchDirectory + '\textures\' + n + '.512c', False);
			end else
				if FileExists(ResourcesPath + '\textures\' + n + '.dds') then
					CopyFile(ResourcesPath + '\textures\' + n + '.dds', PatchDirectory + '\textures\' + n + '.dds', False);     		
		end;
		
		//.IupSetAttribute(dlg, 'INC', nil);
		dlg.SetProgress(0, T+1);
	end;
  
	//.IupDestroy(dlg);
	dlg.Free;
	sl.Free;
end;
  
function gl_map_cb(ih : Ihandle) : Longint; cdecl;
begin
	IupGLMakeCurrent(ih);
	InitializeRender;
	Result := IUP_DEFAULT;
end;
  
function gl_redraw_cb(ih : Ihandle; x, y : Single) : Longint; cdecl;
//var
//	sinx, cosx : Single;
//	siny, cosy : Single;
//	
//	m : TMatrix;
begin
	IupGLMakeCurrent(ih);
	
	//glClearColor(bkg_color.x, bkg_color.y, bkg_color.z, bkg_color.w);
	glClearColor(1.0, 1.0, 1.0, 1.0);
	glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
	
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity;
	glTranslatef(-gl_viewoffsetx, -gl_viewoffsety, 0.0);
	if gl_width > gl_height then
		glScalef(1.0 / (gl_width / gl_height), 1.0, 1.0)
	else
		glScalef(1.0, 1.0 / (gl_height / gl_width), 1.0);
	glScalef(gl_viewscale, gl_viewscale, 1.0);
		
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity;

//	sinx := Sin(anglex * (PI/180));
//	cosx := Cos(anglex * (PI/180));
//	LookAtLH(m, distance*siny*cosx, distance*sinx, distance*cosy*cosx, 0, 0, 0, 0, 1, 0);
//	glLoadMatrixf(@m);

//	glGetDoublev(GL_MODELVIEW_MATRIX, @modelview_d);
//	glGetDoublev(GL_PROJECTION_MATRIX, @proj_d);
//	glGetFloatv(GL_MODELVIEW_MATRIX, @modelview);
//	glGetFloatv(GL_PROJECTION_MATRIX, @proj);
//	glGetIntegerv(GL_VIEWPORT, @viewport);

	if gl_texture_name <> current_texture then
	begin
		glDeleteTextures(1, @gl_texture);
		gl_texture := LoadTexture(ResourcesPath + '\textures\' + current_texture);
		gl_texture_name := current_texture;
	end;
	
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, gl_texture);
	
	glBegin(GL_QUADS);
	
	glTexCoord2f(0, 0);
	glVertex2f(-1, 1);
	glTexCoord2f(1, 0);
	glVertex2f(1, 1);
	glTexCoord2f(1, 1);
	glVertex2f(1, -1);
	glTexCoord2f(0, 1);
	glVertex2f(-1, -1);	
	
	glEnd;
	
	glDisable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, 0);
	
	IupGLSwapBuffers(ih);
	Result := IUP_DEFAULT;
end;

function gl_resize_cb(ih : Ihandle; w, h : Longint) : Longint; cdecl;
begin
	IupGLMakeCurrent(ih);
	glViewport(0, 0, w, h);

	gl_width := w;
	gl_height := h;

	Result := IUP_DEFAULT;
end;

var
	old_x, old_y : Longint;

function gl_motion_cb(ih : Ihandle; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
begin
	if iup_isbutton3(status) then
	begin
		gl_viewoffsetx := gl_viewoffsetx - ((x - old_x) / (gl_width * 0.5));
		gl_viewoffsety := gl_viewoffsety + ((y - old_y) / (gl_height * 0.5));
		IupUpdate(ih);
	end;
	
	old_x := x;
	old_y := y;
	
	Result := IUP_DEFAULT;
end;

function gl_wheel_cb(ih : Ihandle; delta : Single; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
begin
	gl_viewscale := gl_viewscale + (delta * 0.1);
	if gl_viewscale < 0.1 then gl_viewscale := 0.1;
	if gl_viewscale > 10.0 then gl_viewscale := 10.0;
	IupUpdate(ih);
	Result := IUP_DEFAULT;	
end;

function tree_selection_cb(ih : Ihandle; id, state : Longint) : Longint; cdecl;
var
	path : String;
begin
	Result := IUP_DEFAULT;
	
	if state = 1 then
	begin
		path := PathOfTreeNode(ih, id);
		WriteLn('SELECT ', path);
		SelectTexture(path);
	end;
end;

function tree_rightclick_cb(ih : Ihandle; id : Longint) : Longint; cdecl;
begin
	Result := IUP_DEFAULT;
	IupPopup(menu_tree_textures, IUP_MOUSEPOS, IUP_MOUSEPOS);
end;

function list_ttype_cb(ih : Ihandle; _text : PAnsiChar; item, state : Longint) : Longint; cdecl;
begin
	if current_options <> nil then
		if state = 1 then
		begin
			current_options.ttype := item-1;
			Modified := True;
		end;
	
	Result := IUP_DEFAULT;
end;

function text_bump_name_cb(ih : Ihandle; c : Longint; new_value : PAnsiChar) : Longint; cdecl;
begin
	if current_options <> nil then
	begin
		current_options.bump_name := new_value;
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function btn_bump_name_cb(ih : Ihandle) : Longint; cdecl;
begin
	if current_options <> nil then
		if ChooseTexture(current_options.bump_name) then
		begin
			iup.SetStrAttribute(field_bump_name, 'VALUE', current_options.bump_name);
			Modified := True;
		end;
			
	Result := IUP_DEFAULT;
end;

function text_bump_height_cb(ih : Ihandle; c : Longint; new_value : PAnsiChar) : Longint; cdecl;
begin
	if current_options <> nil then
	begin
		try
			ReadStr(new_value, current_options.bump_height);
		except
			current_options.bump_height := 0;
		end;
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function list_displ_mode_cb(ih : Ihandle; _text : PAnsiChar; item, state : Longint) : Longint; cdecl;
begin
	if current_options <> nil then
		if state = 1 then
		begin
			current_options.displ_mode := item-1;
			Modified := True;
		end;
	
	Result := IUP_DEFAULT;
end;

function text_parr_height_cb(ih : Ihandle; c : Longint; new_value : PAnsiChar) : Longint; cdecl;
begin
	if current_options <> nil then
	begin
		try
			current_options.parr_height := StrToInt(new_value);
		except
			current_options.parr_height := 0;
		end;
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function spin_parr_height_cb(ih : Ihandle; pos : Longint) : Longint; cdecl;
begin
	if current_options <> nil then
	begin
		current_options.parr_height := pos;
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function text_det_name_cb(ih : Ihandle; c : Longint; new_value : PAnsiChar) : Longint; cdecl;
begin
	if current_options <> nil then
	begin
		current_options.det_name := new_value;
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function btn_det_name_cb(ih : Ihandle) : Longint; cdecl;
begin
	if current_options <> nil then
		if ChooseTexture(current_options.det_name) then
		begin
			iup.SetStrAttribute(field_det_name, 'VALUE', current_options.det_name);
			Modified := True;
		end;
			
	Result := IUP_DEFAULT;
end;

function text_det_u_scale_cb(ih : Ihandle; c : Longint; new_value : PAnsiChar) : Longint; cdecl;
begin
	if current_options <> nil then
	begin
		try
			ReadStr(new_value, current_options.det_u_scale);
		except
			current_options.det_u_scale := 0;
		end;
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function text_det_v_scale_cb(ih : Ihandle; c : Longint; new_value : PAnsiChar) : Longint; cdecl;
begin
	if current_options <> nil then
	begin
		try
			ReadStr(new_value, current_options.det_v_scale);
		except
			current_options.det_v_scale := 0;
		end;
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function text_det_int_cb(ih : Ihandle; c : Longint; new_value : PAnsiChar) : Longint; cdecl;
begin
	if current_options <> nil then
	begin
		try
			ReadStr(new_value, current_options.det_int);
		except
			current_options.det_int := 0;
		end;
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function tg_animated_cb(ih : Ihandle; state : Longint) : Longint; cdecl;
begin
	if current_options <> nil then
	begin
		current_options.animated := state <> 0;
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function tg_mip_enabled_cb(ih : Ihandle; state : Longint) : Longint; cdecl;
begin
	if current_options <> nil then
	begin
		current_options.mip_enabled := state <> 0;
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function tg_streamable_cb(ih : Ihandle; state : Longint) : Longint; cdecl;
var
	n : String;
	dxt1a : Boolean;
begin
	if current_options <> nil then
	begin
		n := current_texture;
	
		try
			if Engine.version >= eVerLL then
			begin
				if state = 1 then
				begin
					MakeStreamable_LL(ResourcesPath + '\textures\' + n + '.dds', ResourcesPath + '\textures\' + n, dxt1a, nil, nil);
					
					ForceDirectories(ExtractFileDir(PatchDirectory + '\textures\' + n));
					
					CopyFile(ResourcesPath + '\textures\' + n + '.2048', PatchDirectory + '\textures\' + n + '.2048', False);
					CopyFile(ResourcesPath + '\textures\' + n + '.1024c', PatchDirectory + '\textures\' + n + '.1024c', False);
					CopyFile(ResourcesPath + '\textures\' + n + '.512c', PatchDirectory + '\textures\' + n + '.512c', False);
					
					DeleteFile(ResourcesPath + '\textures\' + n + '.dds');
					DeleteFile(PatchDirectory + '\textures\' + n + '.dds');
				end else
				begin
					MakeDDS_LL(ResourcesPath + '\textures\' + n);
					
					ForceDirectories(ExtractFileDir(PatchDirectory + '\textures\' + n));
					
					CopyFile(ResourcesPath + '\textures\' + n + '.dds', PatchDirectory + '\textures\' + n + '.dds', False);
					
					DeleteFile(ResourcesPath + '\textures\' + n + '.512c');
					DeleteFile(ResourcesPath + '\textures\' + n + '.1024c');
					DeleteFile(ResourcesPath + '\textures\' + n + '.2048');
					DeleteFile(PatchDirectory + '\textures\' + n + '.512c');
					DeleteFile(PatchDirectory + '\textures\' + n + '.1024c');
					DeleteFile(PatchDirectory + '\textures\' + n + '.2048');
				end;
			end else // 2033, build 15-10-2012, build 03-12-2012
			begin
				if state = 1 then
				begin
					MakeStreamable_2033(ResourcesPath + '\textures\' + n + '.dds', ResourcesPath + '\textures\' + n);
					
					ForceDirectories(ExtractFileDir(PatchDirectory + '\textures\' + n));
					
					CopyFile(ResourcesPath + '\textures\' + n + '.2048', PatchDirectory + '\textures\' + n + '.2048', False);
					CopyFile(ResourcesPath + '\textures\' + n + '.1024', PatchDirectory + '\textures\' + n + '.1024', False);
					CopyFile(ResourcesPath + '\textures\' + n + '.512', PatchDirectory + '\textures\' + n + '.512', False);
					
					DeleteFile(ResourcesPath + '\textures\' + n + '.dds');
					DeleteFile(PatchDirectory + '\textures\' + n + '.dds');
				end else
				begin
					MakeDDS_2033(ResourcesPath + '\textures\' + n);
					
					ForceDirectories(ExtractFileDir(PatchDirectory + '\textures\' + n));
					
					CopyFile(ResourcesPath + '\textures\' + n + '.dds', PatchDirectory + '\textures\' + n + '.dds', False);
					
					DeleteFile(ResourcesPath + '\textures\' + n + '.512');
					DeleteFile(ResourcesPath + '\textures\' + n + '.1024');
					DeleteFile(ResourcesPath + '\textures\' + n + '.2048');
					DeleteFile(PatchDirectory + '\textures\' + n + '.512');
					DeleteFile(PatchDirectory + '\textures\' + n + '.1024');
					DeleteFile(PatchDirectory + '\textures\' + n + '.2048');
				end;
			end;
			
			current_options.streamable := state <> 0;
			Modified := True;
			
			if dxt1a then
				SelectTexture(current_texture); // format was changed, update value
		except 
			on E : Exception do 
			begin
				// reset toggle and show error
				IupSetInt(ih, 'VALUE', (not state) and 1);
				uEditorUtils.ShowError(E.ClassName + ': ' + E.Message);
			end;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function tg_priority_cb(ih : Ihandle; state : Longint) : Longint; cdecl;
begin
	if current_options <> nil then
	begin
		current_options.priority := state <> 0;
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function tg_deprecated_cb(ih : Ihandle; state : Longint) : Longint; cdecl;
begin
	if current_options <> nil then
	begin
		current_options.isdeprecated := state <> 0;
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function btn_avg_color_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	s : String[32];
	r, g, b, a : Byte;
begin
	if current_options <> nil then
	begin
		dlg := IupColorDlg;
		
		WriteStr(s, 
			(current_options.avg_color and $FF0000) shr 16, ' ',
			(current_options.avg_color and $FF00) shr 8, ' ',
			(current_options.avg_color and $FF), ' ', 
			(current_options.avg_color and $FF000000) shr 24, #0
		);
		
		IupSetAttribute(dlg, 'VALUE', @s[1]);
		IupSetAttribute(dlg, 'SHOWALPHA', 'YES');
		
		IupPopup(dlg, IUP_CURRENT, IUP_CURRENT);
		
		if IupGetAttribute(dlg, 'STATUS') = '1' then
		begin
			s := IupGetAttribute(dlg, 'VALUE') + #0;
			
			ReadStr(s, r, g, b, a);
			current_options.avg_color := (r shl 16) or (g shl 8) or (b) or (a shl 24);
			iup.SetStrAttribute(field_avgcolor, 'BGCOLOR', s);
			
			Modified := True;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function btn_calc_avg_color_cb(ih : Ihandle) : Longint; cdecl;
var
	s : String[32];
begin
	if current_options <> nil then
	begin
		current_options.avg_color := CalculateAvgColor(ResourcesPath + '\textures\' + current_texture);
		
		WriteStr(s, 
			(current_options.avg_color and $FF0000) shr 16, ' ',
			(current_options.avg_color and $FF00) shr 8, ' ',
			(current_options.avg_color and $FF), ' ', 
			(current_options.avg_color and $FF000000) shr 24, #0
		);
		
		iup.SetStrAttribute(field_avgcolor, 'BGCOLOR', s);
		
		Modified := True;
	end;
	
	Result := IUP_DEFAULT;
end;

function btn_remove_texture_cb(ih : Ihandle) : Longint; cdecl;
var
	msg : Ihandle;
	ret : PAnsiChar;
begin
	Result := IUP_DEFAULT;
	
	if current_options <> nil then
	begin
		msg := IupMessageDlg;
		IupSetAttribute(msg, 'BUTTONS', 'YESNO');
		IupSetAttribute(msg, 'DIALOGTYPE', 'QUESTION');
		iup.SetStrAttribute(msg, 'VALUE', 'Are you sure want to delete ''' + current_texture + '''?');
		
		IupPopup(msg, IUP_CURRENT, IUP_CURRENT);
		
		ret := IupGetAttribute(msg, 'BUTTONRESPONSE');
		
		if ret = '1' then
			RemoveCurrentTexture;
			
		IupDestroy(msg);
	end;
end;

function btn_check_notexist_cb(ih : Ihandle) : Longint; cdecl;
var
	I,N : Longint;
	tex_path : String;
	name : String;
begin
	N := 0;
	tex_path := ResourcesPath + '\textures\';
	
	for I := 0 to iup.GetInt(tree_textures, 'COUNT') - 1 do
	begin
		if KindOfTreeNode(tree_textures, I) = 'LEAF' then
		begin
			name := PathOfTreeNode(tree_textures, I);
			
			if (name[1] <> '$') and // special runtime-created textures prefixed with dollar sign, they still have options though
			   (not FileExists(tex_path + name + '.512')) and
			   (not FileExists(tex_path + name + '.512c')) and
			   (not FileExists(tex_path + name + '.dds')) and
			   (not FileExists(tex_path + name + '.ani')) then
			begin
				iup.SetAttribute(tree_textures, 'COLOR'+IntToStr(I), '128 0 0');
				Inc(N);
			end;
		end;
	end;
			
	IupMessagef('Message', 'Found %d not existing textures', N);

	Result := IUP_DEFAULT;
end;

function btn_check_new_cb(ih : Ihandle) : Longint; cdecl;
begin
	try
		ImportTextures(ImportDirectory);
	except on E: Exception do
		IupMessage('Error', PAnsiChar(E.ClassName + ': ' + E.Message));
	end;
	
	Result := IUP_DEFAULT;
end;

function btn_save_texturesbin_cb(ih : Ihandle) : Longint; cdecl;
begin
	SaveTexturesBin;
	Result := IUP_DEFAULT;
end;

function fb_settings_cb(ih : Ihandle) : Longint; cdecl;
begin
	ShowOptions;
	Result := IUP_DEFAULT;
end;

function dlg_close_cb(ih : Ihandle) : Longint; cdecl;
var
	msg : Ihandle;
	ret : PAnsiChar;
begin
	Result := IUP_DEFAULT;
	
	if Modified then
	begin
		msg := IupMessageDlg;
		IupSetAttribute(msg, 'BUTTONS', 'YESNOCANCEL');
		IupSetAttribute(msg, 'DIALOGTYPE', 'QUESTION');
		IupSetAttribute(msg, 'VALUE', 'The file was modified. Do you want to save changes?');
		
		IupPopup(msg, IUP_CURRENT, IUP_CURRENT);
		
		ret := IupGetAttribute(msg, 'BUTTONRESPONSE');
		
		if ret = '1' then
			SaveTexturesBin;
		if ret = '3' then
			Result := IUP_IGNORE;
			
		IupDestroy(msg);
	end;
end;

function menu_expand_all_cb(ih : Ihandle) : Longint; cdecl;
begin
	Result := IUP_DEFAULT;
	IupSetAttribute(tree_textures, 'EXPANDALL', 'YES');
end;

function menu_collapse_all_cb(ih : Ihandle) : Longint; cdecl;
begin
	Result := IUP_DEFAULT;
	IupSetAttribute(tree_textures, 'EXPANDALL', 'NO');
end;

function menu_save_as_sgi_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	selnode : Longint;
	tex_name : String;
begin
	Result := IUP_DEFAULT;
	
	selnode := IupGetInt(tree_textures, 'VALUE');
	if IupGetAttributeId(tree_textures, 'KIND', selnode) = 'LEAF' then
	begin
		dlg := IupFileDlg;
		iup.SetStrAttribute(dlg, 'EXTDEFAULT', 'rgb');
		iup.SetStrAttribute(dlg, 'EXTFILTER', 'Silicon Graphics Image (*.rgb)|*.rgb|All files (*.*)|*.*|');
		IupSetAttribute(dlg, 'DIALOGTYPE', 'SAVE');
		IupSetStrAttribute(dlg, 'FILE', IupGetAttributeId(tree_textures, 'TITLE', selnode));
	
		IupPopup(dlg, IUP_CENTER, IUP_CENTER);
	
		if IupGetInt(dlg, 'STATUS') <> -1 then
		begin
			tex_name := ResourcesPath + '\textures\' + PathOfTreeNode(tree_textures, selnode);
			SaveImage(tex_name, IupGetAttribute(dlg, 'VALUE'));
		end;
	end;

	IupDestroy(dlg);
end;

procedure CreateDialog;
var
	dlg : Ihandle;
	tree : Ihandle;
	props : Ihandle;
  
	top_bar : Ihandle;
	fb_settings : Ihandle;
begin
	// Load icons
	LoadImages;
	
	gl_canvas := IupGLCanvas(nil);
	IupSetAttribute(gl_canvas, 'NAME', 'GL_CANVAS');
	IupSetAttribute(gl_canvas, 'BUFFER', 'DOUBLE');
	//IupSetInt(gl_canvas, 'DEPTH_SIZE', 32);
	//IupSetAttribute(gl_canvas, 'EXPAND', 'YES');
	IupSetAttribute(gl_canvas, 'RASTERSIZE', '512x512');
	IupSetCallback(gl_canvas, 'ACTION', @gl_redraw_cb);
	IupSetCallback(gl_canvas, 'MAP_CB', @gl_map_cb);
	IupSetCallback(gl_canvas, 'RESIZE_CB', @gl_resize_cb);
	IupSetCallback(gl_canvas, 'MOTION_CB', @gl_motion_cb);
	//IupSetCallback(gl_canvas, 'BUTTON_CB', @gl_button_cb);
	IupSetCallback(gl_canvas, 'WHEEL_CB', @gl_wheel_cb);

	tree := IupTree;
	IupSetAttribute(tree, 'ADDEXPANDED', 'NO');
	IupSetAttribute(tree, 'RASTERSIZE', '250x350');
	IupSetAttribute(tree, 'IMAGELEAF', 'ICON_TEXTURE');
	
	IupSetCallback(tree, 'SELECTION_CB', @tree_selection_cb);
	IupSetCallback(tree, 'RIGHTCLICK_CB', @tree_rightclick_cb);
	
	tree_textures := tree;
	
	field_type := IupList(nil); 
	IupSetAttribute(field_type, 'DROPDOWN', 'YES');
	IupSetAttribute(field_type, '1', '2D Texture');
	IupSetAttribute(field_type, '2', 'Detail map');
	IupSetAttribute(field_type, '3', 'Environment map');
	IupSetAttribute(field_type, '4', 'Skybox');
	IupSetAttribute(field_type, '5', 'Terrain');
	IupSetAttribute(field_type, '6', 'Bump Map');
	IupSetAttribute(field_type, '7', 'Unknown 6');
	if Engine.version >= eVerLLBeta15102012 then
	begin
		IupSetAttribute(field_type, '8', 'Terrain Mask');
		IupSetAttribute(field_type, '9', 'Normal Map');
	end;
	IupSetCallback(field_type, 'ACTION', @list_ttype_cb);
  
	field_fmt := IupLabel(''); 
	IupSetAttribute(field_fmt, 'RASTERSIZE', '150x');
	
	field_width := IupLabel(''); 
	IupSetAttribute(field_width, 'RASTERSIZE', '150x');
	
	field_height := IupLabel(''); 
	IupSetAttribute(field_height, 'RASTERSIZE', '150x');
	
	field_bump_name   := IupText(nil);
	IupSetAttribute(field_bump_name, 'EXPAND', 'HORIZONTAL');
	IupSetCallback(field_bump_name, 'ACTION', @text_bump_name_cb);
	
	field_bump_height := IupText(nil);
	IupSetAttribute(field_bump_height, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(field_bump_height, 'MASK', IUP_MASK_UFLOAT);
	IupSetCallback(field_bump_height, 'ACTION', @text_bump_height_cb);
	
	field_displ_mode := IupList(nil);
	IupSetAttribute(field_displ_mode, 'DROPDOWN', 'YES');
	IupSetAttribute(field_displ_mode, '1', 'Simple'); // названия взяты из PDB от Арктики.1
	IupSetAttribute(field_displ_mode, '2', 'Parralax');
	IupSetAttribute(field_displ_mode, '3', 'Displace');
	IupSetCallback(field_displ_mode, 'ACTION', @list_displ_mode_cb);
	
	field_parr_height := IupText(nil);
	IupSetAttribute(field_parr_height, 'MASK', IUP_MASK_UINT);
	IupSetAttribute(field_parr_height, 'SPIN', 'YES');
	IupSetAttribute(field_parr_height, 'SPINMIN', '0');
	IupSetAttribute(field_parr_height, 'SPINMAX', '255');
	IupSetCallback(field_parr_height, 'ACTION', @text_parr_height_cb);
	IupSetCallback(field_parr_height, 'SPIN_CB', @spin_parr_height_cb);
	
	field_det_name    := IupText(nil);
	IupSetAttribute(field_det_name, 'EXPAND', 'HORIZONTAL');
	IupSetCallback(field_det_name, 'ACTION', @text_det_name_cb);
	
	field_det_u_scale := IupText(nil);
	IupSetAttribute(field_det_u_scale, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(field_det_u_scale, 'MASK', IUP_MASK_UFLOAT);
	IupSetCallback(field_det_u_scale, 'ACTION', @text_det_u_scale_cb);
	
	field_det_v_scale := IupText(nil);
	IupSetAttribute(field_det_v_scale, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(field_det_v_scale, 'MASK', IUP_MASK_UFLOAT);
	IupSetCallback(field_det_v_scale, 'ACTION', @text_det_v_scale_cb);
	
	field_det_int := IupText(nil);
	IupSetAttribute(field_det_int, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(field_det_int, 'MASK', IUP_MASK_UFLOAT);
	IupSetCallback(field_det_int, 'ACTION', @text_det_int_cb);
	
	field_animated    := iup.Toggle('Здесь', @tg_animated_cb);
	field_mip_enabled := iup.Toggle('Могла', @tg_mip_enabled_cb);
	field_streamable  := iup.Toggle('Быть', @tg_streamable_cb);
	field_priority    := iup.Toggle('Ваша', @tg_priority_cb);
	field_deprecated  := iup.Toggle('Реклама', @tg_deprecated_cb);
	
	field_avgcolor := IupFlatButton(' ');
	IupSetAttribute(field_avgcolor, 'EXPAND', 'HORIZONTAL');
	IupSetCallback(field_avgcolor, 'FLAT_ACTION', @btn_avg_color_cb);
	IupSetAttribute(field_avgcolor, 'HLCOLOR', nil);
	IupSetAttribute(field_avgcolor, 'PSCOLOR', nil);

	props := IupGridBox(
		IupLabel('Type:'),   field_type,
		IupLabel('Format:'), field_fmt,
		IupLabel('Width:'),  field_width, 
		IupLabel('Height:'), field_height,
		IupLabel('Bump name: '), IupHBox(field_bump_name, iup.Button('Choose', @btn_bump_name_cb), nil),
		IupLabel('Bump height: '), IupHBox(field_bump_height, nil),
		IupLabel('Displ mode: '), field_displ_mode,
		IupLabel('Parr height: '), field_parr_height,
		IupLabel('Detail name: '), IupHBox(field_det_name, iup.Button('Choose', @btn_det_name_cb), nil),
		IupLabel('Detail U scale: '), IupHBox(field_det_u_scale, nil),
		IupLabel('Detail V scale: '), IupHBox(field_det_v_scale, nil),
		IupLabel('Detail intensivity: '), IupHBox(field_det_int, nil),
		IupLabel('Animated: '), field_animated, 
		IupLabel('Mip-Maps: '), field_mip_enabled, 
		IupLabel('Streamable: '), field_streamable, 
		IupLabel('Priority: '), field_priority, 
		IupLabel('Deprecated: '), field_deprecated, 
		IupLabel('Average color:'), IupHBox(field_avgcolor, iup.Button('Calc', @btn_calc_avg_color_cb), nil),
		nil
	);
	
	IupSetAttribute(props, 'NUMDIV', '2');
	IupSetAttribute(props, 'GAPLIN', '5');
	IupSetAttribute(props, 'GAPCOL', '5');
	IupSetAttribute(props, 'NMARGIN', '10x10');
	IupSetAttribute(props, 'SIZECOL', '1');
	IupSetAttribute(props, 'SIZELIN', '1');
	IupSetAttribute(props, 'EXPANDCHILDREN', 'HORIZONTAL');
	IupSetAttribute(props, 'ALIGNMENTCOL1', 'ARIGHT');
	IupSetAttribute(props, 'ALIGNMENTLIN0', 'ACENTER');
	IupSetAttribute(props, 'RASTERSIZE', '320x');
	
	fb_settings := IupFlatButton('Settings');
	IupSetCallback(fb_settings, 'FLAT_ACTION', @fb_settings_cb);
	IupSetAttribute(fb_settings, 'IMAGE', 'ICON_TOOLS');
	
	top_bar := IupHBox(
		fb_settings, 
		nil
	);
	
	IupSetAttribute(top_bar, 'MARGIN', '1x1');
	IupSetAttribute(top_bar, 'PADDING', '1x1');

	dlg := IupDialog(
		IupSetAttributes(
			IupSplit(
				IupVBox(
					tree, 
					IupSetAttributes(iup.Button('Remove texture', @btn_remove_texture_cb), 'EXPAND=HORIZONTAL'),
					IupSetAttributes(iup.Button('Check not existing', @btn_check_notexist_cb), 'EXPAND=HORIZONTAL'),
					IupSetAttributes(iup.Button('Check new textures', @btn_check_new_cb), 'EXPAND=HORIZONTAL'),
					IupSetAttributes(iup.Button('Save textures.bin', @btn_save_texturesbin_cb), 'EXPAND=HORIZONTAL'),
					nil
				),
				IupSplit(IupVBox(top_bar, gl_canvas, nil), props)
			), 
			'NMARGIN=10x10'
		)
	);
	IupSetAttribute(dlg, 'TITLE', 'Image Library');
	IupSetCallback(dlg, 'CLOSE_CB', @dlg_close_cb);
	IupShowXY(dlg, IUP_CURRENT, IUP_CURRENT);
  
	IupSetHandle('MAINDIALOG', dlg);
  
	InitializeTextureList(tree);
	
	menu_tree_textures := IupMenu(
		iup.MenuItem('Expand All', @menu_expand_all_cb),
		iup.MenuItem('Collapse All', @menu_collapse_all_cb),
		IupSeparator,
		iup.MenuItem('Save as SGI...', @menu_save_as_sgi_cb),
		nil
	);
end;

type
	qsort_compar = function(a, b : Pointer) : Longint; cdecl;

procedure qsort(base : Pointer; num : Cardinal; width : Cardinal; compar : qsort_compar); cdecl; external 'msvcrt.dll';

function textures_compar(a, b : Pointer) : Longint; cdecl;
begin
	Result := AnsiCompareStr(PTextureParams(a).name, PTextureParams(b).name);
end;

var
	dlg : Ihandle;
	P : Longint;
begin
	textureQuality := 2048;
	
	IupOpen(nil, nil);
	IupGLCanvasOpen;

	// parse command-line
	P := 0;
		
	if (ParamCount-P > 0) and (ParamStr(P+1) = '-build_15_10_2012') then
	begin
		Engine.version := eVerLLBeta15102012;
		Inc(P);
	end;
	
	if (ParamCount-P > 0) and (ParamStr(P+1) = '-build_3_12_2012') then
	begin
		Engine.version := eVerLLBeta03122012;
		Inc(P);
	end;
	
	if ParamCount-P > 0 then
	begin
		ResourcesPath := ParamStr(P+1);
		Inc(P);
	end;
	
	if not FileExists(ResourcesPath + '\textures\textures.bin') then
	begin
		dlg := IupFileDlg;
		IupSetAttribute(dlg, 'TITLE', 'Select the content directory');
		IupSetAttribute(dlg, 'DIALOGTYPE', 'DIR');
		IupPopup(dlg, IUP_CENTER, IUP_CENTER);

		if IupGetInt(dlg, 'STATUS') = 0 then
			ResourcesPath := IupGetAttribute(dlg, 'VALUE');

		IupDestroy(dlg);
	end;

	InitializeEngine;
	
	if texture_params = nil then
	begin
		IupMessageError(nil, 'textures.bin not loaded, check file existance. Application will now close.');
		FinalizeEngine;
		IupClose;
		Exit;
	end;
	
	// отсортируем записи в алфавитном порядке. В принципе для оригинального textures.bin
	// это не нужно, т.к. он уже отсортирован. Однако если файл был отредактирован в ручную
	// то вероятность этого не высока.
	qsort(@texture_params.options[0], Length(texture_params.options), Sizeof(texture_params.options[0]), textures_compar);
	
	CreateDialog;
	IupMainLoop;
	
	FinalizeEngine;
	IupClose;
end.