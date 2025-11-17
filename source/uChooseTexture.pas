unit uChooseTexture;

interface

function ChooseTexture(var filename : String) : Boolean;

implementation
uses Windows, // for GL-context sharing 
     common_texture, Engine, Iup, sysutils, GL, GLU;

// ---- Texture

var
	texture : TResTexture;
	textures_dir : String;
	
function texture_file_cb(ih : Ihandle; filename : PAnsiChar; status : PAnsiChar) : Longint; cdecl;
var
	gl, main_gl : Ihandle;
	fn : String;
	I : Integer;
  
	width, height : Integer;
begin
	if status = 'INIT' then
	begin
		gl := IupGetAttributeHandle(ih, 'PREVIEWGLCANVAS');
		IupGLMakeCurrent(gl);
		
		main_gl := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'GL_CANVAS');
		wglShareLists(HGLRC(IupGetAttribute(main_gl, 'CONTEXT')), HGLRC(IupGetAttribute(gl, 'CONTEXT')));
		
		fn := IupGetAttribute(ih, 'FILE');
		I := Pos(LowerCase(textures_dir), LowerCase(fn));
		if I > 0 then
			fn := ExtractRelativePath(textures_dir, fn); // cut path to meshes folder from file name
      
		// cut extension
		fn := ChangeFileExt(fn, '');
    
		texture := GetTexture(fn);
	end;

	if status = 'FINISH' then
	begin
		if Assigned(texture) then
		begin
			FreeTexture(texture);
			texture := nil;
		end;
	end;
   
	if status = 'SELECT' then
	begin
		if Assigned(texture) then
		begin
			FreeTexture(texture);
			texture := nil;
		end;
    
		fn := filename;
		I := Pos(LowerCase(textures_dir), LowerCase(fn));
		if I > 0 then
			fn := ExtractRelativePath(textures_dir, fn); // cut path to meshes folder from file name
    
		fn := ChangeFileExt(fn, ''); // cut extension
    
		texture := GetTexture(fn);
	end;
  
	if status = 'PAINT' then
	begin
		gl := IupGetAttributeHandle(ih, 'PREVIEWGLCANVAS');
    
		IupGLMakeCurrent(gl);
    
		glClearColor(1.0, 1.0, 1.0, 0.0);
		glClear(GL_COLOR_BUFFER_BIT);
    
		width := IupGetInt(ih, 'PREVIEWWIDTH');
		height := IupGetInt(ih, 'PREVIEWHEIGHT');
    
		glViewport(0, 0, width, height);
    
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity;
		gluOrtho2D(0, width, height, 0);
    
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity;
    
		if Assigned(texture) then
		begin
			glEnable(GL_TEXTURE_2D);
			glBindTexture(GL_TEXTURE_2D, texture.texture);
			
			glBegin(GL_QUADS);
			glTexCoord2f(1.0, 0.0);
			glVertex2f(height, 0.0);
			glTexCoord2f(0.0, 0.0);
			glVertex2f(0.0, 0.0);
			glTexCoord2f(0.0, 1.0);
			glVertex2f(0.0, height);
			glTexCoord2f(1.0, 1.0);
			glVertex2f(height, height);
			glEnd;
			
			glDisable(GL_TEXTURE_2D);
		end;
    
		IupGLSwapBuffers(gl);
	end;
  
	Result := IUP_DEFAULT;
end;

function ChooseTexture(var filename : String) : Boolean;
var
	dlg, gl : Ihandle;
	fn : String;
	
	I : Integer;
begin
	textures_dir := ExpandFileName(ResourcesPath + '\textures\');

	fn := textures_dir + filename + '.512';
	if not FileExists(fn) then
		fn := textures_dir + filename + '.512c';
	if not FileExists(fn) then
		fn := textures_dir + filename + '.dds';
	if not FileExists(fn) then
		fn := '';

	dlg := IupFileDlg;
	IupSetAttribute(dlg, 'DIALOGTYPE', 'OPEN');
	IupSetAttribute(dlg, 'EXTFILTER', 'Texture (*.dds, *.512, *.512c)|*.dds;*.512;*.512c|All files (*.*)|*.*');
  
	if fn <> '' then
		iup.SetStrAttribute(dlg, 'FILE', fn)
	else
		iup.SetStrAttribute(dlg, 'DIRECTORY', ResourcesPath + '\textures');
  
	gl := IupGLCanvas(nil);
	IupSetAttribute(gl, 'BUFFER', 'DOUBLE');
  
	IupSetAttribute(dlg, 'SHOWPREVIEW', 'YES');
	IupSetAttributeHandle(dlg, 'PREVIEWGLCANVAS', gl);
  
	IupSetCallback(dlg, 'FILE_CB', @texture_file_cb);
  
	IupPopup(dlg, IUP_CURRENT, IUP_CURRENT);
  
	if IupGetInt(dlg, 'STATUS') = 0 then
	begin
		fn := IupGetAttribute(dlg, 'VALUE');
		I := Pos(LowerCase(textures_dir), LowerCase(fn));
		if I > 0 then
		begin
			// cut path to meshes folder from file name
			fn := ExtractRelativePath(textures_dir, fn);
			// cut extension
			fn := ChangeFileExt(fn, '');
			
			filename := fn;
			Result := True;
		end else
			Result := False;  
	end else
		Result := False;
	
	IupDestroy(dlg);
	IupDestroy(gl);
end;

end.
