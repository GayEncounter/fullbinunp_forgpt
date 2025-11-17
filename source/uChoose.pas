unit uChoose;

interface
uses uEntity, skeleton;

function ChooseModel(var filename : String) : Boolean;
function ChooseEntity(out entity : TEntity; showIds : Boolean = False) : Boolean;

function ChooseBone(skeleton : T4ASkeleton; var bone : String) : Boolean;
function ChooseLocator(skeleton : T4ASkeleton; var locator : String) : Boolean;
function ChooseBonePart(skeleton : T4ASkeleton; var bone_part : String) : Boolean;
function ChooseAnimation(skeleton : T4ASkeleton; var anim : String) : Boolean;

function ChooseParticles(var particles : String) : Boolean;

function ChooseCameraTrack(var camera_track : String) : Boolean;

implementation
uses Windows, // for GL-context sharing 
     common, vmath, sysutils, classes, Engine,
     uScene, fouramdl,
     Iup, GL, GLU,
     framework, Konfig;

// ---- Model

var
	model : TResModel;
	
	anglex, angley : Single;
	distance : Single = 1.0;
	gl_font : GLuint;

	meshes_dir : String;
	material_set : String;

function model_file_cb(ih : Ihandle; filename : PAnsiChar; status : PAnsiChar) : Longint; cdecl;
var
	gl, main_gl : Ihandle;
	fn : String;
	I : Longint;
	
	width, height : Longint;
	
	sinx, cosx : Single;
	siny, cosy : Single;
	
	m : TMatrix;
	str : String;
	
	mtlset_id : Longint;
begin
	if status = 'INIT' then
	begin	
		gl := IupGetAttributeHandle(ih, 'PREVIEWGLCANVAS');
		IupGLMakeCurrent(gl);
		
		main_gl := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'GL_CANVAS');
		wglShareLists(HGLRC(IupGetAttribute(main_gl, 'CONTEXT')), HGLRC(IupGetAttribute(gl, 'CONTEXT')));
		
		gl_font := glGenLists(256);
		IupGLUseFont(gl, 0, 256, gl_font);
		
		fn := IupGetAttribute(ih, 'FILE');
		I := Pos(LowerCase(meshes_dir), LowerCase(fn));
		if I > 0 then
		begin
			// cut path to meshes folder from file name
			fn := ExtractRelativePath(meshes_dir, fn);
			// cut extension
			fn := ChangeFileExt(fn, '');
		end;
		
		model := GetModel(fn);
	end;
	
	if status = 'FINISH' then
	begin
		gl := IupGetAttributeHandle(ih, 'PREVIEWGLCANVAS');
		IupGLMakeCurrent(gl);
		
		glDeleteLists(gl_font, 256);
		
		if Assigned(model) then
		begin
			if model.GetMaterialSet(material_set) < 0 then
				material_set := '';
			
			FreeModel(model);
			model := nil;
		end;
	end;
	
	if status = 'SELECT' then
	begin
		if Assigned(model) then
		begin
			FreeModel(model);
			model := nil;
		end;
		
		fn := filename;
		I := Pos(LowerCase(meshes_dir), LowerCase(fn));
		if I > 0 then
		begin
			// cut path to meshes folder from file name
			fn := ExtractRelativePath(meshes_dir, fn);
			// cut extension
			fn := ChangeFileExt(fn, '');
		end;
		
		model := GetModel(fn);
	end;
	
	if status = 'PAINT' then
	begin
		gl := IupGetAttributeHandle(ih, 'PREVIEWGLCANVAS');
		IupGLMakeCurrent(gl);
		
		glEnable(GL_DEPTH_TEST);
		glEnable(GL_CULL_FACE);
		
		glAlphaFunc(GL_GEQUAL, 0.5);
		glFrontFace(GL_CW);
		
		glClearColor(bkg_color.x, bkg_color.y, bkg_color.z, bkg_color.w);
		glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
		
		width := IupGetInt(ih, 'PREVIEWWIDTH');
		height := IupGetInt(ih, 'PREVIEWHEIGHT');
		
		glViewport(0, 0, width, height);
		
		glMatrixMode(GL_PROJECTION);    
		PerspectiveLH(m, 70*(PI/180), width/height, 0.1, 500.0);
		glLoadMatrixf(@m);
		
		glMatrixMode(GL_MODELVIEW);
		sinx := Sin(anglex * (PI/180));
		cosx := Cos(anglex * (PI/180));
		siny := Sin(angley * (PI/180));
		cosy := Cos(angley * (PI/180));
		
		LookAtLH(m, distance*siny*cosx, distance*sinx, distance*cosy*cosx, 0, 0, 0, 0, 1, 0);
		glLoadMatrixf(@m);
		
		mtlset_id := -1;
		
		if Assigned(model) then
		begin
			mtlset_id := model.GetMaterialSet(material_set);
			
			if Assigned(model.maler) then
			begin
				model.maler.Draw(mtlset_id, False, False);
				model.maler.Draw(mtlset_id, False, True);
			end;
		end;
		
		glDisable(GL_CULL_FACE);
		
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity;
		gluOrtho2D(0, width, height, 0);
		
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity;
		
		if (material_set <> '') and (mtlset_id >= 0) then
			str := 'Material Set: ' + material_set
		else
			str := 'Material Set: <none>';
		
		glRasterPos2i(10, 20);
		glListBase(gl_font);
		glCallLists(Length(str), GL_UNSIGNED_BYTE, @str[1]);
		
		IupGLSwapBuffers(gl);
	end;
	
	Result := IUP_DEFAULT;
end;

var
	old_x, old_y : Longint;

function model_motion_cb(ih : Ihandle; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
begin
	if iup_isbutton3(status) then
	begin
		anglex := anglex + (y - old_y);
		angley := angley + (x - old_x);
		
		if anglex >= 90.0 then
		anglex := 90.0;
		if anglex <= -90.0 then
		anglex := -90.0;
		
		model_file_cb(ih, nil, 'PAINT');
	end else
	if iup_isbutton2(status) then
	begin
		distance := distance + ((y - old_y) / 8);
		model_file_cb(ih, nil, 'PAINT');
	end;
	
	old_x := x;
	old_y := y;
	
	Result := IUP_DEFAULT;
end;

function model_button_cb(ih : Ihandle; button, pressed, x, y : Longint; status : PAnsiChar) : Longint; cdecl;
var
	m : T4AModelMtlsets;
	I, sel : Longint;
	names : array of String;
begin
	if iup_isdouble(status) then
		if Assigned(model) and (model.model is T4AModelMtlsets) then
		begin
			m := T4AModelMtlsets(model.model);
			
			SetLength(names, Length(m.mtlsets)+1);
			names[0] := '<none>';
			
			sel := 1;
			
			for I := 0 to Length(m.mtlsets)-1 do
			begin
				names[I+1] := m.mtlsets[I].name;
				
				if m.mtlsets[I].name = material_set then
					sel := I+2;
			end;
			
			I := iup.ListDialog('Select Material Set', names, sel, 25, 35);
			if I >= 0 then
			begin
				if I = 0 then
					material_set := ''
				else
					material_set := names[I];
				
				model_file_cb(ih, nil, 'PAINT');
			end;
		end;

	Result := IUP_DEFAULT;
end;

function ChooseModel(var filename : String) : Boolean;
var
	dlg, gl : Ihandle;
	fn : String;
	
	I : Longint;
begin
	meshes_dir := ExpandFileName(ResourcesPath + '\meshes\');
	
	// cut material set name
	I := Pos('@', filename);
	if I > 0 then
	begin
		fn := Copy(filename, 1, I-1);
		material_set := Copy(filename, I+1);
	end else
	begin
		fn := filename;
		material_set := '';
	end;

	dlg := IupFileDlg;
	IupSetAttribute(dlg, 'DIALOGTYPE', 'OPEN');
	iup.SetStrAttribute(dlg, 'FILE', meshes_dir + fn + '.model');
	IupSetAttribute(dlg, 'EXTFILTER', '4A Model (*.model)|*.model|All files (*.*)|*.*');
	
	gl := IupGLCanvas(nil);
	IupSetAttribute(gl, 'BUFFER', 'DOUBLE');
	
	IupSetAttribute(dlg, 'SHOWPREVIEW', 'YES');
	IupSetAttributeHandle(dlg, 'PREVIEWGLCANVAS', gl);
	
	IupSetCallback(dlg, 'FILE_CB', @model_file_cb);
	IupSetCallback(dlg, 'MOTION_CB', @model_motion_cb);
	IupSetCallback(dlg, 'BUTTON_CB', @model_button_cb);
	
	IupPopup(dlg, IUP_CURRENT, IUP_CURRENT);

	if IupGetInt(dlg, 'STATUS') = 0 then
	begin
		fn := IupGetAttribute(dlg, 'VALUE');
		I := Pos(LowerCase(meshes_dir), LowerCase(fn));
		if I > 0 then
		begin
			// cut path to meshes folder from file name
			fn := ExtractRelativePath(meshes_dir, fn);
			// cut extension
			fn := ChangeFileExt(fn, '');
			// append material set (if any)
			if material_set <> '' then
				fn := fn + '@' + material_set;
			
			filename := fn;
			Result := True;
		end else
			Result := False;
	end else
		Result := False;

	IupDestroy(dlg);
	IupDestroy(gl);
end;

// ---- Entity

function entity_compare_name(e1, e2 : Pointer) : Longint;
begin
	entity_compare_name := AnsiCompareStr(TEntity(e1).Name, TEntity(e2).Name);
end;

function entity_compare_id(e1, e2 : Pointer) : Longint;
begin
	entity_compare_id := TEntity(e1).ID - TEntity(e2).ID;
end;

function ChooseEntity(out entity : TEntity; showIds : Boolean) : Boolean;
var
	I, ret : Integer;
	
	sort_entities : TList;
	names : array of String;
begin
	sort_entities := TList.Create;
	sort_entities.AddList(Scene.entities);
	
	if showIds then
		sort_entities.Sort(entity_compare_id)
	else
		sort_entities.Sort(entity_compare_name);
	
	SetLength(names, sort_entities.Count+1);
	names[0] := '<none>';
	
	for I := 0 to sort_entities.Count - 1 do
	begin
		names[I+1] := TEntity(sort_entities[I]).Name;
		if showIds then
			Insert(IntToStr(TEntity(sort_entities[I]).ID) + ': ', names[I+1], 1);
	end;
	
	ret := iup.ListDialog('Select entity', names, 1, 30, 40);
	if ret >= 0 then
	begin
		if ret = 0 then
			entity := nil
	else
		entity := TEntity(sort_entities[ret-1]);
	
		Result := True;
	end else
		Result := False;
	
	sort_entities.Free;
end;

// ---- Bone

function ChooseBone(skeleton : T4ASkeleton; var bone : String) : Boolean;
var
	count : Longint;
	names : array of String;
	ret : Longint;
	I : Longint;
	op : Longint;
begin
	count := Length(skeleton.bones);
	
	SetLength(names, count+1);
	names[count] := '<none>';
		
	op := 0;
		
	for I := 0 to count-1 do
	begin
		names[I] := skeleton.bones[I].name;
		
		if skeleton.bones[I].name = bone then
			op := I+1;
	end;
	
	if op = 0 then
		op := count+1;

	ret := iup.ListDialog('Select bone', names, op, 15, 20);
	if ret <> -1 then
	begin
		if ret = count then
			bone := ''
		else
			bone := names[ret];
			
		ChooseBone := True;
	end else
		ChooseBone := False;
end;

// ---- Locator

function ChooseLocator(skeleton : T4ASkeleton; var locator : String) : Boolean;
var
	count : Longint;
	names : array of String;
	ret : Longint;
	I : Longint;
	op : Longint;
begin
	count := Length(skeleton.bones)+Length(skeleton.locators);
	
	SetLength(names, count+1);
	names[count] := '<none>';
	
	op := 0;
		
	for I := 0 to Length(skeleton.bones)-1 do
	begin
		names[I] := skeleton.bones[I].name;
		
		if skeleton.bones[I].name = locator then
			op := I+1;
	end;

	for I := 0 to Length(skeleton.locators)-1 do
	begin
		names[I+Length(skeleton.bones)] := skeleton.locators[I].name;
		
		if skeleton.locators[I].name = locator then
			op := I+1;
	end;
	
	if op = 0 then
		op := count+1;

	ret := iup.ListDialog('Select locator', names, op, 15, 20);
	if ret <> -1 then
	begin
		if ret = count then
			locator := ''
		else
			locator := names[ret];
			
		ChooseLocator := True;
	end else
		ChooseLocator := False;
end;

// ---- Bone part

function ChooseBonePart(skeleton : T4ASkeleton; var bone_part : String) : Boolean;
var
	count : Longint;
	names : array of String;
	ret : Longint;
	I : Longint;
	op : Longint;
begin
	count := Length(skeleton.bone_parts);
	
	SetLength(names, count+1);
	names[count] := '<none>';
		
	op := 0;	
	
	for I := 0 to count-1 do
	begin
		names[I] := skeleton.bone_parts[I].name;
		
		if skeleton.bone_parts[I].name = bone_part then
			op := I+1;
	end;
	
	if op = 0 then
		op := count+1;
	
	ret := iup.ListDialog('Select bone part', names, op, 15, 15);
	if ret <> -1 then
	begin
		if ret = count then
			bone_part := ''
		else
			bone_part := names[ret];
			
		ChooseBonePart := True;
	end else
		ChooseBonePart := False;
end;

// ---- Animation

function ChooseAnimation(skeleton : T4ASkeleton; var anim : String) : Boolean;
var
	I, op, ret : Longint;
	dirs : TStringList;
	anims : TStringList;
	
	mask : String;
	sr : TSearchRec;
	
	name : String;
begin
	dirs := TStringList.Create;
	anims := TStringList.Create;
	
	op := 0;
	
	dirs.CommaText := skeleton.anim_path;
	for I := 0 to dirs.Count - 1 do
	begin
		mask := ResourcesPath + '\motions\' + dirs[I] + '\*.m2';
		if FindFirst(mask, faAnyFile xor faDirectory, sr) = 0 then
		begin
			repeat
				name := ChangeFileExt(sr.Name, '');
				anims.Add(name);
				if name = anim then
					op := anims.Count;
			until FindNext(sr) <> 0;
			
			FindClose(sr);
		end;
		
		mask := ResourcesPath + '\motions\' + dirs[I] + '\*.motion';
		if FindFirst(mask, faAnyFile xor faDirectory, sr) = 0 then
		begin
			repeat
				name := ChangeFileExt(sr.Name, '');
				anims.Add(name);
				if name = anim then
					op := anims.Count;
			until FindNext(sr) <> 0;
			
			FindClose(sr);
		end;
	end;
	
	anims.Add('<none>');
	if op = 0 then
		op := anims.Count;
		
	ret := iup.ListDialog('Select animation', anims, op, 25, 30);
	if ret <> -1 then
	begin
		if ret = anims.Count-1 then
			anim := ''
		else
			anim := anims[ret];
			
		ChooseAnimation := True;
	end else
		ChooseAnimation := False;
		
	anims.Free;
	dirs.Free;
end;

// ---- Particles

function ChooseParticles(var particles : String) : Boolean;
var
	K : TKonfig;
	lib : TTextKonfig;
	js : TFramework;
	systems : TSection;
	name : String;
	names : TStringList;
	ret : Longint;
	I : Longint;
	op : Longint;
begin
	op := 1;
	names := TStringList.Create;
	
	if FileExists(ResourcesPath + '\particles\particles.bin') then
	begin	
		K := TKonfig.Create;
		K.Load(ResourcesPath + '\particles\particles.bin');
		
		js := TFramework.Create;
		lib := js.DecompileKonfig(K, 'js\particles.js');
		js.Free;
		
		K.Free;
		
		if lib <> nil then
		begin
			systems := lib.root.GetSect('systems');	
			for I := 0 to systems.items.Count-1 do
				if TSimpleValue(systems.items[I]) is TSection then
				begin
					name := TSection(systems.items[I]).GetStr('name');
					names.Add(name);
					
					if name = particles then
						op := names.Count;
				end;
				
			lib.Destroy;
		end;
	end;
	
	names.Sort;
	names.Add('<none>');
	
	ret := iup.ListDialog('Select particles', names, op, 25, 40);
	if ret <> -1 then
	begin
		if ret = names.Count-1 then
			particles := ''
		else
			particles := names[ret];
			
		ChooseParticles := True;
	end else
		ChooseParticles := False;
		
	names.Free;
end;

// ---- Camera Track

function ChooseCameraTrack(var camera_track : String) : Boolean;
var
	names : TStringList;
	ret : Longint;
	I : Longint;
	op : Longint;
begin
	op := 1;
	names := TStringList.Create;
	
	names.AddStrings(Engine.GetCameraTracksList);
	names.Sort;
	
	for I := 0 to names.Count-1 do
	begin
		if names[I] = camera_track then
		begin
			op := I+1;
			Break;
		end;
	end;
	
	names.Add('<none>');
	
	ret := iup.ListDialog('Select camera track', names, op, 25, 40);
	if ret <> -1 then
	begin
		if ret = names.Count-1 then
			camera_track := ''
		else
			camera_track := names[ret];
			
		ChooseCameraTrack := True;
	end else
		ChooseCameraTrack := False;
		
	names.Free;
end;

end.
