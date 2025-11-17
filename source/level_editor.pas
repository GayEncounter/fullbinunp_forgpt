program level_Editor;
uses Iup, Windows, GL, GLU, GLExt, sysutils, classes, ActiveX, common_multisample,
	chunkedFile,
	Math, vmath, PhysX, PHGroups,
	Konfig, levelbin,
	common, Engine, Texture,
	Manipulator,
	fouramdl, skeleton,
	uWeather, uEditorUtils,
	navmap_server,
	uTabWeather, uTabEntity, uTabEnvZone, uTabCovers,
	uScene, uSelectable, uEntity, uEnvZone, uSceneCovers, uLevelUndo, uImages,
	{$IFDEF HAZ_LEVELEXPORT} uLevelExport, {$ENDIF}
	{$IFDEF HAZ_XRAYEXPORT} uXRayExport, {$ENDIF}
	uChoose, properties, uTemplates, uAttach, uLEOptions, uLevelRun, uObjectList;
	
type
	TEditMode = (emEntity, emEnvZone, emWeather, emCovers);
	
var
	edit_mode : TEditMode = emEntity;
	m_t : TManipulator;

var
	level_path : String;

	cam_fov : Single = 70.0;
	position : TVec3;
	direction : TVec3;
	anglex, angley : Single;
	distance : Single = 5.0;

	far_plane : Single = 1500.0;
	rt_width, rt_height : GLuint;
	rt_color, rt_distort : GLuint;
	
var
	context_menu : Ihandle;
	anim_timer : Ihandle;
	
function anim_timer_action_cb(ih : Ihandle) : Longint; cdecl;
begin
	Redisplay;
	Result := IUP_DEFAULT;
end;
	
procedure CreateAnimTimer;
begin
	anim_timer := IupTimer;
	IupSetInt(anim_timer, 'TIME', 33);
	IupSetCallback(anim_timer, 'ACTION_CB', @anim_timer_action_cb);
end;
	
var
	mouse_x, mouse_y : Longint;
	
	rclick_x, rclick_y : Longint;
	lclick_x, lclick_y : Longint;
	
	selection_rect : Boolean = False;

procedure CreateManipulator(const matrix : TMatrix);
begin
	if m_mode = mmMove then
	begin
		m_t := TMoveManipulator.Create(m_move_axis = maWorld, lineWidth);
		m_t.Matrix := matrix;
	end;
	if m_mode = mmRotate then
	begin
		m_t := TRotateManipulator.Create(m_rotate_axis in [maWorld,maGroup], lineWidth);
		m_t.Matrix := matrix;
	end;
	if m_mode = mmScale then
	begin
		m_t := TScaleManipulator.Create(m_uniform_scale, lineWidth);
		m_t.Matrix := matrix;
	end;
end;

procedure DestroyManipulator;
begin
	FreeAndNil(m_t);
end;

procedure UpdateManipulator;
var
	selected : TEntityArray;
	z : TEnvZoneArray;
	covers : TCoverArray;
	c : TVec3;
	m : TMatrix;
begin
	DestroyManipulator;
	
	case edit_mode of
	
		emEntity: begin
			selected := Scene.GetSelected;
			
			if Length(selected) > 0 then
			begin
				if Length(selected) = 1 then
				begin
					CreateManipulator(selected[0].Matrix);
				end else
				begin
					c := GetCenter(selected);
					Translate(m, c);
					CreateManipulator(m);
				end;
			end;
		end;
		
		emEnvZone: begin
			z := Scene.GetSelectedEZ;
			
			if Length(z) > 0 then
			begin
				c := GetCenter(z);
				Translate(m, c);
				CreateManipulator(m);
			end;
		end;
		
		emCovers: begin
			covers := Scene.GetSelectedCovers;
			
			if Length(covers) > 0 then
			begin
				if Length(covers) = 1 then
				begin
					CreateManipulator(covers[0].Matrix);
				end else
				begin
					c := GetCenter(covers);
					Translate(m, c);
					CreateManipulator(m);
				end;
			end;
		end;
		
	end;
end;

procedure UpdateSelection;
var
	sel_c : Ihandle;
	count : Longint;
begin
	UpdateManipulator;
	
	count := 0;
	
	case edit_mode of
		emEntity: begin
			count := Length(Scene.GetSelected);
			uTabEntity.UpdateTab;
		end;
		
		emEnvZone: begin
			count := Length(Scene.GetSelectedEZ);			
			uTabEnvZone.UpdateTab;
		end;
		
		emCovers: begin
			count := Length(Scene.GetSelectedCovers);
			uTabCovers.UpdateTab;
		end;
	end;
	
	// update selection count
	sel_c := IupGetDialogChild(MainDialog, 'LABEL_SEL');
	iup.SetStrAttribute(sel_c, 'TITLE', 'Sel: ' + IntToStr(count));
end;

procedure DeselectAll;
begin
	case edit_mode of
		emEntity:  uTabEntity.DeselectAll;
		emEnvZone: uTabEnvZone.DeselectAll;
		emCovers:  uTabCovers.DeselectAll;
	end;
end;

procedure DeleteSelection;
begin
	case edit_mode of
		emEntity: uTabEntity.DeleteSelection;
		emEnvZone: uTabEnvZone.DeleteSelection;
		emCovers: uTabCovers.DeleteSelection;
	end;
	
	UpdateSelection;
end;

procedure RecursiveSelect(parent_id : Word);
var
	I : Longint;
	e : TEntity;
begin
	for I := 0 to Scene.entities.Count-1 do
	begin
		e := TEntity(scene.entities[I]);
		if e.ParentID = parent_id then
		begin
			e.selected := True;
			RecursiveSelect(e.ID);
		end;
	end;
end;

procedure AttachSelectionTo(parent : TEntity);
var
	sel : TList;
begin
	sel := Scene.GetSelectedList;
	
	if sel.Count > 0 then
	begin
		UndoSave('Attach entities', Scene.GetSelected);
		AttachEntities(sel, parent)
	end else
		IupMessage('Error', 'Nothing selected to attach');
		
	Scene.UpdateAttaches; // for test
		
	sel.Free;
end;

procedure AttachShapesTo(parent : TEntity);
var
	I : Longint;
	sel : TEntityArray;
begin
	sel := Scene.GetSelected;
	
	if Length(sel) > 0 then
	begin
		UndoSave('Attach shapes');
	
		for I := 0 to Length(sel)-1 do
			if AddShapes(parent, sel[I]) then
				Scene.RemoveEntity(sel[I]);
				
		UpdateSelection;
	end;
end;

procedure AttachEnvZoneTo(parent : TEnvZone);
var
	I : Longint;
	sel : TEnvZoneArray;
begin
	sel := Scene.GetSelectedEZ;
	
	if Length(sel) > 0 then
	begin
		UndoSaveEnv('Attach env zones');
		
		for I := 0 to Length(sel)-1 do
			parent.AddQuads(sel[I].param_tris.data);
			
		uTabEnvZone.DeleteSelection;		
		UpdateSelection;
	end;
end;

procedure RemoveEnvZoneQuad(zone : TEnvZone; id : Longint);
begin
	zone.RemoveQuad(id);
	
	if Length(zone.param_tris.data) = 0 then
	begin
		Scene.env_zones.Remove(zone);
		zone.Free;
	end;
	
	UpdateSelection;
end;

procedure MakeCoverLink(dest : TCover);
var
	sel : TCoverArray;
	I : Longint;
begin
	sel := Scene.GetSelectedCovers;
	
	for I := 0 to Length(sel) - 1 do
		if (sel[I].ID <> dest.ID) and (Scene.cover_manager.FindLink(sel[I].ID, dest.ID) = nil) then
			uTabCovers.CreateLink(sel[I], dest);
end;

procedure LoadMap(const dir : String);
var
	old_scene_ver : TSceneVersion;
	navmap_w : TMemoryWriter;
	
	I, J : Longint;
	ok : Boolean;
begin
	old_scene_ver := Scene.GetVersion;

	DestroyManipulator;
	Scene.LevelUnload;
	
	ClearResources;
	UndoClearHistory;

	LevelPath := dir; // in Engine.pas, for resource loading
	level_path := dir;
	
	IupSetAttribute(MainDialog, 'ACTIVE', 'NO');
	if TEntity.showAnimation then IupSetAttribute(anim_timer, 'RUN', 'NO');
	
	Scene.LevelLoad(dir);
	
	IupSetAttribute(MainDialog, 'ACTIVE', 'YES');
	if TEntity.showAnimation then IupSetAttribute(anim_timer, 'RUN', 'YES');
	
	ResetWeather;
	
	// load templates
	SaveTemplates;
		
	if Scene.GetVersion <> old_scene_ver then
	begin
		case Scene.GetVersion of
			sceneVer2033: 
				LoadTemplates('editor_data\templates.txt');
			sceneVerLL: begin
				case Engine.version of
					eVerLLBeta15102012:
						LoadTemplates('editor_data\templates_ll_beta_15102012.txt');
					eVerLLBeta03122012:
						LoadTemplates('editor_data\templates_ll_beta_03122012.txt');  
					else
						LoadTemplates('editor_data\templates_ll.txt');
				end;  
			end;
			sceneVerRedux:
				LoadTemplates('editor_data\templates_redux.txt');
			sceneVerArktika1:
				LoadTemplates('editor_data\templates_a1.txt');
			sceneVerExodus:
				LoadTemplates('editor_data\templates_exodus.txt');
		end;
	end;
	
	//
	uTabEnvZone.UpdateTab;
	
	// update navmap server
	if Scene.navmap <> nil then
	begin
		navmap_w := TMemoryWriter.Create;
		
		Scene.navmap.SaveRAW(navmap_w);
		navmap_server.LoadNavmapRaw(navmap_w.data, navmap_w.size);
		
		J := 0;
		SetLength(Scene.navmap.end_points_pos, Length(Scene.navmap.end_points));
		for I := 0 to Length(Scene.navmap.end_points) - 1 do
		begin
			ok := navmap_server.Get3dPos(
				Scene.navmap.end_points[I].x,
				Scene.navmap.end_points[I].y,
				Scene.navmap.end_points[I].cell,
				Scene.navmap.end_points_pos[I]
			);
			Inc(J, Integer(ok));
		end;
		
		WriteLn('end-points: ', J, ' ok ',  Length(Scene.navmap.end_points)-J, ' fail');
		
		navmap_w.Free;
	end;
	
	UpdateTemplates(IupGetDialogChild(MainDialog, 'TREE_TEMPLATES'));
	iup.SetStrAttribute(MainDialog, 'TITLE', 'Level Editor - [' + dir + ']');

	UpdateSelection;
	Redisplay;
end;

procedure SaveMap;
var
	kind : Byte;
begin
	if Scene.konf <> nil then
	begin
		case Scene.GetVersion of
			sceneVer2033: kind := konfDebugInfo or konfDiktionary;
			sceneVerLL:   kind := konfDiktionary;
			else          kind := konfDiktionary or konfMultiChunk;
		end;
		
		// save UCovers
		Scene.cover_manager.Save(level_path);
		
		// save Human Combat Covers
		if Scene.GetVersion = sceneVer2033 then
		begin
			try
				Scene.cover_manager.SaveHumanCombatCovers2033(level_path + '\nav_map.human_combat_covers.pc');
			except on E: Exception do
				WriteLn('Can''t save human combat covers (' + E.ClassName + ': ' + E.Message + ')');
			end;
		end;
		
		// save level.bin and level.add.bin
		SaveLevelBin(level_path + '\level.bin', Scene.konf, kind);
		if Scene.konf_add <> nil then
			SaveLevelBin(level_path + '\level.add.bin', Scene.konf_add, kind);
			
		// save Environment Zones
		Scene.SaveEnvironmentToFile(level_path + '\level.environment');
	end;
end;

procedure UpdateCameraPos;
var
	ih : Ihandle;
	x, y, z : String[32];
begin
	ih := IupGetDialogChild(MainDialog, 'LABEL_CAMPOS');

	WriteStr(x, position.x:1:3);
	WriteStr(y, position.y:1:3);
	WriteStr(z, position.z:1:3);

	iup.SetStrAttribute(ih, 'TITLE', 'Cam pos: ' + x + ' ' + y + ' ' + z);
end;

///////////////////////////////////////////////////////////////////////////////
// OpenGL canvas callbacks                                                   //
///////////////////////////////////////////////////////////////////////////////
function gl_map_cb(ih : Ihandle) : Longint; cdecl;
var
	w_handle : HWND;
	dc_handle : HDC;
	gl_context : HGLRC;
	
	pfd : PIXELFORMATDESCRIPTOR;
	iPixelFmt : Longint;
begin
	// Create an OpenGL context
	
	WriteLn('Creating main OpenGL context...');
	
	w_handle := HWND(IupGetAttribute(ih, 'WID'));
	dc_handle := GetDC(w_handle);
	
	pfd.nSize           := Sizeof(pfd);
	pfd.nVersion        := 1;
	pfd.dwFlags         := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
	pfd.iPixelType      := PFD_TYPE_RGBA;
	pfd.cColorBits      := 32;
	pfd.cRedBits        := 8;
	pfd.cRedShift       := 0;
	pfd.cGreenBits      := 8;
	pfd.cGreenShift     := 8;
	pfd.cBlueBits       := 8;
	pfd.cBlueShift      := 16;
	pfd.cAlphaBits      := 0;
	pfd.cAlphaShift     := 0;
	pfd.cAccumBits      := 0;
	pfd.cAccumRedBits   := 0;
	pfd.cAccumGreenBits := 0;	
	pfd.cAccumBlueBits  := 0;
	pfd.cAccumAlphaBits := 0;
	pfd.cDepthBits      := 32;
	pfd.cStencilBits    := 0;
	pfd.cAuxBuffers     := 0;
	pfd.iLayerType      := PFD_MAIN_PLANE;
	pfd.bReserved       := 0;
	pfd.dwLayerMask     := 0;
	pfd.dwVisibleMask   := 0;
	pfd.dwDamageMask    := 0;
	
	if uLEOptions.num_msaa_samples > 1 then
	begin
		WriteLn('Requesting multisample pixel format (', uLEOptions.num_msaa_samples, ') samples...');
		iPixelFmt := ChooseMultisamplePixelFormat(uLEOptions.num_msaa_samples);
		
		if iPixelFmt = 0 then
		begin
			WriteLn('Requesting multisample pixel format failed');
			iPixelFmt := ChoosePixelFormat(dc_handle, @pfd);
		end;
	end else
		iPixelFmt := ChoosePixelFormat(dc_handle, @pfd);
	
	if iPixelFmt = 0 then
		WriteLn('Unable to choose appropriate pixel format')
	else
		WriteLn('Pixel format ', iPixelFmt, ' was chosen');
		
	if not SetPixelFormat(dc_handle, iPixelFmt, @pfd) then
		WriteLn('SetPixelFormat FAILED !')
	else
		WriteLn('... and set');
	
	gl_context := wglCreateContext(dc_handle);
	
	if gl_context = 0 then
		WriteLn('wglCreateContext FAILED !')
	else
		WriteLn('Context created (', IntToHex(Longword(gl_context), 8), ')');
		
	// set attributes in IupGLContext-like way
	IupSetAttribute(ih, 'VISUAL', Pointer(dc_handle));
	IupSetAttribute(ih, 'CONTEXT', PAnsiChar(gl_context));
	
	if not wglMakeCurrent(dc_handle, gl_context) then
		WriteLn('wglMakeCurrent FAILED !');

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glAlphaFunc(GL_GEQUAL, 0.5);
	glFrontFace(GL_CW);

	InitializeRender;
	
	glGenTextures(1, @rt_color);
	glBindTexture(GL_TEXTURE_2D, rt_color);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	
	glGenTextures(1, @rt_distort);
	glBindTexture(GL_TEXTURE_2D, rt_distort);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	
	glBindTexture(GL_TEXTURE_2D, 0);	

	Result := IUP_DEFAULT;
end;

function gl_unmap_cb(ih : Ihandle) : Longint; cdecl;
var
	w_handle : HWND;
	dc_handle : HDC;
	gl_context : HGLRC;
begin
	w_handle := HWND(IupGetAttribute(ih, 'WID'));
	dc_handle := HDC(IupGetAttribute(ih, 'VISUAL'));
	gl_context := HGLRC(IupGetAttribute(ih, 'CONTEXT'));
	
	if wglGetCurrentContext() = gl_context then
		wglMakeCurrent(0, 0);
		
	wglDeleteContext(gl_context);
	ReleaseDC(w_handle, dc_handle);
	
	Result := IUP_DEFAULT;
end;

function gl_resize_cb(ih : Ihandle; w, h : Longint) : Longint; cdecl;
var
	aspect : Single;
	m : TMatrix;
	
	dc_handle : HDC;
	gl_context : HGLRC;
begin
	dc_handle := HDC(IupGetAttribute(ih, 'VISUAL'));
	gl_context := HGLRC(IupGetAttribute(ih, 'CONTEXT'));
	if not wglMakeCurrent(dc_handle, gl_context) then
		WriteLn('wglMakeCurrent FAILED !');
	
	glViewport(0, 0, w, h);

	aspect := w / h;

	glMatrixMode(GL_PROJECTION);
	PerspectiveLH(m, cam_fov*(PI/180), aspect, 0.25, far_plane);
	glLoadMatrixf(@m);
	glMatrixMode(GL_MODELVIEW);
	
	glBindTexture(GL_TEXTURE_2D, rt_color);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, w, h, 0, GL_RGB, GL_UNSIGNED_BYTE, nil);

	glBindTexture(GL_TEXTURE_2D, rt_distort);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, w, h, 0, GL_RGB, GL_UNSIGNED_BYTE, nil);
	
	glBindTexture(GL_TEXTURE_2D, 0);	
	
	rt_width := w;
	rt_height := h;

	Result := IUP_DEFAULT;
end;

procedure UpdateFrustum;
var
	vp : TMatrix;
begin
	vp := proj;
	Mul44(vp, modelview);
	FrustumFromMatrix(frustum, vp);
end;

function gl_redraw_cb(ih : Ihandle; x, y : Single) : Longint; cdecl;
var
	m : TMatrix;
	
	dc_handle : HDC;
	gl_context : HGLRC;
begin
	dc_handle := HDC(IupGetAttribute(ih, 'VISUAL'));
	gl_context := HGLRC(IupGetAttribute(ih, 'CONTEXT'));
	
	if not wglMakeCurrent(dc_handle, gl_context) then
	begin
		WriteLn('wglMakeCurrent FAILED !');
		Result := IUP_DEFAULT;
		Exit;
	end;
	
	glClearColor(bkg_color.x, bkg_color.y, bkg_color.z, bkg_color.w);
	glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
	
//
	glMatrixMode(GL_PROJECTION);
	PerspectiveLH(m, cam_fov*(PI/180), viewport[2] / viewport[3], 0.25, far_plane);
	glLoadMatrixf(@m);
	//glLoadIdentity;
	//gluPerspective(cam_fov, viewport[2] / viewport[3], 0.25, far_plane);
	glMatrixMode(GL_MODELVIEW);
//

	glLoadIdentity;
	glRotatef(anglex, 1, 0, 0);
	glRotatef(angley, 0, 1, 0);
	glTranslatef(-position.x, -position.y, -position.z);
	
	if useWeather then
	begin
		glPushMatrix;
		
		glTranslatef(position.x, position.y, position.z);
		uWeather.RenderSky;
		
		glPopMatrix;
	end;

	glGetDoublev(GL_MODELVIEW_MATRIX,  @modelview_d);
	glGetDoublev(GL_PROJECTION_MATRIX, @proj_d);
	glGetFloatv(GL_MODELVIEW_MATRIX,  @modelview);
	glGetFloatv(GL_PROJECTION_MATRIX, @proj);
	glGetIntegerv(GL_VIEWPORT, @viewport);

	UpdateFrustum;
	uWeather.Update;
	
	camera_pos.x := position.x;
	camera_pos.y := position.y;
	camera_pos.z := position.z;
	
	Scene.RenderPrepare;
	
	if useWireframe then
		glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	
	Scene.RenderOpaqueFast;
	
	if Scene.showWays then
		Scene.RenderWays;
	
	glDepthMask(GL_FALSE);
	Scene.RenderBlended;
	glDepthMask(GL_TRUE);
	
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	
	if Scene.showEnvZones or (edit_mode = emEnvZone) then
		Scene.RenderEnvZones;
	
	glReadBuffer(GL_BACK_LEFT);
	glBindTexture(GL_TEXTURE_2D, rt_color);
	glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 0, 0, rt_width, rt_height, 0);
	glBindTexture(GL_TEXTURE_2D, 0);
	
	glClearColor(0.5, 0.5, 0.0, 0.0);
	glClear(GL_COLOR_BUFFER_BIT);
	
	Scene.RenderDistort;
	
	glReadBuffer(GL_BACK_LEFT);
	glBindTexture(GL_TEXTURE_2D, rt_distort);
	glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 0, 0, rt_width, rt_height, 0);
	glBindTexture(GL_TEXTURE_2D, 0);
	
////
	glDisable(GL_DEPTH_TEST);
////
	
	// begin 2d drawing
	glMatrixMode(GL_PROJECTION);
	glPushMatrix;
	glLoadIdentity;
	gluOrtho2D(0.0, 1.0, 1.0, 0.0);
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix;
	glLoadIdentity;
	
	// draw screen image with distort
	glActiveTexture(GL_TEXTURE0);
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, rt_color);

	glActiveTexture(GL_TEXTURE1);
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, rt_distort);
	
	glEnable(GL_FRAGMENT_PROGRAM_ARB);
	glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog[FP_SCREEN_IMAGE]);
	
	glBegin(GL_QUADS);
	
	glTexCoord2f(0.0, 1.0);
	glVertex3f(0.0, 0.0, 0.0);
	glTexCoord2f(1.0, 1.0);
	glVertex3f(1.0, 0.0, 0.0);
	glTexCoord2f(1.0, 0.0);
	glVertex3f(1.0, 1.0, 0.0);
	glTexCoord2f(0.0, 0.0);
	glVertex3f(0.0, 1.0, 0.0);
	
	glEnd;
	
	glDisable(GL_FRAGMENT_PROGRAM_ARB);
	glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
	
	glActiveTexture(GL_TEXTURE1);
	glDisable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, 0);
	glActiveTexture(GL_TEXTURE0);
	glDisable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, 0);
	
	// draw selection rect
	if (mouse_x <> lclick_x) and (mouse_y <> lclick_y) and selection_rect then
	begin
		glBegin(GL_LINE_LOOP);
		glVertex3f(lclick_x / viewport[2], lclick_y / viewport[3], 0);
		glVertex3f(mouse_x / viewport[2], lclick_y / viewport[3], 0);
		glVertex3f(mouse_x / viewport[2], mouse_y / viewport[3], 0);
		glVertex3f(lclick_x / viewport[2], mouse_y / viewport[3], 0);
		glEnd;
	end;
	
	// end 2d drawing
	glMatrixMode(GL_PROJECTION);
	glPopMatrix;
	glMatrixMode(GL_MODELVIEW);
	glPopMatrix;

	// draw manipulator (if any)
	if m_t <> nil then
		m_t.Draw;
	
//
	glEnable(GL_DEPTH_TEST);
//

	SwapBuffers(dc_handle);
	Result := IUP_DEFAULT;
end;

procedure ApplyManipulator(var dest : TMatrix; const c : TVec3);
var
	mat, mat2 : TMatrix;	
	diff : TMatrix;	
begin
	if m_mode = mmMove then
	begin
		if m_move_axis = maObject then
			Mul44(dest, m_t.Diff)
		else begin
			dest[4,1] := dest[4,1] + m_t.Diff[4,1];
			dest[4,2] := dest[4,2] + m_t.Diff[4,2];
			dest[4,3] := dest[4,3] + m_t.Diff[4,3];
		end;
	end;
	
	if m_mode = mmRotate then
	begin
		if m_rotate_axis = maGroup then
		begin
			mat := dest;
			mat[4,1] := mat[4,1] - c.x;
			mat[4,2] := mat[4,2] - c.y;
			mat[4,3] := mat[4,3] - c.z;
			diff := m_t.Diff;
			//WriteMatrix(diff);
			Mul44(diff, mat);
			mat := diff;
			mat[4,1] := mat[4,1] + c.x;
			mat[4,2] := mat[4,2] + c.y;
			mat[4,3] := mat[4,3] + c.z;
			
			dest := mat;
		end else
		if m_rotate_axis = maWorld then
		begin
			mat := dest;
			mat2 := dest;
			mat2[4,1] := 0.0; mat2[4,2] := 0.0; mat2[4,3] := 0.0;
			
			dest := m_t.Diff;
			Mul44(dest, mat2);
			
			dest[4,1] := mat[4,1]; dest[4,2] := mat[4,2]; dest[4,3] := mat[4,3];
		end else
		begin
			Mul44(dest, m_t.Diff);
		end;
	end;
	
	if m_mode = mmScale then
	begin
		Mul44(dest, m_t.Diff);
	end;
end;

function gl_motion_cb(ih : Ihandle; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
var
	mousexy : Ihandle;
	pos : String;
	
	sinh, cosh : Single;
	
	alt : Boolean;
	btn1 : Boolean;
	btn2 : Boolean;
	btn3 : Boolean;
	
	I : Longint;
	selected : TEntityArray;
	z : TEnvZoneArray;
	c, cc : TVec3;
	mat : TMatrix;
	e : TEntity;
	
	covers : TCoverArray;
	cover : TCover;
begin
	pos := IntToStr(x) + ', ' + IntToStr(y);
	mousexy := IupGetDialogChild(ih, 'MOUSEXY');
	iup.SetStrAttribute(mousexy, 'TITLE', pos);

	alt := iup_isalt(status);
	btn1 := iup_isbutton1(status);
	btn2 := iup_isbutton2(status);
	btn3 := iup_isbutton3(status);

	if (alt and btn1 and btn3) or (not alt and btn3) then // крутить
	begin
		anglex := anglex - camera_rotate_sens * (y - mouse_y);
		angley := angley - camera_rotate_sens * (x - mouse_x);

		if anglex >= 90.0 then
			anglex := 90.0;
		if anglex <= -90.0 then
			anglex := -90.0;

		direction.x := -Sin(angley*(PI/180)) * Cos(anglex*(PI/180));
		direction.y := Sin(anglex*(PI/180));
		direction.z := Cos(angley*(PI/180)) * Cos(anglex*(PI/180));

		IupUpdate(ih);
	end else
	if alt and btn1 then // перемещать в плоскости XZ
	begin
		sinh := Sin(angley*(PI/180));
		cosh := Cos(angley*(PI/180));
		
		position.x := position.x + (camera_move_sens * (y - mouse_y)) * sinh;
		position.z := position.z + (camera_move_sens * (y - mouse_y)) * -cosh;
		
		position.x := position.x + (camera_move_sens * (x - mouse_x)) * cosh;
		position.z := position.z + (camera_move_sens * (x - mouse_x)) * sinh;
		
		UpdateCameraPos;		
	end else
	if alt and btn3 then // перемещать по оси Y
	begin
		position.y := position.y - (camera_move_sens * (y - mouse_y));
		
		UpdateCameraPos;
	end else
	if btn2 then
	begin
		distance := distance + ((y - mouse_y) / 8);
		IupUpdate(ih);
	end;
	
	if selection_rect then
		IupUpdate(ih);

	if (m_t <> nil) then
	begin
		m_t.Update(x, y);
		
		if m_t.IsActive then case edit_mode of
			
			emEntity: begin
				selected := Scene.GetSelected;	
				
				//c := GetCenter(selected);
				c.x := m_t.Matrix[4,1];
				c.y := m_t.Matrix[4,2];
				c.z := m_t.Matrix[4,3];
					
				for I := 0 to Length(selected)-1 do
				begin
					e := selected[I];
					mat := e.Matrix;
					ApplyManipulator(mat, c);
					e.Matrix := mat;
				end;
			end;
			
			emEnvZone: begin
				z := Scene.GetSelectedEZ;
				c := GetCenter(z);
				
				For I := 0 to Length(z)-1 do
				begin
					z[I].GetCenter(cc);
					Translate(mat, cc);
					
					ApplyManipulator(mat, c);
					
					z[I].Transform(mat);
				end;	
			end;
			
			emCovers: begin
				covers := Scene.GetSelectedCovers;

				//c := GetCenter(selected);
				c.x := m_t.Matrix[4,1];
				c.y := m_t.Matrix[4,2];
				c.z := m_t.Matrix[4,3];
					
				for I := 0 to Length(covers)-1 do
				begin
					cover := covers[I];
					mat := cover.Matrix;
					ApplyManipulator(mat, c);
					cover.Matrix := mat;
				end;
				
			end;
		end;
		
		IupUpdate(ih);
	end;

	mouse_x := x;
	mouse_y := y;

	Result := IUP_DEFAULT;
end;

function RaycastPoint(const p, dir : TVec3; dist : Single; out point, normal : TVec3; out shape : Pointer) : Boolean;
var
	group : Longword;
begin
	group := PH_GROUP_DEFAULT_MASK;					
	shape := PHRaycastClosestShape(Scene.ph_scene, @p, @dir, dist, @point, @normal, nil, group);

	Result := shape <> nil;
end;

function RaycastEntity(const p, dir : TVec3; dist : Single; out shape : Pointer) : TEntity;
var
	group : Longword;
	actor : TPHActor;
	sel : TObject;
begin
	if Scene.showShapes then
		group := PH_GROUP_DEFAULT_MASK or PH_GROUP_SHAPE_MASK
	else
		group := PH_GROUP_DEFAULT_MASK;
					
	shape := PHRaycastClosestShape(Scene.ph_scene, @p, @dir, dist, nil, nil, nil, group);
	if shape <> nil then
	begin
		actor := PHGetActor(shape);
		sel := TObject(PHGetUserdata(actor));

		if sel is TEntity then
			Result := TEntity(sel)
		else
			Result := nil;
	end;
end;

function RaycastEnvZone(const p, dir : TVec3; dist : Single; out shape : Pointer) : TEnvZone;
var
	group : Longword;
	actor : TPHActor;
	sel : TObject;
begin
	group := PH_GROUP_DEFAULT_MASK or PH_GROUP_ENV_ZONE_MASK;				
	shape := PHRaycastClosestShape(Scene.ph_scene, @p, @dir, dist, nil, nil, nil, group);
	if shape <> nil then
	begin
		actor := PHGetActor(shape);
		sel := TObject(PHGetUserdata(actor));

		if sel is TEnvZone then
			Result := TEnvZone(sel)
		else
			Result := nil;
	end;
end;

function RaycastUCover(const p, dir : TVec3; dist : Single; out shape : Pointer) : TCover;
var
	group : Longword;
	actor : TPHActor;
	sel : TObject;
begin
	group := PH_GROUP_DEFAULT_MASK or PH_GROUP_UCOVER_MASK;				
	shape := PHRaycastClosestShape(Scene.ph_scene, @p, @dir, dist, nil, nil, nil, group);
	if shape <> nil then
	begin
		actor := PHGetActor(shape);
		sel := TObject(PHGetUserdata(actor));

		if sel is TCover then
			Result := TCover(sel)
		else
			Result := nil;
	end;
end;

function RaycastUCoverLink(const p, dir : TVec3; dist : Single; out shape : Pointer) : TCoverLink;
var
	group : Longword;
	actor : TPHActor;
	sel : TObject;
begin
	if Scene.cover_manager.show_pe_links then
		group := PH_GROUP_DEFAULT_MASK or PH_GROUP_UCOVER_LINK_PE_MASK
	else
		group := PH_GROUP_DEFAULT_MASK or PH_GROUP_UCOVER_LINK_MASK;
	
	shape := PHRaycastClosestShape(Scene.ph_scene, @p, @dir, dist, nil, nil, nil, group);
	if shape <> nil then
	begin
		actor := PHGetActor(shape);
		sel := TObject(PHGetUserdata(actor));

		if sel is TCoverLink then
			Result := TCoverLink(sel)
		else
			Result := nil;
	end;
end;

function RaycastHumanCombatCover(const p, dir : TVec3; dist : Single; out shape : Pointer) : THumanCombatCover;
var
	group : Longword;
	actor : TPHActor;
	sel : TObject;
begin
	group := PH_GROUP_DEFAULT_MASK or PH_GROUP_HUMAN_COMBAT_COVER_MASK;				
	shape := PHRaycastClosestShape(Scene.ph_scene, @p, @dir, dist, nil, nil, nil, group);
	if shape <> nil then
	begin
		actor := PHGetActor(shape);
		sel := TObject(PHGetUserdata(actor));

		if sel is THumanCombatCover then
			Result := THumanCombatCover(sel)
		else
			Result := nil;
	end;
end;

procedure RaySelect(status : PAnsiChar; const p, dir : TVec3; dist : Single);
var
	O : ISelectable;
	shape : TPHShape;
begin
	case edit_mode of
		emEntity:  O := RaycastEntity(p, dir, dist, shape);
		emEnvZone: O := RaycastEnvZone(p, dir, dist, shape);
		emCovers: begin
			O := RaycastHumanCombatCover(p, dir, dist, shape);
			if O <> nil then
			begin
				WriteLn('flags = ', IntToHex(THumanCombatCover(O).flags, 8));
				WriteLn('cover_id = ', IntToHex(THumanCombatCover(O).cover_id, 4));
				WriteLn('cell = ', Longint(PHGetShapeUserdata(shape)));
			end;
		
			O := RaycastUCoverLink(p, dir, dist, shape);
			if O <> nil then
				uTabCovers.sel_link := TCoverLink(O)
			else
				O := RaycastUCover(p, dir, dist, shape);
		end; 
	end;
	
	if O <> nil then
	begin						
		if iup_isshift(status) then
		begin	
			O.Selected := not O.Selected;	
			UpdateSelection;
		end else
		begin
			DeselectAll;
			O.Selected := True;
			UpdateSelection;
		end;		
	end else // sel is not TEntity
	begin
		if not iup_isshift(status) then
		begin
			DeselectAll;
			UpdateSelection;
		end;
	end;
end;

procedure FrustumSelect(status : PAnsiChar; x1, y1, x2, y2 : Longint { rect } );
var
	fovy : Single;
	aspect : Single;
	
	cx, cy : Longint;
	
	p1, p2 : array[1..3] of GLdouble;
	v1, v2 : TVec3;
	
	matrix1, matrix2 : TMatrix;
	frustum : array[1..6] of TPlane;
	
	I : Longint;
	list : TList;
	
	tmp : Longint;
	bb : TAABB;
	
	E : TEntity;
begin
	if (x1 = x2) or (y1 = y2) then
		Exit;
		
	if x1 > x2 then
	begin
		tmp := x2;
		x2 := x1;
		x1 := tmp;
	end;
	
	if y1 > y2 then
	begin
		tmp := y2;
		y2 := y1;
		y1 := tmp;
	end;
		
	WriteLn('y1 = ', y1, ' y2 = ', y2, ' vp[0] = ', viewport[0], ' vp[2] = ', viewport[2]);
	fovy := cam_fov * ((y2-y1) / (viewport[3]-viewport[1]));
	aspect := (x2-x1) / (y2-y1);
	
	WriteLn('fovy = ', fovy, ' aspect = ', aspect);
	
	cx := x1 + (x2-x1) div 2;
	cy := y1 + (y2-y1) div 2;
	
	// in OpenGL origin of window coordinates is a left-bottom corner. That's why viewport[3]-y
	gluUnProject(cx, viewport[3]-cy, 0.9, @modelview_d, @proj_d, @viewport, @p1[1], @p1[2], @p1[3]);
	gluUnProject(cx, viewport[3]-cy, 0.1, @modelview_d, @proj_d, @viewport, @p2[1], @p2[2], @p2[3]);
	
	v1.x := p1[1];
	v1.y := p1[2];
	v1.z := p1[3];

	v2.x := p2[1];
	v2.y := p2[2];
	v2.z := p2[3];
	
	PerspectiveLH(matrix1, fovy*(PI/180), aspect, 0.1, 500);
	LookAtLH(matrix2, v2.x, v2.y, v2.z, v1.x, v1.y, v1.z, 0, 1, 0);
	
	Mul44(matrix1, matrix2);
	
	FrustumFromMatrix(frustum, matrix1);
	
	if not iup_isshift(status) then DeselectAll;
	list := TList.Create;
	
	case edit_mode of
		emEntity: begin
			for I := 0 to Scene.entities.Count-1 do
			begin
				E := TEntity(Scene.entities[I]);
				if E.Visible and AABBVisible(frustum, E.bbox) then
					list.Add(E);
			end;
			
			for I := 0 to list.Count-1 do
				TEntity(list[I]).selected := True;
		end;
		
		emEnvZone: begin
			for I := 0 to Scene.env_zones.Count-1 do
			begin
				TEnvZone(Scene.env_zones[I]).GetBBox(bb);
				if AABBVisible(frustum, bb) then
					list.Add(Scene.env_zones[I]);
			end;
			
			for I := 0 to list.Count-1 do
				TEnvZone(list[I]).selected := True;
		end;
	end;
	
	UpdateSelection;
	list.Free;
end;

function gl_button_cb(ih : Ihandle; button, pressed : Longint; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
const
	RAYCAST_DIST = 500.0;
var
	p1, p2 : array[1..3] of GLdouble;
	p, dir : TVec3;
	
	hit_pos, hit_nrm : TVec3;

	shape : TPHShape;
	m_used : Boolean;
	
	z : TEnvZone;
	e : TEntity;
	c : TCover;
	e_arr : TEntityArray;
	c_arr : TCoverArray;
	
	I : Longint;

begin	
	if not Assigned(Scene.ph_scene) then
	begin
		Result := IUP_DEFAULT;
		Exit;
	end;

	// in OpenGL origin of window coordinates is a left-bottom corner. That's why viewport[3]-y
	gluUnProject(x, viewport[3]-y, 0.1, @modelview_d, @proj_d, @viewport, @p1[1], @p1[2], @p1[3]);
	gluUnProject(x, viewport[3]-y, 0.9, @modelview_d, @proj_d, @viewport, @p2[1], @p2[2], @p2[3]);

	p.x := p1[1];
	p.y := p1[2];
	p.z := p1[3];

	dir.x := p2[1]-p1[1];
	dir.y := p2[2]-p1[2];
	dir.z := p2[3]-p1[3];
	Normalize(dir);

	m_used := False;
	
	if iup_isalt(status) then
	begin
		// этот случай заревервирован для мышиного управления камерой, ничего не делать
	end else
	if button = Ord('3') then
	begin
		if pressed = 1 then
		begin
			rclick_x := x;
			rclick_y := y;
		end else
		if pressed = 0 then
		begin
			if (rclick_x = x) and (rclick_y = y) then
				IupPopup(context_menu, IUP_MOUSEPOS, IUP_MOUSEPOS);
		end;
	end else
	if button = Ord('1') then
	begin
		if pressed = 1 then
		begin
			if (m_t <> nil) and m_t.Activate(x, y) then
			begin
				if edit_mode = emEntity then
					UndoSave('Transformation', Scene.GetSelected)
				else
					UndoSaveEnv('Transformation');
					
				m_used := True;
			end;
			
			// если не трогали манипулятор то начать выделение рамочкой
			if not m_used then
			begin
				lclick_x := x;
				lclick_y := y;
				
				selection_rect := True;
			end;
		end else
		if pressed = 0 then
		begin
			if (m_t <> nil) and m_t.IsActive then
			begin
				m_t.Deactivate;
				m_used := True;
			end;
			
			if not m_used then
			begin	
				if selection_rect and (x <> lclick_x) and (y <> lclick_y) then
				begin
					FrustumSelect(status, lclick_x, lclick_y, x, y);
				end else
				
				if iup_iscontrol(status) and ((GetASyncKeyState(VK_Z) and $8000) <> 0) then
				begin // attach shape
					case edit_mode of
						emEntity: begin
							e := RaycastEntity(p, dir, RAYCAST_DIST, shape);
							if (e <> nil) and not e.Selected then
								AttachShapesTo(e);
						end;
						
						emEnvZone: begin
							z := RaycastEnvZone(p, dir, RAYCAST_DIST, shape);
							if (z <> nil) and not z.Selected then
								AttachEnvZoneTo(z);
						end;
						
						emCovers: begin
							c := RaycastUCover(p, dir, RAYCAST_DIST, shape);
							if (c <> nil) and not c.Selected then
								MakeCoverLink(c);
						end;
					end;
				end else
				
				if iup_iscontrol(status) and ((GetASyncKeyState(VK_X) and $8000) <> 0) then
				begin // remove shape
					case edit_mode of
						emEntity: begin
							e := RaycastEntity(p, dir, RAYCAST_DIST, shape);
							if (e <> nil) and e.selected and (PHGetGroup(shape) = PH_GROUP_SHAPE) then
							begin
								UndoSave('Remove shape', Scene.GetSelected);
								RemoveShape(e, TSection(PHGetShapeUserdata(shape)));
							end;
						end;
						
						emEnvZone: begin
							z := RaycastEnvZone(p, dir, RAYCAST_DIST, shape);
							if (z <> nil) and z.selected then
							begin
								UndoSaveEnv('Remove env zone quad');
								RemoveEnvZoneQuad(z, Longint(PHGetShapeUserdata(shape)));
							end;
						end;
					end;
				end else
				
				if iup_iscontrol(status) and iup_isshift(status) then 
				begin // attach
					if edit_mode = emEntity then
					begin
						e := RaycastEntity(p, dir, RAYCAST_DIST, shape);
						if (e <> nil) and not e.selected then
							AttachSelectionTo(e);
					end;
				end else 
				
				if iup_iscontrol(status) and ((GetASyncKeyState(VK_F) and $8000) <> 0) then
				begin
					if (edit_mode = emEntity) then
					begin
						e_arr := Scene.GetSelected;
						if (Length(e_arr) = 1) and RaycastPoint(p, dir, RAYCAST_DIST, hit_pos, hit_nrm, shape) then
						begin
							UndoSave('Move entity to hit point', e_arr);
							uTabEntity.MoveEntityToHitPoint(e_arr[0], hit_pos, hit_nrm);
						end;
					end;
				end else 
				
				if iup_iscontrol(status) then 
				begin // create
					if RaycastPoint(p, dir, RAYCAST_DIST, hit_pos, hit_nrm, shape) then
					begin
					  case edit_mode of	
							emEntity: begin 
								UndoSave('Create entity'); 
								uTabEntity.CreateEntity(hit_pos, hit_nrm);
							end;
							emEnvZone: begin
								UndoSaveEnv('Create zone');
								uTabEnvZone.CreateEnvZone(hit_pos);
							end;
							emCovers: begin
								//UndoSave('Create cover');
								uTabCovers.CreateCover(hit_pos, hit_nrm);
							end;
						end;
					end;
				end else
					RaySelect(status, p, dir, RAYCAST_DIST);
						
				selection_rect := False;
			end;
			
			if m_used and (edit_mode = emCovers) then
			begin
				c_arr := Scene.GetSelectedCovers;
				for I := 0 to Length(c_arr) - 1 do
					Scene.cover_manager.UpdateCoverPosition(c_arr[I]);
				
				// update property values
				uTabCovers.UpdateTab;
			end;
		end;
	end;

	IupRedraw(ih, 0);

	Result := IUP_DEFAULT;
end;

function gl_keypress_cb(ih : Ihandle; ic : Longint; press : Longint) : Longint; cdecl;
var
	c : Char;
	sinx, cosx, siny, cosy : Single;
	m : TVec3;
begin
	Result := IUP_DEFAULT;

	// show hand cursor if we're gonna attach shape, or attach env-zone, or make cover link
	if iup_isCtrlXkey(ic) and (Chr(ic) in ['Z', 'z']) then
	begin
		if press <> 0 then
			IupSetAttribute(ih, 'CURSOR', 'HAND')
		else
			IupSetAttribute(ih, 'CURSOR', 'ARROW')
	end;
	
	if press <> 1 then
	begin
		Exit;
	end;

	if ic = $FFFF then // DELETE
	begin
		case edit_mode of
			emEntity:  UndoSave('Delete entities');
			emEnvZone: UndoSaveEnv('Delete env zone');
		end;
		
		DeleteSelection;
		
		IupRedraw(ih, 0);
		Exit;
	end;
	
	if iup_isCtrlXkey(ic) and (Chr(ic) in ['T', 't']) then // Undo Ctrl+T
	begin
		DoUndo;
		UpdateSelection;
		Redisplay;
	end;
	
	if iup_isCtrlXkey(ic) and (Chr(ic) in ['Y', 'y']) then // Redo Ctrl+Y
	begin
		DoRedo;
		UpdateSelection;
		Redisplay;
	end;

	sinx := Sin(anglex * (PI/180));
	cosx := Cos(anglex * (PI/180));
	siny := Sin(angley * (PI/180));
	cosy := Cos(angley * (PI/180));

	c := Char(ic);
	case c of
		'W', 'w' :
		begin
			m.x := -(siny * cosx);
			m.y := sinx;
			m.z := cosy * cosx;
		end;
		'S', 's' :
		begin
			m.x := siny * cosx;
			m.y := -sinx;
			m.z := -(cosy * cosx);
		end;
		'D', 'd' :
		begin
			m.x := cosy;
			m.y := 0;
			m.z := siny;
		end;
		'A', 'a' :
		begin
			m.x := -cosy;
			m.y := 0;
			m.z := -siny;
		end;
		else
		begin
			m.x := 0;
			m.y := 0;
			m.z := 0;
		end;
	end;

	if (GetASyncKeyState(VK_SHIFT) and $8000) <> 0 then
	begin
		m.x := m.x * camera_fly_speed_fast;
		m.y := m.y * camera_fly_speed_fast;
		m.z := m.z * camera_fly_speed_fast;
	end else
	begin
		m.x := m.x * camera_fly_speed;
		m.y := m.y * camera_fly_speed;
		m.z := m.z * camera_fly_speed;
	end;		

	position.x := position.x + m.x;
	position.y := position.y + m.y;
	position.z := position.z + m.z;
	
	UpdateCameraPos;

	IupRedraw(ih, 0);
end;

///////////////////////////////////////////////////////////////////////////////
// Toolbox callbacks                                                         //
///////////////////////////////////////////////////////////////////////////////
function fb_ao_cb(ih : Ihandle) : Longint; cdecl;
begin
	showAO := IupGetInt(ih, 'VALUE') = 1;

	ReloadGLPrograms;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function tg_weather_cb(ih : Ihandle) : Longint; cdecl;
begin
	useWeather := IupGetInt(ih, 'VALUE') = 1;

	ReloadGLPrograms;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function btn_tool_cb(ih : Ihandle) : Longint; cdecl;
var
	t : String;
	fr_tool, r_tool_move, r_tool_rotate, r_tool_scale : Ihandle;
	
	procedure Hide(ih : Ihandle);
	begin
		IupSetAttribute(ih, 'FLOATING', 'YES');
		IupSetAttribute(ih, 'VISIBLE', 'NO');
	end;
	
	procedure Show(ih : Ihandle);
	begin
		IupSetAttribute(ih, 'FLOATING', 'NO');
		IupSetAttribute(ih, 'VISIBLE', 'YES');
	end;
begin
	t := IupGetAttribute(ih, 'TITLE');

	fr_tool := IupGetDialogChild(ih,'FRAME_TOOL');
	r_tool_move := IupGetDialogChild(ih, 'R_TOOL_MOVE');
	r_tool_rotate := IupGetDialogChild(ih, 'R_TOOL_ROTATE');
	r_tool_scale := IupGetDialogChild(ih, 'R_TOOL_SCALE');

	Show(fr_tool);
	Hide(r_tool_move);
	Hide(r_tool_rotate);
	Hide(r_tool_scale);

	if t = 'Move' then
	begin
		m_mode := mmMove;
		Show(r_tool_move);
	end
	else if t = 'Rotate' then
	begin
		m_mode := mmRotate;
		Show(r_tool_rotate);
	end
	else if t = 'Scale' then
	begin
		m_mode := mmScale;
		Show(r_tool_scale);
	end
	else
	begin
		m_mode := mmNone;
		Hide(fr_tool);
	end;
	
	IupRefresh(fr_tool);
	
	UpdateManipulator;
	Redisplay;

	Result := IUP_DEFAULT;
end;

function tg_axis_cb(ih : Ihandle; state : Longint) : Longint; cdecl;
var
	t : String;
begin
	if state = 1 then
	begin
		t := IupGetAttribute(ih, 'TITLE');
		
		if m_mode = mmMove then
		begin
			if t = 'Object' then
				m_move_axis := maObject
			else if t = 'World' then
				m_move_axis := maWorld;
		end;
		
		if m_mode = mmRotate then
		begin
			if t = 'Object' then
				m_rotate_axis := maObject
			else if t = 'World' then
				m_rotate_axis := maWorld
			else if t = 'Group' then
				m_rotate_axis := maGroup;
		end;
		
		UpdateManipulator;
		Redisplay;
	end;

	Result := IUP_DEFAULT;
end;

function tg_uniform_scale_cb(ih : Ihandle; state : Longint) : Longint; cdecl;
begin
	m_uniform_scale := state = 1;
	UpdateManipulator;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function tabs_changepos_cb(ih : Ihandle; new_pos, old_pos : Longint) : Longint; cdecl;
begin
	WriteLn('change tab ', new_pos);
	
	case new_pos of
		0: edit_mode := emEntity;
		1: edit_mode := emEnvZone;
		2: begin edit_mode := emWeather; uTabWeather.FillList; end;
		3: edit_mode := emCovers;
	end;
	
	Redisplay;
	
	Result := IUP_DEFAULT;
end;

///////////////////////////////////////////////////////////////////////////////
// Menu callbacks                                                            //
///////////////////////////////////////////////////////////////////////////////
function menu_file_open_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	fn : String;
begin
	dlg := IupFileDlg;
	IupSetAttribute(dlg, 'DIALOGTYPE', 'DIR');

	if level_path <> '' then
		iup.SetStrAttribute(dlg, 'DIRECTORY', level_path)
	else
		iup.SetStrAttribute(dlg, 'DIRECTORY', ResourcesPath + '\maps');

	IupPopup(dlg, IUP_CENTER, IUP_CENTER);

	if IupGetInt(dlg, 'STATUS') = 0 then
	begin
		fn := IupGetAttribute(dlg, 'VALUE');
		LoadMap(fn);
	end;

	IupDestroy(dlg);
	Result := IUP_DEFAULT;
end;

function menu_file_reopen_cb(ih : Ihandle) : Longint; cdecl;
begin
	Scene.LevelReload;
	UndoClearHistory;
	UpdateSelection;
	
	Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_file_save_cb(ih : Ihandle) : Longint; cdecl;
begin
	if Scene.konf <> nil then
		SaveMap
	else
		IupMessage('Message', 'Nothing opened');
		
	Result := IUP_DEFAULT;
end;

function menu_file_save_as_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	fn : String;
	
	kind : Byte;
begin
	if Scene.konf <> nil then
	begin
		dlg := IupFileDlg;
		IupSetAttribute(dlg, 'DIALOGTYPE', 'SAVE');
		IupSetAttribute(dlg, 'FILE', PAnsiChar(level_path + '\level.bin'));
		IupSetAttribute(dlg, 'EXTFILTER', 'level.bin|*.bin|All files|*.*|');

		IupPopup(dlg, IUP_CENTER, IUP_CENTER);

		if IupGetInt(dlg, 'STATUS') <> -1 then
		begin
			case Scene.GetVersion of
				sceneVer2033: kind := konfDebugInfo or konfDiktionary;
				sceneVerLL:   kind := konfDiktionary;
				else          kind := konfDiktionary or konfMultichunk;
			end;
		
			fn := IupGetAttribute(dlg, 'VALUE');
			SaveLevelBin(fn, Scene.konf, kind);
		end;

		IupDestroy(dlg);
	end else
		IupMessage('Message', 'Nothing opened');

	Result := IUP_DEFAULT;
end;

function menu_file_save_addon_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	fn : String;
	
	kind : Byte;
begin
	if Scene.konf_add <> nil then
	begin
		dlg := IupFileDlg;
		IupSetAttribute(dlg, 'DIALOGTYPE', 'SAVE');
		IupSetAttribute(dlg, 'FILE', PAnsiChar(level_path + '\level.add.bin'));
		IupSetAttribute(dlg, 'EXTFILTER', 'level.add.bin|*.bin|All files|*.*|');

		IupPopup(dlg, IUP_CENTER, IUP_CENTER);

		if IupGetInt(dlg, 'STATUS') <> -1 then
		begin
			case Scene.GetVersion of
				sceneVer2033:		kind := 5;
				sceneVerLL:			kind := 4;
				else						kind := 36;
			end;
		
			fn := IupGetAttribute(dlg, 'VALUE');
			SaveLevelBin(fn, Scene.konf_add, kind);
		end;

		IupDestroy(dlg);
	end else
		IupMessage('Message', 'Nothing opened');

	Result := IUP_DEFAULT;
end;

function menu_file_save_env_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	fn : String;
begin
	if Scene.env_zones <> nil then
	begin
		dlg := IupFileDlg;
		IupSetAttribute(dlg, 'DIALOGTYPE', 'SAVE');
		IupSetAttribute(dlg, 'FILE', PAnsiChar(level_path + '\level.environment'));
		IupSetAttribute(dlg, 'EXTFILTER', 'level.environemnt|*.environment|All files|*.*|');

		IupPopup(dlg, IUP_CENTER, IUP_CENTER);

		if IupGetInt(dlg, 'STATUS') <> -1 then
		begin
			fn := IupGetAttribute(dlg, 'VALUE');
			Scene.SaveEnvironmentToFile(fn);
		end;

		IupDestroy(dlg);
	end else
		IupMessage('Message', 'Nothing opened');

	Result := IUP_DEFAULT;
end;

{$IFDEF HAZ_LEVELEXPORT}
function menu_file_export_cb(ih : Ihandle) : Longint; cdecl;
begin
	if Scene.entities <> nil then
	begin
		ExportLevel(ExtractFileName(level_path)); 
	end else
		IupMessage('Message', 'Nothing opened');
		
	Result := IUP_DEFAULT;
end;
{$ENDIF}

{$IFDEF HAZ_XRAYEXPORT}
function menu_file_export_xray_cb(ih : Ihandle) : Longint; cdecl;
begin
	if Scene.entities <> nil then
	begin
		ExportXrayLevel(ExtractFileName(level_path));
	end else
		IupMessage('Message', 'Nothing opened');
		
	Result := IUP_DEFAULT;
end;
{$ENDIF}

function menu_file_exit_cb(ih : Ihandle) : Longint; cdecl;
begin
	Result := IUP_CLOSE;
end;

function menu_edit_undo_cb(ih : Ihandle) : Longint; cdecl;
begin
  DoUndo;
  UpdateSelection;
  Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_edit_redo_cb(ih : Ihandle) : Longint; cdecl;
begin
  DoRedo;
  UpdateSelection;
  Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_edit_cut_cb(ih : Ihandle) : Longint; cdecl;
begin
	case edit_mode of
		emEntity: uTabEntity.CutSelection;
		//emEnvZone: uTabEnvZone.CopySelection;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_edit_copy_cb(ih : Ihandle) : Longint; cdecl;
begin
	case edit_mode of
		emEntity: uTabEntity.CopySelection;
		//emEnvZone: uTabEnvZone.CopySelection;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_edit_paste_cb(ih : Ihandle) : Longint; cdecl;
begin
	case edit_mode of
		emEntity: uTabEntity.PasteSelection;
		//emEnvZone: uTabEnvZone.PasteSelection;
	end;
		
	Result := IUP_DEFAULT;
end;

function menu_edit_hide_cb(ih : Ihandle) : Longint; cdecl;
var
	e : TEntityArray;
	I : Longint;
begin
	e := Scene.GetSelected;
	for I := 0 to Length(e)-1 do
		e[I].Visible := False;
		
	Redisplay;
		
	Result := IUP_DEFAULT;
end;

function menu_edit_show_cb(ih : Ihandle) : Longint; cdecl;
var
	e : TEntityArray;
	I : Longint;
begin
	e := Scene.GetSelected;
	for I := 0 to Length(e)-1 do
		e[I].Visible := True;
		
	Redisplay;
		
	Result := IUP_DEFAULT;
end;

function menu_edit_show_all_cb(ih : Ihandle) : Longint; cdecl;
var
	I : Longint;
begin
	for I := 0 to Scene.entities.Count-1 do
		TEntity(Scene.entities[I]).Visible := True;
		
	Redisplay;
		
	Result := IUP_DEFAULT;
end;

function menu_show_cb(ih : Ihandle) : Longint; cdecl;
var
	title : String;
	
	procedure DoIt(var b : Boolean);
	begin
		b := not b;
		IupSetInt(ih, 'VALUE', Integer(b));
	end;
begin
	title := IupGetAttribute(ih, 'TITLE');
	
	if title = 'Flags' then DoIt(Scene.showFlags);
	if title = 'Shapes' then  DoIt(Scene.showShapes);
	if title = 'Environment zones' then DoIt(Scene.showEnvZones);
	if title = 'Decals' then DoIt(Scene.showDecals);
	if title = 'EGeoms' then DoIt(Scene.showEGeoms);
	if title = 'Ways' then DoIt(Scene.showWays);
	if title = 'Navigation mesh' then DoIt(Scene.showNavMesh);
	
	if title = 'Animation' then
	begin
		DoIt(TEntity.showAnimation);
		if TEntity.showAnimation then
			IupSetAttribute(anim_timer, 'RUN', 'YES')
		else
			IupSetAttribute(anim_timer, 'RUN', 'NO')
	end;
	
	Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_show_object_list_cb(ih : Ihandle) : Longint; cdecl;
begin
	uObjectList.Show;
	Result := IUP_DEFAULT;
end;

function level_options_map_cb(ih : Ihandle) : Longint; cdecl;
var
	startup : TSection;
begin
	startup := Scene.konf.root.GetSect('startup');
	SetupProperties(ih, startup);
	
	Result := IUP_DEFAULT;
end;

function menu_level_options_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg, box, tree : Ihandle;
begin
	if Scene.konf <> nil then
	begin
		tree := IupTree;
		IupSetCallback(tree, 'MAP_CB', @level_options_map_cb);
		
		box := IupVBox(tree, nil);
		IupSetAttribute(box, 'MARGIN', '10x10');
		
		dlg := IupDialog(box);
		IupSetAttribute(dlg, 'TITLE', 'Level options');
		
		IupPopup(dlg, IUP_CURRENT, IUP_CURRENT);
		
		IupDestroy(dlg);
	end;

	Result := IUP_DEFAULT;
end;

procedure WeaponItemsLinkFromList(wil : TSection; list : TList);
var
	I : Longint;
	id : Word;
	n : String;
begin
	wil.Clear;
	wil.AddInt('count', list.Count, 'u32');
	for I := 0 to list.Count - 1 do
	begin
		n := IntToStr(I);
		
		if list[I] <> nil then
			id := TEntity(list[I]).ID
		else
			id := 65535;
		
		wil.AddSect('rec_' + StringOfChar('0',4-Length(n)) + n)
		   .AddInt('items', id, 'entity_link, uobject_link');
	end;
end;

procedure ListFromWeaponItemsLink(wil : TSection; list : TList);
var
	I : Longint;
	rec : TSection;
	id : Word;
	E : TEntity;
begin
	for I := 1 to wil.ParamCount - 1 do
	begin
		rec := wil.GetParam(I) as TSection;
		
		id := rec.GetInt('items', 'entity_link, uobject_link');
		E := Scene.EntityById(id);
		
		if E <> nil then
			list.Add(E)
		else
			WriteLn('Found invalid weapon item link: ', id, '. Removed.');
	end;
end;

function menu_wil_add_selection_cb(ih : Ihandle) : Longint; cdecl;
var
	I : Longint;
	wil : TSection;
	sel : TEntityArray;
	list : TList;
begin
	sel := Scene.GetSelected;
	if Length(sel) = 0 then
	begin
		IupMessage('Message', 'Nothing selected');
		Exit;
	end;
	
	if Scene.konf <> nil then
	begin
		wil := Scene.konf.root.GetSect('startup').GetSect('weapon_items_link', False);
		if wil <> nil then
		begin
			UndoSave('Add weapon items links');
			
			list := TList.Create;
			
			ListFromWeaponItemsLink(wil, list);
			
			for I := 0 to Length(sel) - 1 do
			begin
				if list.IndexOf(sel[I]) < 0 then
					list.Add(sel[I]);
			end;
			
			WeaponItemsLinkFromList(wil, list);
			
			list.Free;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_wil_remove_selection_cb(ih : Ihandle) : Longint; cdecl;
var
	I, idx : Longint;
	wil : TSection;
	sel : TEntityArray;
	list : TList;
begin
	sel := Scene.GetSelected;
	if Length(sel) = 0 then
	begin
		IupMessage('Message', 'Nothing selected');
		Exit;
	end;
	
	if Scene.konf <> nil then
	begin
		wil := Scene.konf.root.GetSect('startup').GetSect('weapon_items_link', False);
		if wil <> nil then
		begin
			UndoSave('Remove weapon items links');
			
			list := TList.Create;
			
			ListFromWeaponItemsLink(wil, list);
			
			for I := 0 to Length(sel) - 1 do
			begin
				idx := list.IndexOf(sel[I]);
				if idx >= 0 then
					list.Delete(idx);
			end;
			
			WeaponItemsLinkFromList(wil, list);
			
			list.Free;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_wil_select_all_cb(ih : Ihandle) : Longint; cdecl;
var
	I : Longint;
	wil, rec : TSection;
	E : TEntity;
begin
	if (edit_mode = emEntity) and (Scene.konf <> nil) then
	begin
		wil := Scene.konf.root.GetSect('startup').GetSect('weapon_items_link', False);
		if wil <> nil then
		begin
			DeselectAll;
			
			for I := 1 to wil.ParamCount - 1 do
			begin
				rec := wil.GetParam(I) as TSection;
				E := Scene.EntityById(rec.GetInt('items', 'entity_link, uobject_link'));
				if E<>nil then E.Selected := True;
			end;
			
			UpdateSelection;
			Redisplay;
		end;
	end else
		IupMessage('Message', 'Please switch to Entity tab');
	
	Result := IUP_DEFAULT;
end;

function menu_wil_erase_invalid_cb(ih : Ihandle) : Longint; cdecl;
var
	wil : TSection;
	list : TList;
begin
	if Scene.konf <> nil then
	begin
		wil := Scene.konf.root.GetSect('startup').GetSect('weapon_items_link', False);
		if wil <> nil then
		begin
			UndoSave('Erase invalid weapon items links');
			
			list := TList.Create;
			
			ListFromWeaponItemsLink(wil, list);
			WeaponItemsLinkFromList(wil, list);
			
			list.Free;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_wil_erase_all_cb(ih : Ihandle) : Longint; cdecl;
var
	wil : TSection;
begin
	if Scene.konf <> nil then
	begin
		wil := Scene.konf.root.GetSect('startup').GetSect('weapon_items_link', False);
		if wil <> nil then
		begin
			UndoSave('Erase all weapon items links');
			
			wil.Clear;
			wil.AddInt('count', 0, 'u32');
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_level_select_entity_cb(ih : Ihandle) : Longint; cdecl;
var
	entity : TEntity;
begin
	if Scene.entities <> nil then
	begin
		if ChooseEntity(entity) then
		begin
			DeselectAll;
			if entity <> nil then
				entity.selected := True;
			UpdateSelection;
			Redisplay;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_level_select_id_cb(ih : Ihandle) : Longint; cdecl;
var
	entity : TEntity;
begin
	if Scene.entities <> nil then
	begin
		if ChooseEntity(entity, True) then
		begin
			DeselectAll;
			if entity <> nil then
				entity.selected := True;
			UpdateSelection;
			Redisplay;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_level_move2leveladdbin_cb(ih : Ihandle) : Longint; cdecl;
var
	arr : TEntityArray;
begin
	if Scene.entities <> nil then
	begin
		UndoSave; // or maybe better clean undo history? Not sure if it'll work fine with this
		arr := Scene.GetSelected;
		Scene.MakeAddon(arr, True);
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_level_move2levelbin_cb(ih : Ihandle) : Longint; cdecl;
var
	arr : TEntityArray;
begin
	if Scene.entities <> nil then
	begin
		UndoSave; // or maybe better clean undo history? Not sure if it'll work fine with this
		arr := Scene.GetSelected;
		Scene.MakeAddon(arr, False);
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_level_selectleveladdbin_cb(ih : Ihandle) : Longint; cdecl;
begin
	Scene.SelectAddon;
	UpdateSelection;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_level_save_n_run_cb(ih : Ihandle) : Longint; cdecl;
var
	content_maps : String;
	l : Longint;
begin
	content_maps := 'content\maps\';
	l := Pos(content_maps, LowerCase(level_path));
	
	if l > 0 then
	begin
		SaveMap;
		RunGame(Scene.GetVersion, Copy(level_path, l + Length(content_maps)))
	end else
		ShowError('Level not in content\maps directory!');
		
	Result := IUP_DEFAULT;
end;

function menu_level_run_options_cb(ih : Ihandle) : Longint; cdecl;
begin
	RunOptions(Scene.GetVersion);
	Result := IUP_DEFAULT;
end;

function set_width(ih: Ihandle; state: Longint) : Longint; cdecl;
begin
	lineWidth := IupGetInt(
		IupGetDialogChild(ih, 'Linerange'),
		'VALUE'
	);

	ReloadGLPrograms;
	Redisplay;

	Result := IUP_DEFAULT;
end;

function menu_settings_custom_manipulator(ih : Ihandle) : Longint; cdecl;
var
	dlg_i : Ihandle;
	box: Ihandle;
	lineRange: Ihandle;
	btn: Ihandle;
begin

	lineRange := IupSetAttributes(
		IupVal('HORIZONTAL'),
		'NAME="Linerange", RASTERSIZE=164x32, MIN=1, MAX=24, MARGIN=8x8'
	);

	btn := IupSetAttributes(
		Button('Set width', @set_width),
		'NAME="setwidth", SIZE=78x14, FONTSIZE=10, MARGIN=8x8, PADDING=0x0'
	);

	box := 	IupVbox(
				IupFlatLabel('Line width'),
				lineRange,
				btn,
				nil
			);

	dlg_i := IupDialog(box);

	IupShow(IupSetAttributes(dlg_i, 'TITLE="Settings Editor", SIZE=128x64'));
	
	Result := IUP_DEFAULT;
end;

function menu_settings_customize_camera(ih : Ihandle) : Longint; cdecl;
begin
	IupGetParam('Customize camera', nil, nil, 
		'Move sens: %r'#10'Rotate sens: %r'#10'Fly speed: %r'#10'Fast fly speed: %r'#10,
		@camera_move_sens, @camera_rotate_sens, @camera_fly_speed, @camera_fly_speed_fast
	);
	
	Result := IUP_DEFAULT;
end;

function menu_render_wireframe_cb(ih : Ihandle) : Longint; cdecl;
begin
	useWireframe := not useWireframe;
	IupSetInt(ih, 'VALUE', Integer(useWireframe));
	Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_render_flat_shade_cb(ih : Ihandle) : Longint; cdecl;
var
	gl : Ihandle;
	dc_handle : HDC;
	gl_context : HGLRC;
	sm : GLenum;
begin
	gl := IupGetDialogChild(ih, 'GL_CANVAS');
	dc_handle := HDC(IupGetAttribute(gl, 'VISUAL'));
	gl_context := HGLRC(IupGetAttribute(gl, 'CONTEXT'));
	
	if wglMakeCurrent(dc_handle, gl_context) then
	begin
		glGetIntegerv(GL_SHADE_MODEL, @sm);
		if sm = GL_FLAT then
		begin
			glShadeModel(GL_SMOOTH);
			IupSetInt(ih, 'VALUE', 0);
		end else
		begin
			glShadeModel(GL_FLAT);
			IupSetInt(ih, 'VALUE', 1);
		end;
		Redisplay;
	end else
		WriteLn('wglMakeCurrent FAILED !');

	Result := IUP_DEFAULT;
end;

function menu_render_setfarplane_cb(ih : Ihandle) : Longint; cdecl;
var
	dist : Single;
begin
	dist := far_plane;
	if IupGetParam('Set view distance', nil, nil, 'View distance: %r'#10, @dist) = 1 then
	begin
		far_plane := dist;
		Redisplay;
	end;
	Result := IUP_DEFAULT;
end;

function menu_render_distance_culling_cb(ih : Ihandle) : Longint; cdecl;
begin
	uLEOptions.cull_distance := not uLEOptions.cull_distance;
	IupSetInt(ih, 'VALUE', Integer(uLEOptions.cull_distance));
	
	Result := IUP_DEFAULT;
end;

function menu_render_textures_cb(ih : Ihandle) : Longint; cdecl;
begin
	useTextures := not useTextures;
	IupSetInt(ih, 'VALUE', Integer(useTextures));
	
	ReloadGLPrograms;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_render_bump_cb(ih : Ihandle) : Longint; cdecl;
begin
	useBump := not useBump;
	IupSetInt(ih, 'VALUE', Integer(useBump));
	
	ReloadGLPrograms;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_render_detail_cb(ih : Ihandle) : Longint; cdecl;
begin
	useDetail := not useDetail;
	IupSetInt(ih, 'VALUE', Integer(useDetail));
	Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_render_setbkcolor(ih : Ihandle) : Longint; cdecl;
var
	gl : Ihandle;
	dc_handle : HDC;
	gl_context : HGLRC;
begin
	gl := IupGetDialogChild(ih, 'GL_CANVAS');
	dc_handle := HDC(IupGetAttribute(gl, 'VISUAL'));
	gl_context := HGLRC(IupGetAttribute(gl, 'CONTEXT'));
	
	if wglMakeCurrent(dc_handle, gl_context) then
	begin
		if SelectColor(bkg_color) then
			Redisplay;
	end else
		WriteLn('wglMakeCurrent FAILED !');

	Result := IUP_DEFAULT;
end;

function menu_render_setquality(ih : Ihandle) : Longint; cdecl;
var
	q : Integer;
begin
	q := textureQuality;
	if IupGetParam('Texture quality', nil, nil, 'Max resolution: %i'#10, @q) = 1 then
		textureQuality := q;

	Result := IUP_DEFAULT;
end;

function menu_focus_on_selection_cb(ih : Ihandle): Longint; cdecl;
var
	sel : TSelectableArray;
	I : Longint;
	
	tmp_bb : TAABB;
	sel_bb : TAABB;
	sel_bs : TSphere;
	
	dist : Single;
begin
	case edit_mode of
		emEntity:  sel := TSelectableArray(Scene.GetSelected);
		emEnvZone: sel := TSelectableArray(Scene.GetSelectedEZ);
		emCovers:  sel := TSelectableArray(Scene.GetSelectedCovers);
		else       Exit;
	end;
	
	if Length(sel) > 0 then
	begin
	
		sel[0].GetBBox(sel_bb);
		for I := 1 to Length(sel)-1 do
		begin
			sel[I].GetBBox(tmp_bb);
			AABBMerge(sel_bb, tmp_bb);
		end;
		CalcBSphere(sel_bs, sel_bb);
		
		dist := sel_bs.radius / tan(cam_fov / 2.0 * (PI/180));
		
		position.x := sel_bs.center.x + (-direction.x * dist);
		position.y := sel_bs.center.y + (-direction.y * dist);
		position.z := sel_bs.center.z + (-direction.z * dist);
		
		UpdateCameraPos;
		
		Redisplay;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_select_parent_cb(ih : Ihandle): Longint; cdecl;
var
	sel : TEntityArray;
	p : TEntity;
begin
	sel := Scene.GetSelected;
	if Length(sel) = 1 then
	begin
		sel[0].selected := False;
		p := Scene.EntityById(sel[0].ParentID);
		if p <> nil then p.selected := True;
		
		UpdateSelection;
		Redisplay;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_select_childs_cb(ih : Ihandle): Longint; cdecl;
var
	sel : TEntityArray;
begin
	sel := Scene.GetSelected;
	if Length(sel) = 1 then
	begin
		RecursiveSelect(sel[0].ID);
		UpdateSelection;
		Redisplay;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_reset_scale_cb(ih : Ihandle): Longint; cdecl;
var
	I : Longint;
	sel : TEntityArray;
	scale : TVec3;
	mat : TMatrix;
begin
	sel := Scene.GetSelected;
	
	if Length(sel) > 0 then
	begin
		UndoSave('Reset scale', sel);
		
		for I := 0 to Length(sel)-1 do
		begin
			mat := sel[I].Matrix;
			NormalizeMatrixAndGetScale(mat, scale);
			sel[I].Matrix := mat;
		end;
		
		UpdateManipulator;
		Redisplay;
	end;
	
	Result := IUP_DEFAULT;
end;

///////////////////////////////////////////////////////////////////////////////
// Dialog & menu creation                                                    //
///////////////////////////////////////////////////////////////////////////////
procedure CreateDialog;
var
	gl : Ihandle;
	
	fb_none : Ihandle;
	fb_move : Ihandle;
	fb_rotate : Ihandle;
	fb_scale : Ihandle;
	r_tools : Ihandle;
	
	fb_weather : Ihandle;
	fb_ao : Ihandle;
	fb_run : Ihandle;
	top_bar : Ihandle;
	
	label_sel : Ihandle;
	mousexy : Ihandle;
	campos : Ihandle;
	status_bar : Ihandle;

	tg_object, tg_world, tg_group, tg_uniform : Ihandle;
	fr_tool : Ihandle;
	r_tool_move, r_tool_rotate, r_tool_scale : Ihandle;

	tabs : Ihandle;
	
	toolbox : Ihandle;
	
	menu : Ihandle;
	menu_save_part : Ihandle;
	
	sm_file, sm_edit, sm_show, sm_level, sm_wil, sm_render, sm_settings : Ihandle;

	dlg : Ihandle;
begin
	// Load icons
	LoadImages;

	// OpenGL Window
	gl := IupCanvas(nil);
	IupSetAttribute(gl, 'RASTERSIZE', '800x600');
	IupSetCallback(gl, 'ACTION', @gl_redraw_cb);
	IupSetCallback(gl, 'MAP_CB', @gl_map_cb);
	IupSetCallback(gl, 'UNMAP_CB', @gl_unmap_cb);
	IupSetCallback(gl, 'RESIZE_CB', @gl_resize_cb);
	IupSetCallback(gl, 'MOTION_CB', @gl_motion_cb);
	IupSetCallback(gl, 'BUTTON_CB', @gl_button_cb);
	IupSetCallback(gl, 'KEYPRESS_CB', @gl_keypress_cb);
	IupSetAttribute(gl, 'NAME', 'GL_CANVAS');
	
	// Topbar
	fb_none := IupFlatButton('None');
	IupSetAttribute(fb_none, 'IMAGE', 'ICON_CURSOR');
	IupSetCallback(fb_none, 'FLAT_ACTION', @btn_tool_cb);
	IupSetAttribute(fb_none, 'TOGGLE', 'YES');
	
	fb_move := IupFlatButton('Move');
	IupSetAttribute(fb_move, 'IMAGE', 'ICON_MOVE');
	IupSetCallback(fb_move, 'FLAT_ACTION', @btn_tool_cb);
	IupSetAttribute(fb_move, 'TOGGLE', 'YES');
	
	fb_rotate := IupFlatButton('Rotate');
	IupSetAttribute(fb_rotate, 'IMAGE', 'ICON_ROTATE');
	IupSetCallback(fb_rotate, 'FLAT_ACTION', @btn_tool_cb);
	IupSetAttribute(fb_rotate, 'TOGGLE', 'YES');
	
	fb_scale := IupFlatButton('Scale');
	IupSetAttribute(fb_scale, 'IMAGE', 'ICON_SCALE');
	IupSetCallback(fb_scale, 'FLAT_ACTION', @btn_tool_cb);
	IupSetAttribute(fb_scale, 'TOGGLE', 'YES');
	
	fb_weather := IupFlatButton('Weather');
	IupSetAttribute(fb_weather, 'IMAGE', 'ICON_WEATHER');
	IupSetAttribute(fb_weather, 'TOGGLE', 'YES');
	IupSetInt(fb_weather, 'VALUE', Integer(useWeather));
	IupSetCallback(fb_weather, 'FLAT_ACTION', @tg_weather_cb);
	
	fb_ao := IupFlatButton('Show AO');
	IupSetAttribute(fb_ao, 'IMAGE', 'ICON_AO');
	IupSetAttribute(fb_ao, 'TOGGLE', 'YES');
	IupSetInt(fb_ao, 'VALUE', Integer(showAO));
	IupSetCallback(fb_ao, 'FLAT_ACTION', @fb_ao_cb);
	
	fb_run := IupFlatButton('Save && Run');
	IupSetAttribute(fb_run, 'IMAGE', 'ICON_RUN');
	IupSetCallback(fb_run, 'FLAT_ACTION', @menu_level_save_n_run_cb);
	
	r_tools := IupRadio(IupHBox(fb_none, fb_move, fb_rotate, fb_scale, nil));
	IupSetAttributes(r_tools, 'MARGIN=0x0, PADDING=0x0');
	
	top_bar := IupHBox(
		r_tools, 
		IupSetAttributes(IupSpace, 'SIZE=15x5'), 
		fb_weather, fb_ao, 
		IupSetAttributes(IupSpace, 'SIZE=15x5'),
		fb_run,
		nil
	);
	
	IupSetAttribute(top_bar, 'MARGIN', '1x1');
	IupSetAttribute(top_bar, 'PADDING', '1x1');
	
	// Statusbar
	mousexy := IupLabel('0, 0');
	IupSetAttribute(mousexy, 'NAME', 'MOUSEXY');
	IupSetAttribute(mousexy, 'RASTERSIZE', '100x');
	
	label_sel := IupLabel('Sel: 0');
	IupSetAttribute(label_sel, 'NAME', 'LABEL_SEL');
	IupSetAttribute(label_sel, 'ALIGNMENT', 'RIGHT');
	IupSetAttribute(label_sel, 'RASTERSIZE', '80x');
	
	campos := IupLabel('Cam pos: 0.0 0.0 0.0');
	IupSetAttribute(campos, 'NAME', 'LABEL_CAMPOS');
	IupSetAttribute(campos, 'RASTERSIZE', '250x');
	
	status_bar := IupHBox(mousexy, label_sel, campos, nil);

	// Left Toolbox
	tg_object := iup.Toggle('Object', @tg_axis_cb);
	tg_world := iup.Toggle('World', @tg_axis_cb);
	
	r_tool_move := IupRadio(IupHBox(tg_object, tg_world, IupFill, nil));
	IupSetAttribute(r_tool_move, 'NAME', 'R_TOOL_MOVE');
	IupSetAttribute(r_tool_move, 'VISIBLE', 'NO');
	IupSetAttribute(r_tool_move, 'FLOATING', 'YES');
	
	case m_move_axis of
		maObject: IupSetAttributeHandle(r_tool_move, 'VALUE', tg_object);
		maWorld: IupSetAttributeHandle(r_tool_move, 'VALUE', tg_world);
	end;
	
	tg_object := iup.Toggle('Object', @tg_axis_cb);
	tg_world := iup.Toggle('World', @tg_axis_cb);
	tg_group := iup.Toggle('Group', @tg_axis_cb);
	
	r_tool_rotate := IupRadio(IupHBox(tg_object, tg_world, tg_group, IupFill, nil));
	IupSetAttribute(r_tool_rotate, 'NAME', 'R_TOOL_ROTATE');
	IupSetAttribute(r_tool_rotate, 'VISIBLE', 'NO');
	IupSetAttribute(r_tool_rotate, 'FLOATING', 'YES');
	
	case m_rotate_axis of
		maObject: IupSetAttributeHandle(r_tool_rotate, 'VALUE', tg_object);
		maWorld: IupSetAttributeHandle(r_tool_rotate, 'VALUE', tg_world);
		maGroup: IupSetAttributeHandle(r_tool_rotate, 'VALUE', tg_group);
	end;
	
	tg_uniform := iup.Toggle('Uniform', @tg_uniform_scale_cb, m_uniform_scale);
	
	r_tool_scale := IupHBox(tg_uniform, IupFill, nil);
	IupSetAttribute(r_tool_scale, 'NAME', 'R_TOOL_SCALE');
	IupSetAttribute(r_tool_scale, 'VISIBLE', 'NO');
	IupSetAttribute(r_tool_scale, 'FLOATING', 'YES');
	
	fr_tool := IupFrame(
		IupSetAttributes(IupVBox(
			r_tool_move,
			r_tool_rotate,
			r_tool_scale,
			nil
		), 'MARGIN=5x3')
	);
	IupSetAttribute(fr_tool, 'NAME', 'FRAME_TOOL');
	IupSetAttribute(fr_tool, 'TITLE', 'Tool Options');
	IupSetAttribute(fr_tool, 'VISIBLE', 'NO');
	IupSetAttribute(fr_tool, 'FLOATING', 'YES');
	
	
	tabs := IupTabs(
		uTabEntity.CreateTab,
		uTabEnvZone.CreateTab,
		uTabWeather.CreateTab,
		uTabCovers.CreateTab,
		nil
	);
	
	IupSetAttribute(tabs, 'TABTITLE0', 'Entities');
	IupSetAttribute(tabs, 'TABTITLE1', 'Environment');
	IupSetAttribute(tabs, 'TABTITLE2', 'Weather');
	IupSetAttribute(tabs, 'TABTITLE3', 'Covers');
	
	IupSetCallback(tabs, 'TABCHANGEPOS_CB', @tabs_changepos_cb);

	toolbox := IupVBox(fr_tool, tabs, nil);
	IupSetAttribute(toolbox, 'MARGIN', '10x10');
	IupSetAttribute(toolbox, 'GAP', '5x5');
	
	// Main Menu
	menu_save_part := IupSubmenu('Save part',
		IupMenu(
			iup.MenuItem('Save level.bin as...', @menu_file_save_as_cb),
			iup.MenuItem('Save level.add.bin as...', @menu_file_save_addon_cb),
			iup.MenuItem('Save level.environemnt as...', @menu_file_save_env_cb),
			nil
		)
	);

	sm_file := IupSubmenu('File',
		IupMenu(
			iup.MenuItem('Open...', @menu_file_open_cb), 
			iup.MenuItem('Reopen', @menu_file_reopen_cb),
			iup.MenuItem('Save', @menu_file_save_cb), 
			menu_save_part,
			IupSeparator,
			{$IFDEF HAZ_LEVELEXPORT} iup.MenuItem('Export...', @menu_file_export_cb), {$ENDIF}
			{$IFDEF HAZ_XRAYEXPORT} iup.MenuItem('Export for X-Ray SDK...', @menu_file_export_xray_cb), {$ENDIF}
			{$IF Defined(HAZ_LEVELEXPORT) or Defined(HAZ_XRAYEXPORT)} IupSeparator, {$ENDIF}
			iup.MenuItem('Exit', @menu_file_exit_cb), 
			nil
		)
	);
		
	sm_edit := IupSubmenu('Edit',
		IupMenu(
		  iup.MenuItem('Undo'#9'Ctrl+T', @menu_edit_undo_cb),
		  iup.MenuItem('Redo'#9'Ctrl+Y', @menu_edit_redo_cb),
		  IupSeparator,
		  iup.MenuItem('Cut', @menu_edit_cut_cb),
			iup.MenuItem('Copy', @menu_edit_copy_cb), 
			iup.MenuItem('Paste', @menu_edit_paste_cb), 
			IupSeparator,
			iup.MenuItem('Hide', @menu_edit_hide_cb),
			iup.MenuItem('Show', @menu_edit_show_cb),
			iup.MenuItem('Show all', @menu_edit_show_all_cb),
			nil
		)
	);
		
	sm_show := IupSubmenu('Show',
		IupMenu(
			iup.MenuItem('Flags', @menu_show_cb, Scene.showFlags),
			iup.MenuItem('Animation', @menu_show_cb, TEntity.showAnimation),
			iup.MenuItem('Shapes', @menu_show_cb, Scene.showShapes),
			iup.MenuItem('Environment zones', @menu_show_cb, Scene.showEnvZones),
			iup.MenuItem('Decals', @menu_show_cb, Scene.showDecals),
			iup.MenuItem('EGeoms', @menu_show_cb, Scene.showEGeoms),
			iup.MenuItem('Ways', @menu_show_cb, Scene.showWays),
			iup.MenuItem('Object List', @menu_show_object_list_cb),
			IupSeparator,
			iup.MenuItem('Navigation mesh', @menu_show_cb, Scene.showNavmesh),
			nil
		)
	);
		
	// have to use this extra variable due to a compiler bug
	sm_wil := IupSubmenu('Weapon Items Link', 
		IupMenu(
			iup.MenuItem('Add selection', @menu_wil_add_selection_cb),
			iup.MenuItem('Remove selection', @menu_wil_remove_selection_cb),
			iup.MenuItem('Select all', @menu_wil_select_all_cb),
			iup.MenuItem('Erase invalid', @menu_wil_erase_invalid_cb),
			iup.MenuItem('Erase all', @menu_wil_erase_all_cb),
			nil
		)
	);
		
	sm_level := IupSubmenu('Level',
		IupMenu(
			iup.MenuItem('Options...', @menu_level_options_cb), 
			sm_wil,
			iup.MenuItem('Select entity...', menu_level_select_entity_cb), 
			iup.MenuItem('Select entity (with IDs)...', @menu_level_select_id_cb), 
			IupSeparator, 
			iup.MenuItem('Move selection to level.add.bin',  @menu_level_move2leveladdbin_cb), 
			iup.MenuItem('Move selection to level.bin', @menu_level_move2levelbin_cb),
			iup.MenuItem('Select all entities from level.add.bin', @menu_level_selectleveladdbin_cb), 
			IupSeparator,
			iup.MenuItem('Save && Run', @menu_level_save_n_run_cb),
			iup.MenuItem('Run Options...', @menu_level_run_options_cb),
			nil
		)
	);
		
	sm_render := IupSubmenu('Render',
		IupMenu(
			iup.MenuItem('Set view distance', @menu_render_setfarplane_cb), 
			iup.MenuItem('Distance culling', @menu_render_distance_culling_cb, uLEOptions.cull_distance),
			iup.MenuItem('Set background color', @menu_render_setbkcolor), 
			IupSeparator, 
			iup.MenuItem('Wireframe', @menu_render_wireframe_cb, useWireframe), 
			iup.MenuItem('Flat shading', @menu_render_flat_shade_cb, False), 
			IupSeparator, 
			iup.MenuItem('Textures', @menu_render_textures_cb, useTextures), 
			iup.MenuItem('Bump', @menu_render_bump_cb, useBump), 
			iup.MenuItem('Detail', @menu_render_detail_cb, useDetail), 
			iup.MenuItem('Set texture quality', @menu_render_setquality), 
			nil
		)
	);

	sm_settings := IupSubmenu('Settings',
		IupMenu(
			iup.MenuItem('Customize manipulator', @menu_settings_custom_manipulator),
			iup.MenuItem('Customize camera', @menu_settings_customize_camera),
			nil
		)
	);

	menu := IupMenu(
		sm_file,
		sm_edit,
		sm_show,
		sm_level,
		sm_render,
		sm_settings,
		nil
	);

	// Dialog
	dlg := IupDialog(
		IupSplit( toolbox, IupVBox(top_bar, gl, status_bar, nil) )
	);
	
	IupSetAttribute(dlg, 'TITLE', 'Level Editor');
	IupSetAttributeHandle(dlg, 'MENU', menu);

	IupSetHandle('MAINDIALOG', dlg);

	IupShowXY(dlg, IUP_CENTER, IUP_CENTER);
	
	uObjectList.Create;
end;

procedure CreateContextMenu;
begin	
	context_menu := IupMenu(
		iup.MenuItem('Cut', @menu_edit_cut_cb),
		iup.MenuItem('Copy', @menu_edit_copy_cb),
		iup.MenuItem('Paste', @menu_edit_paste_cb),
		IupSeparator,
		iup.MenuItem('Hide', @menu_edit_hide_cb),
		iup.MenuItem('Show', @menu_edit_show_cb),
		iup.MenuItem('Show all', @menu_edit_show_all_cb),
		IupSeparator,
		iup.MenuItem('Focus on selection', @menu_focus_on_selection_cb),
		IupSeparator,
		iup.MenuItem('Select parent', @menu_select_parent_cb),
		iup.MenuItem('Select all childs', @menu_select_childs_cb),
		iup.MenuItem('Reset scale', @menu_reset_scale_cb), // deprecated due to matrix editor?
		nil
	);
end;

var 
	P : Longint;
	initial_map_name : String = '';
	
begin
	utabentity.UpdateSelection := UpdateSelection; // Refactor!!
	utabcovers.UpdateSelection := UpdateSelection; // Refactor!!

	IupOpen(nil, nil);
	IupGLCanvasOpen;
	
	// initialize COM (for uChooseSound)
	if FAILED(CoInitializeEx(nil, COINIT_APARTMENTTHREADED)) then
		IupMessageError(nil, 'CoInitializeEx failed!');
	
	// initialize PhysX
	PHInitialize; 

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
	end else
	begin
		GetDir(0, ResourcesPath);
		ResourcesPath := ResourcesPath + '\content';
	end;
	
	if ParamCount-P > 0 then
	begin
		initial_map_name := ParamStr(P+1);
		Inc(P);
	end;
	// end parse command-line
	
	navmap_server.Run(False);

	InitializeEngine;
	
	Scene := TScene.Create;
	Scene.showFlags := True;
	Scene.showDecals := True;
	Scene.showEGeoms := True;
	Scene.showNavmesh := True;

	CreateDialog;
	CreateContextMenu;
	CreateAnimTimer;
	
	if initial_map_name <> '' then
		LoadMap(initial_map_name);
	
	IupMainLoop();
	IupDestroy(context_menu);

	SaveTemplates;
	UnloadTemplates;
	
	Scene.LevelUnload;
	Scene.Free;
	
	FinalizeEngine;
	
	navmap_server.Stop;
	
	// finalize PhysX
	PHFinalize;
	
	// finalize COM
	CoUninitialize;

	IupClose;
end.
