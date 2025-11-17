unit uTabCovers;

interface
uses vmath, Iup, uSceneCovers;

var UpdateSelection : procedure;

var
	sel_link : TCoverLink;

function CreateTab : Ihandle;
procedure UpdateTab;

procedure CreateCover(hit_pos, hit_nrm : TVec3);
procedure CreateLink(from, _to : TCover);

procedure CreateHumanCombatCover(for_cover : TCover);
procedure DeleteHumanCombatCover(of_cover : TCover);

procedure DeselectAll;

procedure DeleteSelection;

implementation
uses
	sysutils, classes,
	NavmapLdr, navmap_server, { for saving }
	_compressed_normal,
	Engine, Konfig, uScene, uLEOptions, uEditorUtils, properties, Windows, 
	
	propertyGrid,
	commctrl; { for subclassing }

var
	select_created : Boolean;
	attach_to_aimap : Boolean = True;
	auto_link : Boolean;

function btn_calc_pe_links_cb(ih : Ihandle) : Longint; cdecl;
begin
	Scene.cover_manager.CalculatePELinks(Scene.ph_scene);
	Result := IUP_DEFAULT;
end;

function btn_update_gnd_cells_cb(ih : Ihandle) : Longint; cdecl;
var
	I : Longint;
	old_cell, old_conn_reg : Longint;
	cover : TCover;
	
	cover_count : Longint;
	match_cell : Longint;
	match_conn_reg : Longint;
	
	cover_ok : Longint;
begin

	cover_count := 0;
	match_cell := 0;
	match_conn_reg := 0;
	cover_ok := 0;

	for I := 0 to 65534 do
	begin
		cover := Scene.cover_manager.covers[I];
		
		if (cover <> nil) and (cover.ConnectedToAimap) then
		begin
			Inc(cover_count);
			
			old_cell := cover._ground_cell.num;
			old_conn_reg := cover._conn_reg.num;
			
			Scene.cover_manager.UpdateCoverPosition(cover);
			
			if cover._ground_cell.num = old_cell then
				Inc(match_cell);
			if cover._conn_reg.num = old_conn_reg then
				Inc(match_conn_reg);
				
			if (cover._ground_cell.num >= 0) and (cover._conn_reg.num >= 0) then
				Inc(cover_ok);	
		end;
	end;
	
	IupMessage('Results', 
		PAnsiChar(
			'ground_cell: ' + IntToStr(match_cell) + ' match, ' + IntToStr(cover_count-match_cell) + ' dont match' + #10
			+ 'conn_reg: ' + IntToStr(match_conn_reg) + ' match, ' + IntToStr(cover_count-match_conn_reg) + ' dont match' + #10
			+ #10
			+ 'Update OK: ' + IntToStr(cover_ok) + #10
			+ 'Update FAIL: ' + IntToStr(cover_count-cover_ok)
		)
	);
	
	Result := IUP_DEFAULT;
end;

function btn_compare_human_combat_covers_cb(ih : Ihandle) : Longint; cdecl;
var
	I, J : Longint;	
	old_cells : array[0..11] of Longint;
	_match : Longint;
	_fail : Longint;
	
	bMatch : Boolean;
begin

	_match := 0;
	_fail := 0;
	
	for I := 0 to Length(Scene.cover_manager.human_combat_covers) - 1 do
	begin
		for J := 0 to 5 do
		begin
			old_cells[J*2+0] := Scene.cover_manager.human_combat_covers[I].arr[J].cell1;
			old_cells[J*2+1] := Scene.cover_manager.human_combat_covers[I].arr[J].cell2;
		end;
		
		Scene.cover_manager.UpdateHumanCombatCoverPosition( Scene.cover_manager.human_combat_covers[I] );
		
		bMatch := True;
		
		for J := 0 to 5 do
		begin
			if (old_cells[J*2+0] <> Scene.cover_manager.human_combat_covers[I].arr[J].cell1) or
			   (old_cells[J*2+1] <> Scene.cover_manager.human_combat_covers[I].arr[J].cell2) then bMatch := False;
			   
			if (0 > Scene.cover_manager.human_combat_covers[I].arr[J].cell1) or
			   (0 > Scene.cover_manager.human_combat_covers[I].arr[J].cell2) then Inc(_fail);
		end;
		
		if bMatch then Inc(_match);
	end;
	
	IupMessage('Results', 
	PAnsiChar(
		IntToStr(_match) + ' match, ' + 
		IntToStr(Length(Scene.cover_manager.human_combat_covers)-_match) + ' dont match' + #10
		+ IntToStr(_fail) + ' fail'));
	
	Result := IUP_DEFAULT;
end;

function btn_savenm_cb(ih : Ihandle) : Longint; cdecl;
var
	nm : TNavigationMap;
	dummy_conn_reg : Longint;
	num_PE_covers : Longint;
	I, J, K : Longint;
begin

	// create copy of navigation map
	nm := TNavigationMap.Create;
	
	nm.verts := Scene.navmap.verts;
	nm.tris := Scene.navmap.tris;
	
	// create end-points and connections
	num_PE_covers := 0;
	for I := 0 to 65534 do
		if (Scene.cover_manager.covers[I] <> nil) and (Scene.cover_manager.covers[I]._pe_index.num <> 65535) then
			Inc(num_PE_covers);
	
	SetLength(nm.end_points, num_PE_covers);

	J := 0;
	for I := 0 to 65534 do
		if (Scene.cover_manager.covers[I] <> nil) and (Scene.cover_manager.covers[I]._pe_index.num <> 65535) then
		begin
			nm.end_points[J].x := Trunc( Scene.cover_manager.covers[I].Pos.X * 1000 );
			nm.end_points[J].y := Trunc( Scene.cover_manager.covers[I].Pos.Z * 1000 );
			
			navmap_server.GetCellAndConnRegForCover(
				Scene.cover_manager.covers[I].Pos.X,
				Scene.cover_manager.covers[I].Pos.Y,
				Scene.cover_manager.covers[I].Pos.Z,
				0,
				nm.end_points[J].cell,
				dummy_conn_reg
			);
			
			if nm.end_points[J].cell < 0 then
			begin
				WriteLn('cell error');
			end;
			
			Inc(J);
		end;
		
	// create connections !
	SetLength(nm.connections, Length( Scene.cover_manager.links_pe ) );
	
	for I := 0 to Length( Scene.cover_manager.links_pe ) - 1 do
	begin
		K := 0;
		for J := 0 to Scene.cover_manager.links_pe[J].cover_from.num - 1 do
			if (Scene.cover_manager.covers[J] <> nil) and (Scene.cover_manager.covers[J]._pe_index.num <> 65535) then
				Inc(K);
				
		nm.connections[I].point_from := K;
		
		K := 0;
		for J := 0 to Scene.cover_manager.links_pe[I].cover_to.num - 1 do
			if (Scene.cover_manager.covers[J] <> nil) and (Scene.cover_manager.covers[J]._pe_index.num <> 65535) then
				Inc(K);
				
		nm.connections[I].point_to := K;
		
		nm.connections[I].penalty := 1;
	end;
	
	nm.SaveRAW('D:\Games\Metro 2033\nav_map.raw');
	nm.Free;

	Result := IUP_DEFAULT;
end;

function btn_sel_invalid_cb(ih : Ihandle) : Longint; cdecl;
var
	I : Longint;
	cover : TCover;
begin
	DeselectAll;
	
	for I := 0 to 65534 do
	begin
		cover := Scene.cover_manager.covers[I];
		
		if (cover <> nil) and (cover.ConnectedToAimap) then
		begin
			if ((cover._ground_cell <> nil) and (cover._ground_cell.num < 0)) or
			   ((cover._conn_reg <> nil) and (cover._conn_reg.num < 0)) then
			begin
				cover.Selected := True;
				Break; // select just one cover
			end;
		end;
	end;
	
	UpdateSelection;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function tg_links_cb(ih : Ihandle) : Longint; cdecl;
begin
	Scene.cover_manager.show_pe_links := (iup.GetAttribute(ih, 'NAME') = 'ULINKS_PE');
	Redisplay;
	Result := IUP_DEFAULT;
end;

function btn_attach_to_aimap_cb(ih : Ihandle) : Longint; cdecl;
begin
	Redisplay;
	Result := IUP_DEFAULT;
end;

function btn_detach_from_aimap_cb(ih : Ihandle) : Longint; cdecl;
begin
	Redisplay;
	Result := IUP_DEFAULT;
end;

function btn_create_hcc_cb(ih : Ihandle) : Longint; cdecl;
var
	sel_covers : TCoverArray;
begin
	sel_covers := Scene.GetSelectedCovers;
	if Length(sel_covers) = 1 then
	begin
		CreateHumanCombatCover(sel_covers[0]);
		UpdateTab;
		Redisplay;
	end;
	Result := IUP_DEFAULT;
end;

function btn_delete_hcc_cb(ih : Ihandle) : Longint; cdecl;
var
	sel_covers : TCoverArray;
begin
	sel_covers := Scene.GetSelectedCovers;
	if Length(sel_covers) = 1 then
	begin
		DeleteHumanCombatCover(sel_covers[0]);
		UpdateTab;
		Redisplay;
	end;
	Result := IUP_DEFAULT;
end;

// GRID
const
	CAT_ALLOWED_POSES = 'Allowed Poses';
	
const
	POSE_SIT_LEFT    = 'Sit Left';
	POSE_SIT_RIGHT   = 'Sit Right';
	POSE_STAND_LEFT  = 'Stand Left';
	POSE_STAND_RIGHT = 'Stand Right';
	POSE_UP_LEFT     = 'Up Left';
	POSE_UP_RIGHT    = 'Up Right';
	
const
	PARAM_VISIBLE    = 'Visible';
	PARAM_ROTATION   = 'Rotation';
	PARAM_FIREPOINT  = 'Fire Point';
	PARAM_ENTERPOINT = 'Enter Point 270';
	
function pose_id_from_name(const name : String) : Longint;
begin
	Result := -1;
	if name = POSE_SIT_LEFT    then Result := 0;
	if name = POSE_SIT_RIGHT   then Result := 1;
	if name = POSE_STAND_LEFT  then Result := 2;
	if name = POSE_STAND_RIGHT then Result := 3;	
	if name = POSE_UP_LEFT     then Result := 4;
	if name = POSE_UP_RIGHT    then Result := 5;	
end;

function Vec3FromStr(const str : String; out vec : TVec3) : Boolean;
var
	x : Extended;
	y : Extended;
	z : Extended;
begin
	if SScanf(str, '%f %f %f', [@x, @y, @z]) = 3 then
	begin
		vec.x := x;
		vec.y := y;
		vec.z := z;
		Vec3FromStr := True;
	end else
		Vec3FromStr := False;
end;
	
function Vec3ToStr(const vec : TVec3) : String;
begin
	Vec3ToStr := Format('%.4f %.4f %.4f', [vec.x, vec.y, vec.z]);
end;
	
procedure handle_param_change(nm : LPNMPROPGRID);
var
	pItem : LPPROPGRIDITEM;
	pose_id : Longint;
	
	vec_result : TVec3;
	
	c_arr : TCoverArray;
	hcc : THumanCombatCover;
	
	item : PROPGRIDITEM;
	str : String;
	cur_value : String;
	
	rotation : TMatrix;
	direction_vec : TVec3;
begin
	c_arr := Scene.GetSelectedCovers;
	if Length(c_arr) <> 1 then
		Exit;
		
	hcc := Scene.cover_manager.FindHumanCombatCover(c_arr[0].ID);
	if hcc = nil then
		Exit;

	pItem := PropGrid_GetItemData(nm.hdr.hwndFrom, nm.iIndex);
	
	// enable/disable pose
	if pItem.lpszCatalog = CAT_ALLOWED_POSES then
	begin
		pose_id := pose_id_from_name(pItem.lpszPropName);
		if pose_id <> -1 then
		begin
			if pItem.lpCurValue <> 0 then
				hcc.flags := hcc.flags or (1 shl pose_id)
			else
				hcc.flags := hcc.flags and not (1 shl pose_id);
				
			UpdateTab;
			Redisplay;
		end;
	end;
	
	// change of rotation
	if pItem.lpszPropName = PARAM_VISIBLE then
	begin
		pose_id := pose_id_from_name(pItem.lpszCatalog);
		if pose_id = -1 then
			Exit;
		
		hcc.e_visible[pose_id] := Boolean(pItem.lpCurValue);
		
		// Redraw scene
		Redisplay;
	end;
	
	// change of rotation
	if pItem.lpszPropName = PARAM_ROTATION then
	begin
		pose_id := pose_id_from_name(pItem.lpszCatalog);
		if pose_id = -1 then
			Exit;
			
		if Vec3FromStr(PAnsiChar(pItem.lpCurValue), vec_result) then
		begin
			// convert to radian
			vec_result.x := vec_result.x * (PI/180);
			vec_result.y := vec_result.y * (PI/180);
			vec_result.z := vec_result.z * (PI/180);
			
			// get direction vec
			SetHPB(rotation, vec_result.y, vec_result.x, vec_result.z);
			direction_vec.x := rotation[3,1];
			direction_vec.y := rotation[3,2];
			direction_vec.z := rotation[3,3];
			
			// save
			hcc.arr[pose_id].packed_dir := pvCompress(direction_vec);
			
			// Redraw scene
			Redisplay;
		end else
		begin
			ShowError('Error parsing value!');

			// restore
			item := pItem^;
			str := Vec3ToStr(hcc.arr[pose_id].vec1);
			item.lpCurValue := LPARAM(str);
			PropGrid_SetItemData(nm.hdr.hwndFrom, nm.iIndex, @item);			
		end;
	end;
	
	// change of firepoint
	if pItem.lpszPropName = PARAM_FIREPOINT then
	begin
		pose_id := pose_id_from_name(pItem.lpszCatalog);
		if pose_id = -1 then
			Exit;
			
		if PAnsiChar(pItem.lpCurValue) = '!' then
		begin
			cur_value := Vec3ToStr(c_arr[0].Pos);
			
			// 
			item := pItem^;
			item.lpCurValue := LPARAM(cur_value);
			PropGrid_SetItemData(nm.hdr.hwndFrom, nm.iIndex, @item);	
		end else
			cur_value := PAnsiChar(pItem.lpCurValue);
			
		if Vec3FromStr(cur_value, vec_result) then
		begin
			hcc.arr[pose_id].vec1 := vec_result;		
			Scene.cover_manager.UpdateHumanCombatCoverPosition(hcc);
			
			// update cell value (next to current)
			item := PropGrid_GetItemData(nm.hdr.hwndFrom, nm.iIndex + 1)^;
			str := IntToStr(hcc.arr[pose_id].cell1);
			item.lpCurValue := LPARAM(str);
			PropGrid_SetItemData(nm.hdr.hwndFrom, nm.iIndex + 1, @item);
			
			// Redraw scene
			Redisplay;
		end else
		begin
			ShowError('Error parsing value!');

			// restore
			item := pItem^;
			str := Vec3ToStr(hcc.arr[pose_id].vec1);
			item.lpCurValue := LPARAM(str);
			PropGrid_SetItemData(nm.hdr.hwndFrom, nm.iIndex, @item);			
		end;
	end;
	
	// change of enterpoint
	if pItem.lpszPropName = PARAM_ENTERPOINT then
	begin
		pose_id := pose_id_from_name(pItem.lpszCatalog);
		if pose_id = -1 then
			Exit;
			
		if PAnsiChar(pItem.lpCurValue) = '!' then
		begin
			cur_value := Vec3ToStr(c_arr[0].Pos);
			
			// 
			item := pItem^;
			item.lpCurValue := LPARAM(cur_value);
			PropGrid_SetItemData(nm.hdr.hwndFrom, nm.iIndex, @item);
		end else
			cur_value := PAnsiChar(pItem.lpCurValue);
			
		if Vec3FromStr(cur_value, vec_result) then
		begin
			hcc.arr[pose_id].vec2 := vec_result;		
			Scene.cover_manager.UpdateHumanCombatCoverPosition(hcc);
			
			// update cell value (next to current)
			item := PropGrid_GetItemData(nm.hdr.hwndFrom, nm.iIndex + 1)^;
			str := IntToStr(hcc.arr[pose_id].cell2);
			item.lpCurValue := LPARAM(str);
			PropGrid_SetItemData(nm.hdr.hwndFrom, nm.iIndex + 1, @item);
			
			// Redraw scene
			Redisplay;
		end else
		begin
			ShowError('Error parsing value!');

			// restore
			item := pItem^;
			str := Vec3ToStr(hcc.arr[pose_id].vec2);
			item.lpCurValue := LPARAM(str);
			PropGrid_SetItemData(nm.hdr.hwndFrom, nm.iIndex, @item);			
		end;
	end;
end;
	
procedure set_human_cover_properties(ih_grid : Ihandle; cover : THumanCombatCover);
const
	poses_arr : array of String = ['Sit Left', 'Sit Right', 'Stand Left', 'Stand Right', 'Up Left', 'Up Right'];
var
	w_grid : HWND;
	item : PROPGRIDITEM;
	
	str : String;
	
	I : Longint;
	
	m : TMatrix;
	vx, vy, vz : TVec3;
	r : TVec3;
begin
	w_grid := HWND(IupGetAttribute(ih_grid, 'HANDLE_GRID'));
	
	PropGrid_ResetContent(w_grid);
	
	for I := 0 to Length(poses_arr) - 1 do
	begin
		PropGrid_ItemInit(item);
		item.lpszCatalog := CAT_ALLOWED_POSES;
		item.lpszPropName := PAnsiChar(poses_arr[I]);
		item.lpCurValue := LPARAM((cover.flags and (1 shl I)) <> 0);
		item.iItemType := PIT_CHECK;
		PropGrid_AddItem(w_grid, @item);		
	end;
	
	for I := 0 to Length(poses_arr) - 1 do
	begin
		if (cover.flags and (1 shl I)) <> 0 then
		begin
			// Visibility toggle
			PropGrid_ItemInit(item);
			item.lpszCatalog := PAnsiChar(poses_arr[I]);
			item.lpszPropName := PARAM_VISIBLE;
			item.lpszPropDesc := 'Only for convience in editor; doesn''t affect the game';
			item.lpCurValue := LPARAM(cover.e_visible[I]);
			item.iItemType := PIT_CHECK;
			PropGrid_AddItem(w_grid, @item);
			
			// Flags ???
			WriteStr(str, cover.arr[I].flags2, 'd ', IntToHex(cover.arr[I].flags2, 4), 'h');
			
			PropGrid_ItemInit(item);
			item.lpszCatalog := PAnsiChar(poses_arr[I]);
			item.lpszPropName := 'Pose Flags';
			item.lpCurValue := LPARAM(PAnsiChar(str));
			item.iItemType := PIT_STATIC;
			PropGrid_AddItem(w_grid, @item);
		
			// Direction/Rotation
			// Is it related to fire point or enter point ?
			vz := pvDecompress(cover.arr[I].packed_dir);
			MakeMatrixFromDir(vx, vy, vz);
			
			m[1,1] := vx.x; m[1,2] := vy.x; m[1,3] := vz.x; m[1,4] := 0.0; 
			m[2,1] := vx.y; m[2,2] := vy.y; m[2,3] := vz.y; m[2,4] := 0.0; 
			m[3,1] := vx.z; m[3,2] := vy.z; m[3,3] := vz.z; m[3,4] := 0.0; 
			m[4,1] := 0.0; m[4,2] := 0.0; m[4,3] := 0.0; m[4,4] := 1.0; 
			GetHPB(m, r.y, r.x, r.z);
			
			// convert to degrees
			r.y := r.y / (PI/180);
			r.x := r.x / (PI/180);
			r.z := r.z / (PI/180);
			
			//
			str := Vec3ToStr(r);
			
			PropGrid_ItemInit(item);
			item.lpszCatalog := PAnsiChar(poses_arr[I]);
			item.lpszPropName := PARAM_ROTATION;
			item.lpCurValue := LPARAM(PAnsiChar(str));
			item.iItemType := PIT_EDIT;
			PropGrid_AddItem(w_grid, @item);
		
			// Fire Point
			//WriteStr(str, cover.arr[I].vec1.x:1:4, ' ', cover.arr[I].vec1.y:1:4, ' ', cover.arr[I].vec1.z:1:4);
			str := Vec3ToStr(cover.arr[I].vec1);
			
			PropGrid_ItemInit(item);
			item.lpszCatalog := PAnsiChar(poses_arr[I]);
			item.lpszPropName := PARAM_FIREPOINT;
			item.lpszPropDesc := 'Type in single exclamation point ''!'' to use current cover position';
			item.lpCurValue := LPARAM(PAnsiChar(str));
			item.iItemType := PIT_EDIT;
			PropGrid_AddItem(w_grid, @item);
			
			// Fire Point Cell
			WriteStr(str, cover.arr[I].cell1);
			
			PropGrid_ItemInit(item);
			item.lpszCatalog := PAnsiChar(poses_arr[I]);
			item.lpszPropName := 'Fire Point Cell';
			item.lpCurValue := LPARAM(PAnsiChar(str));
			item.iItemType := PIT_STATIC;
			PropGrid_AddItem(w_grid, @item);
			
			// Enter Point
			//WriteStr(str, cover.arr[I].vec2.x:1:4, ' ', cover.arr[I].vec2.y:1:4, ' ', cover.arr[I].vec2.z:1:4);
			str := Vec3ToStr(cover.arr[I].vec2);
			
			PropGrid_ItemInit(item);
			item.lpszCatalog := PAnsiChar(poses_arr[I]);
			item.lpszPropName := PARAM_ENTERPOINT;
			item.lpszPropDesc := 'Type in single exclamation point ''!'' to use current cover position';
			item.lpCurValue := LPARAM(PAnsiChar(str));
			item.iItemType := PIT_EDIT;
			PropGrid_AddItem(w_grid, @item);
			
			// Enter Point Cell
			WriteStr(str, cover.arr[I].cell2);
			
			PropGrid_ItemInit(item);
			item.lpszCatalog := PAnsiChar(poses_arr[I]);
			item.lpszPropName := 'Enter Point 270 Cell';
			item.lpCurValue := LPARAM(PAnsiChar(str));
			item.iItemType := PIT_STATIC;
			PropGrid_AddItem(w_grid, @item);
		end;
	end;
	
	PropGrid_ExpandAllCatalogs(w_grid);
end;
	
function grid_container_subclass_proc(_hwnd : HWND; _msg : UINT; _wparam : WPARAM; _lparam : LPARAM; subclass_id : UINT_PTR; ref_data : DWORD_PTR) : LRESULT; stdcall;
var
	w_grid : HWND;
begin
	if _msg = WM_SIZE then
	begin
		w_grid := HWND(ref_data);
		MoveWindow(w_grid, 0, 0, LOWORD(_lparam), HIWORD(_lparam), TRUE);		
	end;
	
	if _msg = WM_NOTIFY then
	begin
		if _wparam = 444 then
			handle_param_change(LPNMPROPGRID(_lparam));
	end;
	
	Result := DefSubclassProc(_hwnd, _msg, _wparam, _lparam);
end;

function t_grid_map_cb(ih : Ihandle) : Longint; cdecl;
const
	WND_GRID_ID = 444;
var
	w_parent : HWND;
	w_grid : HWND;
begin
	w_parent := HWND(IupGetAttribute(ih, 'WID'));

	InitPropertyGrid(GetModuleHandle(PROPERTYGRIDDLLNAME));
	w_grid := New_PropertyGrid(w_parent, WND_GRID_ID);
	
	// ensure propertyGrid visibility
	SetWindowLong(w_parent, GWL_STYLE, GetWindowLong(w_parent, GWL_STYLE) or WS_CLIPCHILDREN); 
	// handle resizing & param changing
	SetWindowSubclass(w_parent, grid_container_subclass_proc, 1555, DWORD_PTR(w_grid)); 
	
	PropGrid_ShowPropertyDescriptions(w_grid, TRUE);
	
	IupSetAttribute(ih, 'HANDLE_GRID', PAnsiChar(w_grid));
	
	Result := IUP_DEFAULT;
end;

function CreateTab : Ihandle;
var
	btn_calc_pe_links : Ihandle;
	btn_ugndc : Ihandle;
	btn_chcc : Ihandle;
	btn_savenm : Ihandle;
	btn_sel_invalid : Ihandle;
	rad_links : Ihandle;
	list_cover_type : Ihandle;
	fr_create : Ihandle;

	fr_hcc : Ihandle;
	btn_create_hcc : Ihandle;
	btn_delete_hcc : Ihandle;
	t_grid : Ihandle;
	
	fr_cover : Ihandle;
	t_props : Ihandle;
	
	fr_link : Ihandle;
	t_props_link : Ihandle;	
	
	I : Longint;
	cover_manager_params : TTextKonfig;
	cover_types : TSection;
begin

	btn_calc_pe_links := iup.Button('Calculate PE links', @btn_calc_pe_links_cb);
	
	rad_links := IupRadio(IupHBox(
		IupSetAttributes(iup.Toggle('ulinks', @tg_links_cb, True), 'NAME=ULINKS'),
		IupSetAttributes(iup.Toggle('ulinks_pe', @tg_links_cb, False), 'NAME=ULINKS_PE'),
		nil
	));
	
	btn_ugndc := iup.Button('Update ground_cell''s and conn_reg', @btn_update_gnd_cells_cb);
	btn_chcc := iup.Button('Compare HCC', @btn_compare_human_combat_covers_cb);
	btn_savenm := iup.Button('Save navmap as RAW', @btn_savenm_cb);
	btn_sel_invalid := iup.Button('Select invalid cover', @btn_sel_invalid_cb);
	
	list_cover_type := IupList(nil);
	IupSetAttribute(list_cover_type, 'NAME', 'LIST_COVER_TYPE');
	IupSetAttribute(list_cover_type, 'VISIBLELINES', '7');
	IupSetAttribute(list_cover_type, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(list_cover_type, 'DROPDOWN', 'YES');
	IupSetAttribute(list_cover_type, 'TIP', 'Cover type');
	
	cover_manager_params := Engine.GetCoverManagerParams;
	if cover_manager_params <> nil then
	begin
		cover_types := cover_manager_params.root.GetSect('cover_type', False);
		if cover_types <> nil then
		begin
			for I := 1 to cover_types.ParamCount - 1 do
			begin
				iup.SetStrAttribute(list_cover_type, IntToStr(I), '#' + IntToStr(I-1) + ': ' + (cover_types.GetParam(I) as TSection).GetStrDef('name', ''));
			end;
		end;
	end;
	
	IupSetAttribute(list_cover_type, 'VALUE', '2'); // select 'human_combat' by default
	
	fr_create := IupFrame(
		IupVBox(
			IupSetAttributes(IupHBox(
				IupSetAttributes(iup.AutoToggle('AI-map', @attach_to_aimap), 'TIP="Attach newly created cover to AI-map (aka NavMesh)"'),
				IupSetAttributes(iup.AutoToggle('Select', @select_created), 'TIP="Select newly created cover"'),
				IupSetAttributes(iup.AutoToggle('Auto link', @auto_link), 'TIP="Create a link from currently selected cover to newly created"'),
				nil
			), 'MARGIN=0x0'),
			list_cover_type,
			nil
		)
	);
	
	IupSetAttribute(fr_create, 'TITLE', 'Create');

	// cover frame
	if uLEOptions.props_two_column then
	begin
		t_props := IupFlatTree;
		IupSetAttribute(t_props, 'EXTRATEXTWIDTH', '200');
	end else
		t_props := IupTree;

	IupSetAttributes(t_props, 'NAME=TREE_PROPS_COVER, RASTERSIZE=200x');
	//IupSetCallback(t_props, 'PROPS_EDIT_CB', @property_edit_cb);
	//IupSetCallback(t_props, 'PROPS_BEFORE_CHANGE_CB', @property_before_change_cb);
	
	fr_cover := IupFrame(
		IupVBox(
			IupSetAttributes(IupHBox(
				iup.Button('Attach to AI-map', @btn_attach_to_aimap_cb), 
				iup.Button('Detach from AI-map', @btn_detach_from_aimap_cb), 
				nil
			), 'MARGIN=0x0'),
			t_props,
			nil
		)
	);
	
	IupSetAttribute(fr_cover, 'NAME', 'FRAME_COVER');
	IupSetAttribute(fr_cover, 'TITLE', 'Cover');
	
	IupSetAttributes(fr_cover, 'VISIBLE=NO, FLOATING=YES'); // hide by default

	// link frame	
	if uLEOptions.props_two_column then
	begin
		t_props_link := IupFlatTree;
		IupSetAttribute(t_props_link, 'EXTRATEXTWIDTH', '200');
	end else
		t_props_link := IupTree;

	IupSetAttributes(t_props_link, 'NAME=TREE_PROPS_COVER_LINK, RASTERSIZE=200x');
	//IupSetCallback(t_props_link, 'PROPS_EDIT_CB', @property_edit_cb);
	//IupSetCallback(t_props_link, 'PROPS_BEFORE_CHANGE_CB', @property_before_change_cb);
	
	fr_link := IupFrame(
		IupVBox(t_props_link, nil)
	);
	
	IupSetAttribute(fr_link, 'NAME', 'FRAME_LINK');
	IupSetAttribute(fr_link, 'TITLE', 'Link');
	
	IupSetAttributes(fr_link, 'VISIBLE=NO, FLOATING=YES'); // hide by default

	// human combat cover
	btn_create_hcc := iup.Button('Create Combat Cover', @btn_create_hcc_cb);
	IupSetAttribute(btn_create_hcc, 'NAME', 'BTN_CREATE_HCC');
	
	btn_delete_hcc := iup.Button('Delete Combat Cover', @btn_delete_hcc_cb);
	IupSetAttribute(btn_delete_hcc, 'NAME', 'BTN_DELETE_HCC');
	
	t_grid := IupCanvas(nil);
	IupSetAttribute(t_grid, 'NAME', 'PROPS_HCC');
	IupSetAttribute(t_grid, 'BORDER', 'NO');
	IupSetCallback(t_grid, 'MAP_CB', @t_grid_map_cb);
	
	fr_hcc := IupFrame(
		IupVBox(
			IupSetAttributes(IupHBox(btn_create_hcc, btn_delete_hcc, nil), 'MARGIN=0x0'),
			t_grid, 
			nil
		)
	);
	
	IupSetAttribute(fr_hcc, 'NAME', 'FRAME_HUMAN_COMBAT_COVER');
	IupSetAttribute(fr_hcc, 'TITLE', 'Human Combat Cover');	
	
	IupSetAttributes(fr_hcc, 'VISIBLE=NO, FLOATING=YES'); // hide by default

	Result := IupVBox(btn_calc_pe_links, rad_links, btn_ugndc, btn_chcc, btn_savenm, btn_sel_invalid, fr_create, fr_cover, fr_link, fr_hcc, nil);
end;

procedure UpdateTab;
var
	tab : Ihandle;
	fr_cover : Ihandle;
	fr_link : Ihandle;
	fr_hcc : Ihandle;
	t_props : Ihandle;
	t_props_link : Ihandle;
	
	btn_create_hcc : Ihandle;
	btn_delete_hcc : Ihandle;
	hcc_props : Ihandle;
	
	c_arr : TCoverArray;
	hcc : THumanCombatCover;
begin
	fr_cover := IupGetDialogChild(MainDialog, 'FRAME_COVER');
	fr_link := IupGetDialogChild(MainDialog, 'FRAME_LINK');
	fr_hcc := IupGetDialogChild(MainDialog, 'FRAME_HUMAN_COMBAT_COVER');
	t_props := IupGetDialogChild(MainDialog, 'TREE_PROPS_COVER');
	t_props_link := IupGetDialogChild(MainDialog, 'TREE_PROPS_COVER_LINK');
	
	btn_create_hcc := IupGetDialogChild(MainDialog, 'BTN_CREATE_HCC');
	btn_delete_hcc := IupGetDialogChild(MainDialog, 'BTN_DELETE_HCC');
	hcc_props := IupGetDialogChild(MainDialog, 'PROPS_HCC');
	
	c_arr := Scene.GetSelectedCovers;
	if Length(c_arr) = 1 then
	begin
		SetupProperties(t_props, c_arr[0].data);
		IupSetAttribute(fr_cover, 'VISIBLE', 'YES');
		IupSetAttribute(fr_cover, 'FLOATING', 'NO');
		
		if c_arr[0].data.GetInt('cover_type_id', 0, 'u8') = 1 then // if it's human combat cover
		begin
			IupSetAttributes(fr_hcc, 'VISIBLE=YES, FLOATING=NO');
			
			hcc := Scene.cover_manager.FindHumanCombatCover(c_arr[0].ID);
			if hcc <> nil then
			begin
				IupSetAttribute(btn_create_hcc, 'ACTIVE', 'NO');
				IupSetAttribute(btn_delete_hcc, 'ACTIVE', 'YES');
				
				IupSetAttributes(hcc_props, 'VISIBLE=YES, FLOATING=NO');
				set_human_cover_properties(hcc_props, hcc); 
			end else
			begin
				IupSetAttribute(btn_create_hcc, 'ACTIVE', 'YES');
				IupSetAttribute(btn_delete_hcc, 'ACTIVE', 'NO');
			
				IupSetAttributes(hcc_props, 'VISIBLE=NO, FLOATING=YES');
			end;
			
		end else
			IupSetAttributes(fr_hcc, 'VISIBLE=NO, FLOATING=YES');
	end else
	begin
		IupSetAttributes(fr_cover, 'VISIBLE=NO, FLOATING=YES');
		IupSetAttributes(fr_hcc, 'VISIBLE=NO, FLOATING=YES');
	end;
			
	if sel_link <> nil then
	begin
		SetupProperties(t_props_link, sel_link.data);
		IupSetAttribute(fr_link, 'VISIBLE', 'YES');
		IupSetAttribute(fr_link, 'FLOATING', 'NO');
	end else
	begin
		IupSetAttribute(fr_link, 'VISIBLE', 'NO');
		IupSetAttribute(fr_link, 'FLOATING', 'YES');
	end;
	
	tab := IupGetParent(fr_cover);
	IupRefresh(tab);
end;

procedure CreateCover(hit_pos, hit_nrm : TVec3);
var
	dlg : Ihandle;
	list_cover_type : Ihandle;
	cover_type_id : Longint;
	
	cover_id : Word;
	pe_index : Word;
	
	data : TSection;
	cover : TCover;
	
	c_arr : TCoverArray;
begin
	dlg := IupGetHandle('MAINDIALOG');
	list_cover_type := IupGetDialogChild(dlg, 'LIST_COVER_TYPE');
					
	cover_type_id := IupGetInt(list_cover_type, 'VALUE') - 1;
	if cover_type_id < 0 then
		Exit;

	cover_id := Scene.cover_manager.GenerateCoverId;
	
	if cover_id = 65535 then
	begin
		uEditorUtils.ShowError('Cannot find free ID for cover!');
		Exit;
	end;
	
	if attach_to_aimap then
	begin
		pe_index := Scene.cover_manager.GenerateCoverPEIndex;
		if pe_index = 65535 then
		begin
			uEditorUtils.ShowError('Cannot find free ''pe_index'' for cover!');
			Exit;
		end;
	end else
		pe_index := 65535;
		
	data := TSection.Create('cover');
	data.AddVec3('position', hit_pos);
	data.AddInt('ground_cell', -1, 's32');
	data.AddInt('conn_reg', -1, 's32');
	data.AddInt('dist_in_dir', 0, 'u32'); // what does it mean ?
	data.AddInt('cover_id', cover_id, 'u16');
	data.AddInt('group0', 255, 'u8');
	data.AddInt('group1', 255, 'u8');
	data.AddInt('pe_index', pe_index, 'u16');
	data.AddInt('direction', 0, 'u16');
	data.AddInt('cover_type_id', cover_type_id, 'u8');
	data.AddHint('allowed_actions', 'flags8');
	data.AddInt('allowed_actions', 32, 'u8');
	data.AddInt('radius', 128, 'fp32_q8');
	
	cover := TCover.Create(Scene.ph_scene, data);
	Scene.cover_manager.AddCover(cover);
	
	if attach_to_aimap then
		Scene.cover_manager.UpdateCoverPosition(cover);
		
	if auto_link then
	begin
		c_arr := Scene.GetSelectedCovers;
		if Length(c_arr) = 1 then
			CreateLink(c_arr[0], cover);
	end;
		
	if select_created then
	begin
		DeselectAll;
		cover.Selected := True;
		UpdateSelection;
	end;
end;

procedure CreateLink(from, _to : TCover);
var
	data : TSection;
begin
	data := TSection.Create('link');
	data.AddInt('from', from.ID, 'cover_link, ucover_link');
	data.AddInt('to', _to.ID, 'cover_link, ucover_link');
	data.AddHint('move_action', 'animation_str');
	data.AddStr('move_action', '');
	data.AddHint('move_action_add', 'animation_str');
	data.AddStr('move_action_add', '');
	if Scene.GetVersion >= sceneVerLL then
		data.AddInt('cost', 0, 'u32');
	data.AddInt('trans_frame', 0, 'u16');
	data.AddInt('trans_speed', 0, 'u8');
	data.AddInt('lnk_type', 1, 'u32');
	data.AddInt('type', 0, 'u32');
	data.AddInt('anim_state', 255, 'u8');
	data.AddInt('mental_state', 255, 'u8');	
	
	Scene.cover_manager.AddLink(
		TCoverLink.Create(Scene.ph_scene, data, Scene.cover_manager.covers)
	);
end;

procedure CreateHumanCombatCover(for_cover : TCover);
var
	hcc : THumanCombatCover;
begin
	hcc := THumanCombatCover.Create(Scene.ph_scene);
	hcc.cover_id := for_cover.ID;
	Insert(hcc, Scene.cover_manager.human_combat_covers, Length(Scene.cover_manager.human_combat_covers));
end;

procedure DeleteHumanCombatCover(of_cover : TCover);
var
	id : Word;
	I : Longint;
begin
	id := of_cover.ID;
	for I := 0 to Length(Scene.cover_manager.human_combat_covers) - 1 do
		if Scene.cover_manager.human_combat_covers[I].cover_id = id then
		begin
			Scene.cover_manager.human_combat_covers[I].Free;
			Delete(Scene.cover_manager.human_combat_covers, I, 1);
			Exit;
		end;
end;

procedure DeselectAll;
var
	I : Longint;
begin
	for I := 0 to 65534 do
		if Scene.cover_manager.covers[I] <> nil then
			Scene.cover_manager.covers[I].Selected := False;
			
	for I := 0 to Length(Scene.cover_manager.links) - 1 do
		Scene.cover_manager.links[I].Selected := False;
			
	for I := 0 to Length(Scene.cover_manager.links_pe) - 1 do
		Scene.cover_manager.links_pe[I].Selected := False;
end;

procedure DeleteSelection;
var
	objs_to_delete : TList;
	I : Longint;
begin
	objs_to_delete := TList.Create;
	
	// Delete selected links
	for I := 0 to Length(Scene.cover_manager.links) - 1 do
		if Scene.cover_manager.links[I].Selected then
			objs_to_delete.Add(Scene.cover_manager.links[I]);
			
	for I := 0 to objs_to_delete.Count - 1 do
		Scene.cover_manager.RemoveLink(TCoverLink(objs_to_delete[I]));
		
	objs_to_delete.Clear;
		
	// Delete selected covers
	for I := 0 to 65534 do
		if Scene.cover_manager.covers[I] <> nil then
			if Scene.cover_manager.covers[I].Selected then
				objs_to_delete.Add(Scene.cover_manager.covers[I]);
			
	for I := 0 to objs_to_delete.Count - 1 do
	begin
		DeleteHumanCombatCover(TCover(objs_to_delete[I]));
		Scene.cover_manager.RemoveCover(TCover(objs_to_delete[I]));
	end;
	
	objs_to_delete.Free;
end;

end.