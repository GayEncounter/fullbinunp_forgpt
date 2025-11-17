unit script_editor;

interface
uses Konfig;

function EditScript(s : TSection) : Boolean;

implementation
uses Windows, // for GL-context sharing 
     classes, sysutils, Iup, GL, GLU, vmath, properties, 
     uScene,
     uEntity,
     glfont,
     skeleton, uChoose, // for properties
     Math, // for Max
     script_block_descs,
     framework, // for so called 'commons vs' loading
     Engine, // for overly specific version detection
     uLEOptions, uEditorUtils;

// utility functions

procedure DrawCircle(x, y : Single; r : Single);
var
	I : Integer;
	a : Single;
begin
	glBegin(GL_TRIANGLE_FAN);
	glVertex2f(x, y);
	a := 0;
	for I := 0 to 15 do
	begin
		glVertex2f(x + (r * Sin(a)), y + (r * Cos(a)));
		a := a + (PI*2 / 15); 
	end;
	glEnd;
end;

procedure DrawLine(x1, y1, x2, y2 : Single);
const
	ArrowHeadSize = 8.0;
var
	d, p1, p2, l1, l2 : TVec2;
begin
	d.x := x1 - x2;
	d.y := y1 - y2;
	Normalize(d);
	
	p1.x := -d.y;
	p1.y := d.x;
	
	p2.x := d.y;
	p2.y := -d.x;
	
	l1.x := d.x + p1.x;
	l1.y := d.y + p1.y;
	Normalize(l1);
	
	l2.x := d.x + p2.x;
	l2.y := d.y + p2.y;
	Normalize(l2);
	
	glBegin(GL_LINES);
	glVertex2f(x1, y1);
	glVertex2f(x2, y2);
	glEnd;
	
	glBegin(GL_TRIANGLES);
	glVertex2f(x2, y2);
	glVertex2f(x2 + l1.x*ArrowHeadSize, y2 + l1.y*ArrowHeadSize);
	glVertex2f(x2 + l2.x*ArrowHeadSize, y2 + l2.y*ArrowHeadSize);
	glEnd;
end;

{
function Pow(x, y : Single) : Single;
begin
	Pow := exp(y*ln(abs(x)));
end;

procedure DrawCurve(x1, y1, x2, y2, cx1, cy1, cx2, cy2 : Single);
const
	Points = 50;
var
	I : Integer;
	t : Single;
	p : TVec2;
begin
	glBegin(GL_LINE_STRIP);

	t := 0;
	for I := 0 to Points do
	begin
		p.x := Pow(1 - t, 3.0) * x1 + 3 * t * Pow(1 - t, 2.0) * cx1 + 3 * (1 - t) * Pow(t, 2.0) * cx2 + Pow(t, 3.0) * x2;
		p.y := Pow(1 - t, 3.0) * y1 + 3 * t * Pow(1 - t, 2.0) * cy1 + 3 * (1 - t) * Pow(t, 2.0) * cy2 + Pow(t, 3.0) * y2;
	
		glVertex2f(p.x, p.y);
	
		t := t + 1/Points;
	end;
	
	glEnd;
end;
}

function Pow2(x : Single) : Single;
begin
	Pow2 := x*x;
end;

function Pow3(x : Single) : Single;
begin
	Pow3 := x*x*x;
end;

procedure DrawCurve(x1, y1, x2, y2, cx1, cy1, cx2, cy2 : Single);
const
	Points = 50;
var
	I : Integer;
	t : Single;
	p : TVec2;
begin
	glBegin(GL_LINE_STRIP);

	t := 0;
	for I := 0 to Points do
	begin
		p.x := Pow3(1 - t) * x1 + 3 * t * Pow2(1 - t) * cx1 + 3 * (1 - t) * Pow2(t) * cx2 + Pow3(t) * x2;
		p.y := Pow3(1 - t) * y1 + 3 * t * Pow2(1 - t) * cy1 + 3 * (1 - t) * Pow2(t) * cy2 + Pow3(t) * y2;
	
		glVertex2f(p.x, p.y);
	
		t := t + 1/Points;
	end;
	
	glEnd;
end;

const
	SCRIPT_VER_LL_BETA_15_10_2012 = 18;
	SCRIPT_VER_LL                 = 20;
	SCRIPT_VER_REDUX              = 20; // version same as LL
	SCRIPT_VER_ARKTIKA1           = 46;
	SCRIPT_VER_EXODUS             = 55; // or 54
	
const
	TYPE_BLOCK      = $01000000;
	TYPE_LINK       = $02000000;
	TYPE_INPOINT    = $03000000;
	TYPE_OUTPOINT   = $04000000;
	
	NAME_MASK       = $00FFFFFF;
	TYPE_MASK       = $FF000000;

type
	TBlock = class
		clsid : String;
		posx, posy : Longint;
		sizex, sizey : Longint;
		desc : TBlockDesc;
		free_desc : Boolean;
		
		props : TSection;
			
		constructor Create(blk : TSection); overload;
		constructor Create(desc : TBlockDesc); overload;
		destructor Destroy; override;
		
		procedure UpdateDesc;
	end;
	
	TLink = class
		block_from, block_to : TBlock;
		point_from, point_to : Integer;
	end;
	
constructor TBlock.Create(blk : TSection);
var
	param_posx, param_posy : TIntegerValue;
	param_clsid : TStringValue;
	
	I : Longint;
	
	v : TSimpleValue;
begin
	inherited Create;
	
	param_posx := blk.GetParam('posx', 'u16') as TIntegerValue;
	param_posy := blk.GetParam('posy', 'u16') as TIntegerValue;
	param_clsid := blk.GetParam('clsid', 'stringz') as TStringValue;
		
	posx := Smallint(param_posx.num) * 6;
	posy := Smallint(param_posy.num) * 6;
	clsid := param_clsid.str;
				
	props := TSection.Create(clsid);
	for I := 0 to blk.items.Count - 1 do
	begin
		v := blk.items[I];
		if not ((v.name = 'clsid') or (v.name = 'posx') or (v.name = 'posy')) then
			props.items.Add(v.Copy);
	end;
	
	UpdateDesc;
end;

constructor TBlock.Create(desc : TBlockDesc);
begin
	inherited Create;
	
	clsid := desc.clsid;
	self.desc := desc;
	
	props := TSection(desc.props.Copy);
	props.name := clsid;
end;

destructor TBlock.Destroy;
begin
	props.Free;
	
	if free_desc then
		desc.Free;
	
	inherited Destroy;
end;

procedure TBlock.UpdateDesc;
begin
	if free_desc then
		desc.Free;
		
	desc := GetBlockDesc(clsid, props, free_desc);
end;
	
type
	TScriptEditor = class
		blocks : TList;
		links : TList;
		script_version : Word;
	
		dlg : Ihandle;	
		font : TGLFont;
		
		mousex, mousey : Longint;
		offsetx, offsety : Longint;
		sizex, sizey : Longint;
		scale : Single;
		
		over_block : TBlock;
		over_link : TLink;
		over_outpoint : Longint;
		over_inpoint : Longint;
		
		sel_block : TBlock;
		
		// link creation
		start_block : TBlock;
		start_outpoint : Longint;
		start_inpoint : Longint;
		
		
		constructor Create;
		destructor Destroy; override;
		
		function ShowModal : Longint;
		
		procedure ClearScript;
		procedure OpenScript;
		procedure LoadScript(script : TSection);
		procedure SaveScript(script : TSection);
		
		function Empty : Boolean;
		
		function GetInPoint(const block : TBlock; n : Longint) : TVec2;
		function GetOutPoint(const block : TBlock; n : Longint) : TVec2;
		
		procedure SelectBlock(block : TBlock);
		procedure RemoveBlock(block : TBlock);
		
		procedure DrawBlocks;
		procedure DrawLinks;
		
		procedure ProcessHits(count : Longint; hits : array of GLuint);
		
		function GetTargetEntity(block : TBlock) : TEntity;
		function FindLink(block_from, block_to : TBlock; point_from, point_to : Integer) : TLink;
		
		// maybe store canvas handle as field and remove ih parameter ?
		function gl_map_cb(ih : Ihandle) : Longint;
		function gl_unmap_cb(ih : Ihandle) : Longint;
		function gl_resize_cb(ih : Ihandle; w, h : Longint) : Longint;
		function gl_button_cb(ih : Ihandle; button, pressed : Longint; x, y : Longint; status : PAnsiChar) : Longint;
		function gl_motion_cb(ih : Ihandle; x, y : Longint; status : PAnsiChar) : Longint;
		function gl_wheel_cb(ih : Ihandle; amount : Single; x, y : Longint; status : PAnsiChar) : Longint;
		function gl_redraw_cb(ih : Ihandle; x, y : Longint) : Longint;
	end;
	
function gl_map_cb_wrapper(ih : Ihandle) : Longint; cdecl;
begin
	Result := TScriptEditor(IupGetAttribute(IupGetDialog(ih), 'TScriptEditor->this')).gl_map_cb(ih);
end;

function gl_unmap_cb_wrapper(ih : Ihandle) : Longint; cdecl;
begin
	Result := TScriptEditor(IupGetAttribute(IupGetDialog(ih), 'TScriptEditor->this')).gl_unmap_cb(ih);
end;

function gl_resize_cb_wrapper(ih : Ihandle; x, y : Longint) : Longint; cdecl;
begin
	Result := TScriptEditor(IupGetAttribute(IupGetDialog(ih), 'TScriptEditor->this')).gl_resize_cb(ih, x, y);
end;

function gl_button_cb_wrapper(ih : Ihandle; button, pressed : Longint; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
begin
	Result := TScriptEditor(IupGetAttribute(IupGetDialog(ih), 'TScriptEditor->this')).gl_button_cb(ih, button, pressed, x, y, status);
end;	

function gl_motion_cb_wrapper(ih : Ihandle; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
begin
	Result := TScriptEditor(IupGetAttribute(IupGetDialog(ih), 'TScriptEditor->this')).gl_motion_cb(ih, x, y, status);
end;

function gl_wheel_cb_wrapper(ih : Ihandle; amount : Single; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
begin
	Result := TScriptEditor(IupGetAttribute(IupGetDialog(ih), 'TScriptEditor->this')).gl_wheel_cb(ih, amount, x, y, status);
end;

function gl_redraw_cb_wrapper(ih : Ihandle; x, y : Longint) : Longint; cdecl;
begin
	Result := TScriptEditor(IupGetAttribute(IupGetDialog(ih), 'TScriptEditor->this')).gl_redraw_cb(ih, x, y);
end;

function btn_save_cb(ih : Ihandle) : Longint; cdecl;
begin
	IupSetAttribute(IupGetDialog(ih), 'VALUE', '1');
	Result := IUP_CLOSE;
end;

function btn_clear_cb(ih : Ihandle) : Longint; cdecl;
begin
	TScriptEditor(IupGetAttribute(IupGetDialog(ih), 'TScriptEditor->this')).ClearScript;
	Result := IUP_DEFAULT;
end;

function btn_open_cb(ih : Ihandle) : Longint; cdecl;
begin
	TScriptEditor(IupGetAttribute(IupGetDialog(ih), 'TScriptEditor->this')).OpenScript;
	Result := IUP_DEFAULT;
end;

function property_edit_cb(tree : Ihandle; sect : TSection; prop : TSimpleValue) : Longint; cdecl;
var
	entity : TEntity;
	skeleton : T4ASkeleton;
	s : TStringValue;
	
	editor : TScriptEditor;
begin
	editor := TScriptEditor(IupGetAttribute(IupGetDialog(tree), 'TScriptEditor->this'));
	
	Result := 1;
	
	entity := editor.GetTargetEntity(editor.sel_block);
	if entity <> nil then
	begin
		skeleton := entity.GetSkeleton;
		if skeleton <> nil then
		begin
			if (prop.vtype = 'bone_id') or (prop.vtype = 'bone_str') then
			begin
				s := sect.GetParam(prop.name, 'stringz') as TStringValue;
		
				if ChooseBone(skeleton, s.str) then
				begin
					properties.UpdateCaption(tree, s);
					Result := 2;
				end else
					Result := 0;
			end;
	
			if (prop.vtype = 'locator_str') then
			begin
				s := sect.GetParam(prop.name, 'stringz') as TStringValue;
				
				if ChooseLocator(skeleton, s.str) then
				begin
					properties.UpdateCaption(tree, s);
					Result := 2;
				end else
					Result := 0;
			end;
	
			if (prop.vtype = 'part_id') then
			begin
				s := sect.GetParam(prop.name, 'stringz') as TStringValue;

				if ChooseBonePart(skeleton, s.str) then
				begin
					properties.UpdateCaption(tree, s);
					Result := 2;
				end else
					Result := 0;
			end;
	
			if (prop.vtype = 'animation_str') then
			begin
				s := sect.GetParam(prop.name, 'stringz') as TStringValue;

				if ChooseAnimation(skeleton, s.str) then
				begin
					properties.UpdateCaption(tree, s);
					Result := 2;
				end else
					Result := 0;
			end;
		end;
	end;
end;

function IndexOf(const s : String; const arr : array of String) : Longint;
var I : Longint;
begin
	IndexOf := -1;
	for I := 0 to Length(arr) - 1 do
	begin
		if arr[I] = s then
		begin
			IndexOf := I;
			Exit;
		end;
	end;
end;

procedure property_after_change_cb(tree : Ihandle; prop : TSimpleValue); cdecl;
var
	editor : TScriptEditor;
	
	old_inpoints : array of String;
	old_outpoints : array of String;
	
	I : Longint;
	remap_idx : array of Longint;
	
	link : TLink;
	del_links : TList;
begin
	editor := TScriptEditor(IupGetAttribute(IupGetDialog(tree), 'TScriptEditor->this'));
	
	if NeedUpdateBlockDesc(editor.sel_block.clsid, prop.name, prop.vtype, editor.sel_block.props) then
	begin
		old_inpoints := editor.sel_block.desc.in_names;
		old_outpoints := editor.sel_block.desc.out_names;
		
		editor.sel_block.UpdateDesc;
		CalcBlockSize(editor.sel_block.desc, editor.font, editor.sel_block.sizex, editor.sel_block.sizey);
		
		// remap points
		del_links := TList.Create;
		
		// remap input points
		SetLength(remap_idx, Length(old_inpoints));
		for I := 0 to Length(old_inpoints) - 1 do
			remap_idx[I] := IndexOf(old_inpoints[I], editor.sel_block.desc.in_names);
			
		for I := 0 to editor.links.Count - 1 do
		begin
			link := TLink(editor.links[I]);	
			if link.block_to = editor.sel_block then
			begin
				if remap_idx[link.point_to] < 0 then
				begin
					if del_links.IndexOf(link) < 0 then
						del_links.Add(link)
				end else
					link.point_to := remap_idx[link.point_to]; 
			end;
		end;
		
		// remap output points
		SetLength(remap_idx, Length(old_outpoints));
		for I := 0 to Length(old_outpoints) - 1 do
			remap_idx[I] := IndexOf(old_outpoints[I], editor.sel_block.desc.out_names);
			
		for I := 0 to editor.links.Count - 1 do
		begin
			link := TLink(editor.links[I]);	
			if link.block_from = editor.sel_block then
			begin
				if remap_idx[link.point_from] < 0 then
				begin
					if del_links.IndexOf(link) < 0 then
						del_links.Add(link)
				end else
					link.point_from := remap_idx[link.point_from]; 
			end;
		end;
		
		// delete links that lost their points
		for I := 0 to del_links.Count - 1 do
		begin
			link := TLink(del_links[I]);	
			editor.links.Remove(link);
			link.Free;
		end;
		
		del_links.Free;
		// end of remap points
		
		editor.SelectBlock(editor.sel_block);
	end;
end;
	
constructor TScriptEditor.Create;
var
	gl, main : Ihandle;
	
	btn_clear : Ihandle;
	btn_save : Ihandle;
	btn_open : Ihandle;
	
	tree_descs : Ihandle;
	fr_create : Ihandle;
	
	tree_props : Ihandle;
	fr_properties : Ihandle;
	
	toolbox : Ihandle;
	
	descs_fn : String;
begin
	inherited Create;

	gl := IupGLCanvas(nil);
	IupSetAttribute(gl, 'NAME', 'GL_CANVAS');
	IupSetAttribute(gl, 'BUFFER', 'DOUBLE');
	IupSetAttribute(gl, 'RASTERSIZE', '800x600');
	
	IupSetCallback(gl, 'MAP_CB', @gl_map_cb_wrapper);
	IupSetCallback(gl, 'UNMAP_CB', @gl_unmap_cb_wrapper);
	IupSetCallback(gl, 'RESIZE_CB', @gl_resize_cb_wrapper);
	IupSetCallback(gl, 'BUTTON_CB', @gl_button_cb_wrapper);
	IupSetCallback(gl, 'MOTION_CB', @gl_motion_cb_wrapper);
	IupSetCallback(gl, 'WHEEL_CB', @gl_wheel_cb_wrapper);
	IupSetCallback(gl, 'ACTION', @gl_redraw_cb_wrapper);
	
	btn_clear := IupButton('Clear', nil);
	IupSetAttribute(btn_clear, 'IMAGE', 'ICON_TRASHCAN');
	IupSetCallback(btn_clear, 'ACTION', @btn_clear_cb);
	
	btn_save := IupButton('Save', nil);
	IupSetAttribute(btn_save, 'IMAGE', 'ICON_DISKETTE');
	IupSetCallback(btn_save, 'ACTION', @btn_save_cb);
	
	btn_open := IupButton('Open', nil);
	IupSetAttribute(btn_open, 'IMAGE', 'ICON_FISHCAN');
	IupSetCallback(btn_open, 'ACTION', @btn_open_cb);
	
	tree_descs := IupTree;
	IupSetAttribute(tree_descs, 'NAME', 'TREE_DESCS');
	IupSetAttribute(tree_descs, 'RASTERSIZE', '200x');
	IupSetAttribute(tree_descs, 'ADDROOT', 'NO');
	//IupSetInt(tree_descs, 'VISIBLELINES', 20);
	
	IupSetAttribute(tree_descs, 'IMAGEBRANCHCOLLAPSED', 'ICON_FOLDER1');
	IupSetAttribute(tree_descs, 'IMAGEBRANCHEXPANDED', 'ICON_FOLDER2');
	IupSetAttribute(tree_descs, 'IMAGELEAF', 'IMGBLANK');
	
	fr_create := IupFrame(IupVBox(tree_descs, nil));
	IupSetAttribute(fr_create, 'TITLE', 'Create');
	
	if uLEOptions.props_two_column then
	begin
		tree_props := IupFlatTree;
		IupSetAttribute(tree_props, 'EXTRATEXTWIDTH', '200');
	end else
		tree_props := IupTree;
		
	IupSetAttribute(tree_props, 'NAME', 'TREE_PROPS');
	IupSetAttribute(tree_props, 'RASTERSIZE', '200x');
	IupSetCallback(tree_props, 'PROPS_EDIT_CB', @property_edit_cb);
	IupSetCallback(tree_props, 'PROPS_AFTER_CHANGE_CB', @property_after_change_cb);
	
	fr_properties := IupFrame(IupVBox(tree_props, nil));
	IupSetAttribute(fr_properties, 'NAME', 'FRAME_PROPERTIES');
	IupSetAttribute(fr_properties, 'TITLE', 'Properties');
	IupSetAttribute(fr_properties, 'VISIBLE', 'NO');
	
	toolbox := IupVBox(
		IupSetAttributes(IupHBox(btn_clear, btn_save, btn_open, nil), 'MARGIN=0x0'),
		fr_create,
		fr_properties,
		nil
	);
	IupSetAttribute(toolbox, 'MARGIN', '10x10');
	IupSetAttribute(toolbox, 'GAP', '5x5');
	
	dlg := IupDialog(IupSplit(toolbox, gl));
	IupSetAttribute(dlg, 'TITLE', 'Script Editor');
	IupSetAttribute(dlg, 'VALUE', '0'); // set to 1 if save button clicked
	IupSetAttribute(dlg, 'TScriptEditor->this', Pointer(self));
	
	IupMap(dlg);
	

	case Scene.GetVersion of
		sceneVer2033:
			descs_fn := 'block_descs';
		sceneVerLL:
			case Engine.version of
				eVerLLBeta15102012:
					descs_fn := 'block_descs_ll_beta_15_10_2012';
				eVerLLBeta03122012:
					descs_fn := 'block_descs_ll_beta_3_12_2012';
				else
					descs_fn := 'block_descs_ll';
			end;
		sceneVerRedux:
			descs_fn := 'block_descs_redux';
		sceneVerArktika1: 
			descs_fn := 'block_descs_a1';
		sceneVerExodus:
			descs_fn := 'block_descs_exodus';
		else              
			descs_fn := 'block_descs';
	end;
	
	try		
		LoadDescs(descs_fn, tree_descs);
	except 
		on E: Exception do 
			uEditorUtils.ShowError('Block descriptions loading failed (' + descs_fn + ')'#10 + E.Message);
	end;
	
	blocks := TList.Create;
	links := TList.Create;
	script_version := 0;
	
	scale := 1.0;
	
	over_outpoint := -1;
	over_inpoint := -1;
	
	start_outpoint := -1;
	start_inpoint := -1;
end;

destructor TScriptEditor.Destroy;
begin
	IupDestroy(dlg);
	
	ClearScript;
	blocks.Free;
	links.Free;
	
	UnloadDescs;
	
	inherited Destroy;
end;

function TScriptEditor.ShowModal : Longint;
begin
	IupPopup(dlg, IUP_CURRENT, IUP_CURRENT); 
	Result := IupGetInt(dlg, 'VALUE');
end;

procedure TScriptEditor.ClearScript;
var
	I : Integer;
	gl : Ihandle;
begin
	for I := 0 to blocks.Count - 1 do
		TBlock(blocks[I]).Free;
	blocks.Clear;
	
	for I := 0 to links.Count - 1 do
		TLink(links[I]).Free;
	links.Clear;
	
	gl := IupGetDialogChild(dlg, 'GL_CANVAS');
	IupRedraw(gl, 0);
end;

procedure TScriptEditor.OpenScript;
var
	dlg : Ihandle;
	fn : String;
	script_name : String;
	
	fw : TFramework;
	k : TKonfig;
	tk : TTextKonfig;
	
	decompile_ok : Boolean;
begin
	dlg := IupFileDlg;
	IupSetAttribute(dlg, 'DIALOGTYPE', 'OPEN');
	IupSetAttribute(dlg, 'EXTFILTER', 'Binary file (*.bin)|*.bin|All files (*.*)|*.*|');

	IupPopup(dlg, IUP_CENTER, IUP_CENTER);

	if IupGetInt(dlg, 'STATUS') = 0 then
	begin
		case Scene.GetVersion of
			sceneVer2033:  script_name := 'js\2033\visualscript.js';
			sceneVerLL:    script_name := 'js\ll\visualscript.js';
			sceneVerRedux: script_name := 'js\redux\visualscript.js';
			else begin
				uEditorUtils.ShowError('Not supported');
				script_name := '';
			end;
		end;
	
		if script_name <> '' then
		begin
			fn := IupGetAttribute(dlg, 'VALUE');
			
			k := TKonfig.Create;
			k.Load(fn);
			
			fw := TFramework.Create;
			tk := fw.DecompileKonfig(k, script_name, @decompile_ok);
			fw.Destroy;
			
			k.Free;
			
			if decompile_ok then
			begin
				LoadScript(tk.root.GetSect('script'));
				iup.SetStrAttribute(self.dlg, 'TITLE', 'Script Editor - [' + fn + ']');
			end;
			
			// who owns tk now ?
		end;
	end;

	IupDestroy(dlg);
end;

procedure TScriptEditor.LoadScript(script : TSection);
var
	sect_blocks, blk : TSection;
	
	lnk_count : TIntegerValue;
	lnk : TIntegerArrayValue;
	
	I : Integer;
	
	block : TBlock;
	link : TLink;
begin
	ClearScript;
	
	sect_blocks := script.GetParam('blocks', 'section') as TSection;
	script_version := sect_blocks.GetInt('version', 0, 'u16');
	
	for I := 0 to sect_blocks.items.Count - 1 do
	begin
		if TObject(sect_blocks.items[I]) is TSection then
		begin
			blk := TSection(sect_blocks.items[I]);
		
	 		block := TBlock.Create(blk);
	 		CalcBlockSize(block.desc, font, block.sizex, block.sizey);
			blocks.Add(block);
		end;
	end;
		
	lnk_count := script.GetParam('link_count', 'u32') as TIntegerValue;
	for I := 0 to lnk_count.num - 1 do
	begin
		lnk := script.GetParam(IntToStr(I), 'vec4s16') as TIntegerArrayValue;
		 
		link := TLink.Create; 
		link.block_from := TBlock(blocks[lnk.data[0]]);
		link.point_from := lnk.data[1];
		link.block_to := TBlock(blocks[lnk.data[2]]);
		link.point_to := lnk.data[3];
		links.Add(link);
	end;
	
	// focus on first block
	if blocks.Count > 0 then
	begin
		block := TBlock(blocks[0]);
		
		offsetx := block.posx;
		offsety := block.posy;
	end;
end;

procedure TScriptEditor.SaveScript(script : TSection);
var
	I, J : Integer;
	sgroups, sblocks : TSection;
	
	sblock : TSection;
	block : TBlock;
	n : String;
	
	link : TLink;
	vlink : TVec4S16;
begin
	// ?
	script.AddHint('groups', 'array');
	sgroups := script.AddSect('groups');
	sgroups.AddInt('count', 0, 'u32');
	
	sblocks := script.AddSect('blocks');
	if script_version > 0 then
		sblocks.AddInt('version', script_version, 'u16');
	sblocks.AddInt('block_count', $FFFFFFFF, 'u32');
	sblocks.AddHint('array with no key', 'array');
	sblocks.AddInt('count', blocks.Count, 'u32');
	
	for I := 0 to blocks.Count - 1 do
	begin
		block := TBlock(blocks[I]);
		
		n := IntToStr(I);
		n := StringOfChar('0', 4-Length(n)) + n;
		
		sblock := sblocks.AddSect('block_' + n);
		sblock.AddStr('clsid', block.clsid);
		sblock.AddInt('posx', block.posx div 6, 'u16');
		sblock.AddInt('posy', block.posy div 6, 'u16');
		
		for J := 0 to block.props.items.Count - 1 do
			sblock.items.Add( TSimpleValue(block.props.items[J]).Copy );
	end;
	
	script.AddInt('link_count', links.Count, 'u32');
	for I := 0 to links.Count - 1 do
	begin
		link := TLink(links[I]);

		vlink.x := blocks.IndexOf(link.block_from);
		vlink.y := link.point_from;
		vlink.z := blocks.IndexOf(link.block_to);
		vlink.w := link.point_to;
		
		script.AddVec4S16(IntToStr(I), vlink);
	end;
end;

function TScriptEditor.Empty : Boolean;
begin
	Empty := blocks.Count = 0;
end;

function TScriptEditor.GetInPoint(const block : TBlock; n : Longint) : TVec2;
begin
	Result.x := block.posx;
	Result.y := block.posy + BlockMarginY + font.cell_height*2 + (font.cell_height*n) - (font.cell_height/2);
end;

function TScriptEditor.GetOutPoint(const block : TBlock; n : Longint) : TVec2;
begin
	Result.x := block.posx + block.sizex;
	Result.y := block.posy + BlockMarginY + font.cell_height*2 + (font.cell_height*n) - (font.cell_height/2);
end;

procedure TScriptEditor.SelectBlock(block : TBlock);
var
	tree_props, fr_properties : Ihandle;
begin
	// select block
	tree_props := IupGetDialogChild(dlg, 'TREE_PROPS');
	fr_properties := IupGetDialogChild(dlg, 'FRAME_PROPERTIES');

	sel_block := block;
	if Assigned(sel_block) then
	begin
		IupSetAttribute(fr_properties, 'VISIBLE', 'YES');
		SetupProperties(tree_props, sel_block.props);
	end else
		IupSetAttribute(fr_properties, 'VISIBLE', 'NO');
end;

procedure TScriptEditor.RemoveBlock(block : TBlock);
var
	I : Integer;
	del_links : TList;
	link : TLink;
begin
	// remove all links from- and to that block
	del_links := TList.Create;
	
	for I := 0 to links.Count - 1 do
	begin
		link := TLink(links[I]);
		if (link.block_from = block) or (link.block_to = block) then
			del_links.Add(link);
	end;
	
	for I := 0 to del_links.Count - 1 do
	begin
		links.Remove(del_links[I]);
		TLink(del_links[I]).Free;
	end;
	
	del_links.Free;
	
	// remove block itself
	blocks.Remove(block);
	block.Free;
end;

procedure TScriptEditor.DrawBlocks;
var
	posx, posy : Longint;
	sizex, sizey : Longint;
	
	I, J : Integer;
	s : String;
	
	block : TBlock;
	p : TVec2;
	
	x, y : Longint;
begin	
	for I := 0 to blocks.Count - 1 do
	begin
		block := TBlock(blocks[I]);
		glLoadName(TYPE_BLOCK or I);
		
		posx := block.posx;
		posy := block.posy;
		sizex := block.sizex;
		sizey := block.sizey;
	
		glColor3f(1.0, 1.0, 1.0);
		glBegin(GL_QUADS);
		glVertex2f(posx, posy);
		glVertex2f(posx+sizex, posy);
		glVertex2f(posx+sizex, posy+sizey);
		glVertex2f(posx, posy+sizey);
		glEnd;
		
		if over_block = block then
		begin
			glColor3f(0.6, 0.8, 1.0)
		end else
		begin
			if sel_block = block then
				glColor3f(0.4, 0.6, 1.0)
			else
				glColor3f(0.0, 0.0, 0.0)
		end;
			
		glBegin(GL_LINE_LOOP);
		glVertex2f(posx, posy);
		glVertex2f(posx+sizex, posy);
		glVertex2f(posx+sizex, posy+sizey);
		glVertex2f(posx, posy+sizey);
		glEnd;
		
		glColor3f(1.0, 1.0, 1.0);
		font.DrawText2D(posx + BlockMarginX, posy + BlockMarginY + font.cell_height, block.clsid);
		
		for J := 0 to Length(block.desc.in_names) - 1 do
		begin
			glColor3f(1.0, 1.0, 1.0);
			s := block.desc.in_names[J];	
			
			x := posx + BlockMarginPointX;
			y := posy + BlockMarginY + font.cell_height*2 + font.cell_height*J;
			
			font.DrawText2D(x, y, s);
		
			glPushName(TYPE_INPOINT or J);
			
			if (block = over_block) and (J = over_inpoint) then
				glColor3f(0.5, 0.8, 1.0)
			else
				glColor3f(0.0, 0.0, 0.3);
			
			p := GetInPoint(block, J);
			DrawCircle(p.x + 10, p.y, 3);
			
			glPopName;
		end;
		
		for J := 0 to Length(block.desc.out_names) - 1 do
		begin
			glColor3f(1.0, 1.0, 1.0);
			s := block.desc.out_names[J];
			
			x := posx + sizex - BlockMarginPointX - font.StringWidth(s);
			y := posy + BlockMarginY + font.cell_height*2 + font.cell_height*J;
			
			font.DrawText2D(x, y, s);
		
			glPushName(TYPE_OUTPOINT or J);
		
			if (block = over_block) and (J = over_outpoint) then
				glColor3f(0.5, 0.8, 1.0)
			else
				glColor3f(0.0, 0.0, 0.3);
			
			p := GetOutPoint(block, J);
			DrawCircle(p.x - 10, p.y, 3);
			
			glPopName;
		end;
	end;
end;

procedure TScriptEditor.DrawLinks;
var
	inp : TVec2;
	outp : TVec2;
	
	c1, c2 : TVec2;
	
	I : Integer;
	link : TLink;
begin
	for I := 0 to links.Count - 1 do
	begin
		link := TLink(links[I]);
		glLoadName(TYPE_LINK or I);
	
		outp := GetOutPoint(link.block_from, link.point_from);
		inp	 := GetInPoint(link.block_to, link.point_to);
		
		if over_link = link then
			glColor3f(0.8, 0.0, 0.0)
		else
			glColor3f(0.0, 0.0, 0.8);
			
		if link.block_from = link.block_to then
		begin
			c1.x := outp.x + 75;
			c1.y := outp.y - 150;
			c2.x := inp.x - 75;
			c2.y := inp.y - 150;
		end else
		begin
			//c1.x := outp.x + 150;
			//c1.y := outp.y;
			//c2.x := inp.x - 150;
			//c2.y := inp.y;
			
			c1.x := outp.x + Min(200, Abs(outp.x - inp.x));
			c1.y := outp.y;
			c2.x := inp.x - Min(200, Abs(outp.x - inp.x));
			c2.y := inp.y;
			
			if outp.x > inp.x then
			begin
				if outp.y > inp.y then
				begin
					c1.y := c1.y - 150;
					c2.y := c2.y + 150;
				end else
				begin
					c1.y := c1.y + 150;
					c2.y := c2.y - 150;
				end;
			end;
		end;
		
		//DrawLine(outp.x, outp.y, inp.x, inp.y);
		DrawCurve(outp.x, outp.y, inp.x, inp.y, c1.x, c1.y, c2.x, c2.y);
	end;
end;

procedure TScriptEditor.ProcessHits(count : Longint; hits : array of GLuint);
var
	I, J, ptr : Integer;
	nhits : GLuint;
	
	name : Longint;
begin
	over_block := nil;
	over_link := nil;
	over_inpoint := -1;
	over_outpoint := -1;

	ptr := 0;
	
	for I := 0 to count - 1 do
	begin
		nhits := hits[ptr];
		Inc(ptr, 3);
		
		for J := 0 to nhits - 1 do
		begin
			//if (hits[ptr] and TYPE_MASK) = TYPE_BLOCK then
			//	Write('BLOCK ');
			//if (hits[ptr] and TYPE_MASK) = TYPE_LINK then
			//	Write('LINK ');
			//WriteLn(hits[ptr] and NAME_MASK);
			
			name := hits[ptr] and NAME_MASK;
			case (hits[ptr] and TYPE_MASK) of
				TYPE_BLOCK:    over_block := TBlock(blocks[name]);
				TYPE_LINK:     over_link := TLink(links[name]);
				TYPE_INPOINT:  over_inpoint := name;
				TYPE_OUTPOINT: over_outpoint := name;
			end;
			
			Inc(ptr);
		end;
	end;
end;

function TScriptEditor.GetTargetEntity(block : TBlock) : TEntity;
var
	I : Longint;
	link : TLink;
begin
	Result := nil;
	
	for I := 0 to links.Count - 1 do
	begin
		link := TLink(links[I]);
		
		if link.block_from = block then
		begin
			if 
				(link.point_from < Length(block.desc.out_names)) and 
				(block.desc.out_names[link.point_from] = 'entity')
			then
			begin
				if link.block_to.clsid = 'entities/entity self' then
					Result := Scene.GetSelected[0];
				if link.block_to.clsid = 'entities/entity ref' then
					Result := Scene.EntityById(link.block_to.props.GetInt('target', 'entity_link, uobject_link'));
			end;
			
			if Result <> nil then
				Exit;
		end;
	end;
end;

function TScriptEditor.FindLink(block_from, block_to : TBlock; point_from, point_to : Integer) : TLink;
var
	I : Integer;
	link : TLink;
begin
	for I := 0 to links.Count - 1 do
	begin
		link := TLink(links[I]);
		
		if (link.block_from = block_from) and
		   (link.block_to = block_to) and
		   (link.point_from = point_from) and
		   (link.point_to = point_to) then
		begin
			FindLink := link;
			Exit;
		end;
	end;
	
	FindLink := nil;
end;

function TScriptEditor.gl_map_cb(ih : Ihandle) : Longint;
//var
//	main_gl : Ihandle;
begin
	IupGLMakeCurrent(ih);
	
	// do we even need this ??
//	main_gl := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'GL_CANVAS');
//	wglShareLists(HGLRC(IupGetAttribute(main_gl, 'CONTEXT')), HGLRC(IupGetAttribute(ih, 'CONTEXT')));
	
	glClearColor(0.8, 0.8, 0.8, 0.0);
	glAlphaFunc(GL_GEQUAL, 0.5);
	//glEnable(GL_POLYGON_SMOOTH);
	//glEnable(GL_LINE_SMOOTH);
	
	font := TGLFont.Create('editor_data\segoe_ui.bff');
	
	Result := IUP_DEFAULT;
end;

function TScriptEditor.gl_unmap_cb(ih : Ihandle) : Longint;
begin
	IupGLMakeCurrent(ih);
	font.Free;
	Result := IUP_DEFAULT;
end;

function TScriptEditor.gl_resize_cb(ih : Ihandle; w, h : Longint) : Longint;
begin
	IupGLMakeCurrent(ih);
	
	glViewport(0, 0, w, h);
	
	sizex := w;
	sizey := h;
	
	Result := IUP_DEFAULT;
end;

function TScriptEditor.gl_button_cb(ih : Ihandle; button, pressed : Longint; x, y : Longint; status : PAnsiChar) : Longint;
var
	tree_descs : Ihandle;
	
	link : TLink;
	block : TBlock;
	I : Integer;
	obj : TObject;
begin
	if (button = Ord('1')) and (pressed = 1) then
	begin
	
		if over_outpoint <> -1 then
		begin
			// start link creation (from outpoint to inpoint)
			start_block := over_block;
			start_outpoint := over_outpoint;
			
		end else
		if over_inpoint <> -1 then
		begin
			// start link creation (from inpoint to outpoint)
			start_block := over_block;
			start_inpoint := over_inpoint;
			
		end else
		begin
			SelectBlock(over_block);
		end;		
	end;
	
	if (button = Ord('1')) and (pressed = 0) then
	begin
		if start_block <> nil then
		begin
			if (start_outpoint <> -1) and (over_inpoint <> -1) then
			begin
				if FindLink(start_block, over_block, start_outpoint, over_inpoint) = nil then
				begin
					// make link
					link := TLink.Create;
					link.block_from := start_block;
					link.point_from := start_outpoint;
					link.block_to := over_block;
					link.point_to := over_inpoint;
					links.Add(link)
				end;
			end;
			
			if (start_inpoint <> -1) and (over_outpoint <> -1) then
			begin
				if FindLink(over_block, start_block, over_outpoint, start_inpoint) = nil then
				begin
					// make link
					link := TLink.Create;
					link.block_from := over_block;
					link.point_from := over_outpoint;
					link.block_to := start_block;
					link.point_to := start_inpoint;
					links.Add(link)
				end;
			end;			
			
			// stop link creation
			start_block := nil;
			start_outpoint := -1;
			start_inpoint := -1;
		end;
	end;
	
	if (button = Ord('1')) and iup_isdouble(status) then
	begin
		if Assigned(over_link) then
		begin
			// remove link
			over_link.Free;
			links.Remove(over_link);

			over_link := nil;			
		end;
	end;
	
	if (button = Ord('1')) and (pressed = 1) and iup_iscontrol(status) then
	begin
		// create block
		tree_descs := IupGetDialogChild(ih, 'TREE_DESCS');
		I := IupGetInt(tree_descs, 'VALUE');
		WriteLn('create block 1');
		if I > 0 then
		begin
			obj := TObject(IupGetAttributeId(tree_descs, 'USERDATA', I));
			if obj is TBlockDesc then
			begin
				WriteLn('create block 2 ', I);
				block := TBlock.Create(TBlockDesc(obj));
				block.posx := Trunc(mousex*scale)+offsetx;
				block.posy := Trunc(mousey*scale)+offsety;
				CalcBlockSize(block.desc, font, block.sizex, block.sizey);
				blocks.Add(block);
			end;
		end;
	end;
	
	if (button = Ord('1')) and (pressed = 0) and iup_isshift(status) then
	begin
		if Assigned(over_block) then
		begin
			if over_block = sel_block then
				SelectBlock(nil);
				
			RemoveBlock(over_block);
			over_block := nil;
		end;
	end;

	Result := IUP_DEFAULT;
end;
	
function TScriptEditor.gl_motion_cb(ih : Ihandle; x, y : Longint; status : PAnsiChar) : Longint;
begin
	if iup_isbutton3(status) then
	begin
		offsetx := offsetx - Trunc((x-mousex)*scale);
		offsety := offsety - Trunc((y-mousey)*scale);
	end;
	
	if iup_isbutton1(status) and Assigned(sel_block) and (start_block = nil) then
	begin
		sel_block.posx := sel_block.posx + Trunc((x-mousex)*scale);
		sel_block.posy := sel_block.posy + Trunc((y-mousey)*scale);
	end;
	
	mousex := x;
	mousey := y;
	
	IupRedraw(ih, 0);
	
	Result := IUP_DEFAULT;
end;

function TScriptEditor.gl_wheel_cb(ih : Ihandle; amount : Single; x, y : Longint; status : PAnsiChar) : Longint;
var
	s : Single; // new scale
	diff : Single;
begin
	//WriteLn('wheel amount = ', amount);

	s := scale + (amount/4);
	
	if s < 0.1 then
		s := 0.1;
	if s > 5.0 then
		s := 5.0;
		
	diff := s-scale;
	scale := s;
		
	offsetx := offsetx + -Trunc((sizex-(sizex-mousex))*(diff));
	offsety := offsety + -Trunc((sizey-(sizey-mousey))*(diff));
	
	IupRedraw(ih, 0);
	
	Result := IUP_DEFAULT;
end;

function TScriptEditor.gl_redraw_cb(ih : Ihandle; x, y : Longint) : Longint;
var
	viewport : array[0..3] of GLint;
	select_buf : array[1..64] of GLuint;
	hit_cnt : GLuint;
	
	p1, p2 : TVec2;
begin
	IupGLMakeCurrent(ih);
	
	// select
	glSelectBuffer(64, @select_buf);
	glRenderMode(GL_SELECT);
	glInitNames;
	glPushName(0);
	
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity;
	
	viewport[0] := 0;
	viewport[1] := 0;
	viewport[2] := sizex;
	viewport[3] := sizey;
	gluPickMatrix(mousex, sizey-mousey, 8, 8, @viewport);
	gluOrtho2D(0, sizex*scale, sizey*scale, 0);
	
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity;
	
	glTranslatef(-offsetx, -offsety, 0);
	
	DrawBlocks;
	DrawLinks;
	
	hit_cnt := glRenderMode(GL_RENDER);
	ProcessHits(hit_cnt, select_buf);
	
	// actual rendering
	glClear(GL_COLOR_BUFFER_BIT);
	
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity;
	gluOrtho2D(0, sizex*scale, sizey*scale, 0);
	
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity;
	
	glTranslatef(-offsetx, -offsety, 0);
	
	DrawBlocks;
	DrawLinks;
	
	if start_block <> nil then
	begin
		glColor3f(0.0, 0.7, 0.0);
		
		if start_outpoint <> -1 then
			p1 := GetOutPoint(start_block, start_outpoint)
		else
			p1 := GetInPoint(start_block, start_inpoint);
		
		p2.x := (mousex*scale)+offsetx;
		p2.y := (mousey*scale)+offsety;
		
		glBegin(GL_LINES);
		glVertex2fv(@p1);
		glVertex2fv(@p2);
		glEnd;
	end;
	
	IupGLSwapBuffers(ih);
	
	Result := IUP_DEFAULT;
end;

function EditScript(s : TSection) : Boolean;
var
	editor : TScriptEditor;
	rec_0000 : TSimpleValue;
	count : TSimpleValue;
	
	res : Longint;
begin
	editor := TScriptEditor.Create;
	
	// may in reality be more than one script per object?
	rec_0000 := s.GetParam('rec_0000', 'section');
	if rec_0000 <> nil then
	begin
		editor.LoadScript(TSection(rec_0000));
	end else
	begin
		// set script version according to scene version
		// move to constructor?
		case Scene.GetVersion of
			sceneVerLL:
				if Engine.version = eVerLLBeta15102012 then
					editor.script_version := SCRIPT_VER_LL_BETA_15_10_2012
				else
					editor.script_version := SCRIPT_VER_LL;
			sceneVerRedux:      
				editor.script_version := SCRIPT_VER_REDUX;
			sceneVerArktika1:   
				editor.script_version := SCRIPT_VER_ARKTIKA1;
			sceneVerExodus:
				editor.script_version := SCRIPT_VER_EXODUS;
		end;
	end;
	
	res := editor.ShowModal;
	
	if res = 1 then
	begin
		if editor.Empty then
		begin
			if rec_0000 <> nil then
			begin
				s.items.Remove(rec_0000);
				rec_0000.Free;
				
 				count := s.GetParam('count', 'u32');
				(count as TIntegerValue).num := 0; 			
			end;
		end else
			begin
			if rec_0000 <> nil then
			begin
				TSection(rec_0000).Clear;
				editor.SaveScript(TSection(rec_0000));
			end else
			begin
				rec_0000 := s.AddSect('rec_0000');
				editor.SaveScript(TSection(rec_0000));
				
				count := s.GetParam('count', 'u32');
				(count as TIntegerValue).num := 1;
			end;
		end;
		
		EditScript := True;
	end else
		EditScript := False;
	
	editor.Free;
end;

end.
