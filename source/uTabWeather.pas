unit uTabWeather;

interface
uses Iup;

function CreateTab : Ihandle;
procedure ResetWeather;
procedure FillList;

implementation
uses classes, sysutils, Konfig, uScene, uWeather, uEditorUtils, Timer;

procedure Redisplay;
var
	gl : Ihandle;
begin
	gl := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'GL_CANVAS');
	IupRedraw(gl, 0);
end;

procedure ResetWeather;
var	
	startup : TSection;
begin
	if Scene.konf <> nil then
	begin
		startup := Scene.konf.root.GetSect('startup', False);
		if startup <> nil then
			uWeather.SetWeather(startup.GetStrDef('desc_0', ''));
	end;
	
	Redisplay;
end;

procedure FillList;
var
	tree : Ihandle;
	weathers : TStringList;
begin
	tree := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'TREE_WEATHERS');

	IupSetAttribute(tree, 'DELNODE', 'CHILDREN0');
	IupSetAttribute(tree, 'AUTOREDRAW', 'NO');

	weathers := GetWeathersList;
	weathers.Sort;
	
	FillTreeFromList(tree, 0, weathers);
			
	weathers.Free;
	
	IupSetAttribute(tree, 'AUTOREDRAW', 'YES');
	IupSetAttribute(tree, 'TITLE0', 'Weathers');
	IupSetAttribute(tree, 'STATE0', 'EXPANDED');
end;

function tree_weathers_cb(ih : Ihandle; id, state : Longint) : Longint; cdecl;
begin
	if (state = 1) and (KindOfTreeNode(ih, id) = 'LEAF') then
		SetWeather(PathOfTreeNode(ih, id));
	Redisplay;
	Result := IUP_DEFAULT;
end;

function btn_apply_cb(ih : Ihandle) : Longint; cdecl;
var	
	startup : TSection;
	desc_0, desc_1 : TStringValue;
	tree : Ihandle;
	selnode : Longint;
begin
	tree := IupGetDialogChild(ih, 'TREE_WEATHERS');
	selnode := IupGetInt(tree, 'VALUE');

	if (Scene.konf <> nil) and (selnode > 0) and (KindOfTreeNode(tree, selnode) = 'LEAF') then
	begin
		try
			startup := Scene.konf.root.GetSect('startup');
			desc_0 := startup.GetParam('desc_0', 'stringz') as TStringValue;
			desc_1 := startup.GetParam('desc_1', 'stringz') as TStringValue;
			
			desc_0.str := PathOfTreeNode(tree, selnode);
			desc_1.str := PathOfTreeNode(tree, selnode);
		except
			on E: Exception do
				ShowError(E.ClassName + ': ' + E.Message);
		end;
	end;
	Result := IUP_DEFAULT;
end;

function btn_reset_cb(ih : Ihandle) : Longint; cdecl;
begin
	ResetWeather;
	Result := IUP_DEFAULT;
end;

function CreateTab : Ihandle;
var
	tree_weathers : Ihandle;
	btn_apply : Ihandle;
	btn_reset : Ihandle;
begin
	// Weather tab
	tree_weathers := IupTree;
	IupSetAttribute(tree_weathers, 'NAME', 'TREE_WEATHERS');
	IupSetAttribute(tree_weathers, 'EXPAND', 'YES');
	IupSetAttribute(tree_weathers, 'RASTERSIZE', '250x350');
	IupSetCallback(tree_weathers, 'SELECTION_CB', @tree_weathers_cb);
	
	btn_apply := iup.Button('Apply!', @btn_apply_cb);
	btn_Reset := iup.Button('Reset', @btn_reset_cb);
	
	Result := IupVBox(IupHBox(btn_apply, btn_reset, nil), tree_weathers, nil)
end;

end.
