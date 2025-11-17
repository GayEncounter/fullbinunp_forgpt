program tree_test;
uses Iup, sysutils;

function tree_button_cb(ih : Ihandle; button, pressed, x, y : Longint; status : PAnsiChar) : Longint; cdecl;
var
  id, value: Longint;
  s : String;
begin
  id := IupConvertXYToPos(ih, x, y);
  s := IntToStr(id);

  if (pressed = 1) and (status[5] = 'D') then
  begin
    //IupMessage('message', PAnsiChar(s));
    value := 100;
    IupGetParam('id', nil, nil, 'Value: %i[0,65535]'#10, @value, nil);
    IupSetAttribute(ih, PAnsiChar('TITLE'+s), PAnsiChar('id = ' + IntToStr(value)));
  end;

  Result := IUP_DEFAULT;
end;

procedure CreateDialog;
var
  dlg : Ihandle;
  tree : Ihandle;
  list : Ihandle;
begin
  tree := IupTree;
  IupSetAttribute(tree, 'RASTERSIZE', '250x350');
  IupSetCallback(tree, 'BUTTON_CB', @tree_button_cb);

	list := IupList(nil);
	IupSetAttributes(list, '1=item1, 2=item2, 3=item3');
	IupSetAttribute(list, 'DROPDOWN', 'YES');
	IupSetAttribute(list, 'EDITBOX', 'YES');

  dlg := IupDialog(IupSetAttributes(IupVBox(tree, list, nil), 'MARGIN=10x10'));
  IupSetAttribute(dlg, 'TITLE', 'Tree Test');
  IupShowXY(dlg, IUP_CURRENT, IUP_CURRENT);

  IupSetAttribute(tree, 'TITLE0', 'ROOT');
  IupSetAttribute(tree, 'ADDLEAF', 'id = 100');
end;

begin
  IupOpen(nil, nil);

  CreateDialog;
  IupMainLoop;

  IupClose;
end.
