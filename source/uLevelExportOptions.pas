unit uLevelExportOptions;

interface

function LevelExportOptions(var filename : String; var selectionOnly, exportSuperstatic, exportLights : Boolean) : Boolean;

implementation
uses sysutils, Iup;

function btn_export_cb(ih : Ihandle) : Longint; cdecl;
begin
	IupSetInt(IupGetDialog(ih), 'VALUE', 1);
	Result := IUP_CLOSE;
end;

function btn_cancel_cb(ih : Ihandle) : Longint; cdecl;
begin
	IupSetInt(IupGetDialog(ih), 'VALUE', 0);
	Result := IUP_CLOSE;
end;

function FileDlg(var filename : String) : Boolean;
var
	dlg : Ihandle;
	fn : String;
	ext : String[16];
begin
	dlg := IupFileDlg;
	IupSetAttribute(dlg, 'DIALOGTYPE', 'SAVE');
	IupSetAttribute(dlg, 'EXTFILTER', 'FBX|*.fbx|3DS|*.3ds|OBJ|*.obj|');

	IupPopup(dlg, IUP_CENTER, IUP_CENTER);

	if IupGetInt(dlg, 'STATUS') <> -1 then
	begin
		fn := IupGetAttribute(dlg, 'VALUE');

		case IupGetInt(dlg, 'FILTERUSED') of
			1: ext := 'fbx';
			2: ext := '3ds';
			3: ext := 'obj';
		end;
		
		if ExtractFileExt(fn) = '' then
			fn := fn + '.' + ext;
		
		filename := fn;
		Result := True;
	end else
		Result := False;
		
	IupDestroy(dlg);
end;

function LevelExportOptions(var filename : String; var selectionOnly, exportSuperstatic, exportLights : Boolean) : Boolean;
var
	tg_selonly : Ihandle;
	tg_superstatic : Ihandle;
	tg_lights : Ihandle;

	box : Ihandle;
	dlg : Ihandle;
	
	value : Longint;
begin
	tg_selonly := iup.Toggle('Selection only', nil, selectionOnly);
	tg_superstatic := iup.Toggle('Export superstatic', nil, exportSuperstatic);
	tg_lights := iup.Toggle('Export lights', nil, exportLights);

	box := IupVBox(
		tg_selonly,
		tg_superstatic,
		tg_lights,
		IupHBox(iup.Button('Export', @btn_export_cb), iup.Button('Cancel', @btn_cancel_cb), nil),
		nil
	);
	IupSetAttribute(box, 'GAPLIN', '8');
	IupSetAttribute(box, 'MARGIN', '5x5');
	IupSetAttribute(box, 'EXPANDCHILDREN', 'HORIZONTAL');

	dlg := IupDialog(box);
	IupSetAttribute(dlg, 'TITLE', 'Options');
	IupSetAttribute(dlg, 'SIZE', '400x');
	
	IupPopup(dlg, IUP_CENTER, IUP_CENTER);
	
	value := IupGetInt(dlg, 'VALUE');
	if (value = 1) and FileDlg(filename) then
	begin
		selectionOnly := IupGetInt(tg_selonly, 'VALUE') <> 0;
		exportSuperstatic := IupGetInt(tg_superstatic, 'VALUE') <> 0;
		exportLights := IupGetInt(tg_lights, 'VALUE') <> 0;
		Result := True;
	end else
		Result := False;
	
	IupDestroy(dlg);
end;

end.
