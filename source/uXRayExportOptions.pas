unit uXRayExportOptions;

interface

function XRayExportOptions(var map_name, objects_path, maps_path : String; var use_ltx : Boolean; var use_lwo : Boolean) : Boolean;

implementation
uses Iup;

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

function XRayExportOptions(var map_name, objects_path, maps_path : String; var use_ltx : Boolean; var use_lwo : Boolean) : Boolean;
var
	text_name : Ihandle;
	list_format : Ihandle;
	list_obj_format : Ihandle;

	text_objects : Ihandle;
	text_maps : Ihandle;

	box : Ihandle;
	
	dlg : Ihandle;
begin
	text_name := IupText(nil);
	IupSetAttribute(text_name, 'VALUE', PAnsiChar(map_name));
	
	list_format := IupList(nil);
	IupSetAttribute(list_format, 'DROPDOWN', 'YES');
	IupSetAttribute(list_format, '1', 'LTX');
	IupSetAttribute(list_format, '2', 'Binary');
	
	list_obj_format := IupList(nil);
	IupSetAttribute(list_obj_format, 'DROPDOWN', 'YES');
	IupSetAttribute(list_obj_format, '1', 'LWO');
	IupSetAttribute(list_obj_format, '2', '.object');
	
	if use_ltx then
		IupSetInt(list_format, 'VALUE', 1)
	else
		IupSetInt(list_format, 'VALUE', 2);
		
	if use_lwo then
		IupSetInt(list_obj_format, 'VALUE', 1)
	else
		IupSetInt(list_obj_format, 'VALUE', 2);
	
	text_objects := IupText(nil);
	IupSetAttribute(text_objects, 'VALUE', PAnsiChar(objects_path));

	text_maps := IupText(nil);
	IupSetAttribute(text_maps, 'VALUE', PAnsiChar(maps_path));

	box := IupGridBox(
		IupLabel('Map name: '), text_name,
		IupLabel('Map format: '), list_format,
		IupLabel('Object format: '), list_obj_format,
		IupLabel('Objects path: '), text_objects,
		IupLabel('Maps path: '), text_maps,
		IupHBox(iup.Button('Export', @btn_export_cb), iup.Button('Cancel', @btn_cancel_cb), nil),
		nil
	);
	IupSetAttribute(box, 'GAPLIN', '8');
	IupSetAttribute(box, 'MARGIN', '5x5');
	IupSetAttribute(box, 'EXPANDCHILDREN', 'HORIZONTAL');
	IupSetAttribute(box, 'NUMDIV', '2');

	dlg := IupDialog(box);
	IupSetAttribute(dlg, 'TITLE', 'Options');
	IupSetAttribute(dlg, 'SIZE', '400x');
	
	IupPopup(dlg, IUP_CENTER, IUP_CENTER);
	
	Result := IupGetInt(dlg, 'VALUE') = 1;
	if Result then
	begin
		map_name := IupGetAttribute(text_name, 'VALUE');
		objects_path := IupGetAttribute(text_objects, 'VALUE');
		maps_path := IupGetAttribute(text_maps, 'VALUE');
		use_ltx := IupGetInt(list_format, 'VALUE') = 1;
		use_lwo := IupGetInt(list_obj_format, 'VALUE') = 1;
		
		if objects_path[Length(objects_path)] <> '\' then
			objects_path := objects_path + '\';
		if maps_path[Length(maps_path)] <> '\' then
			maps_path := maps_path + '\';
	end;
	
	IupDestroy(dlg);
end;

end.