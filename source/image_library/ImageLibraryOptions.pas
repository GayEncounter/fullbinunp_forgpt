unit ImageLibraryOptions;

interface

var
	ImportDirectory : String = 'import';
	PatchDirectory : String = 'F:\Dev-Cpp\projects\mllp\content';
	CrunchTextureQuality : Longint = 255;
	
procedure ShowOptions;

implementation
uses Windows, Engine, Inifiles, Iup;

var
	F : TIniFile;
	
	ResourcesPathNew : String;
	ResourcesPathChanged : Boolean;
	
function btn_apply_cb(ih : Ihandle) : Longint; cdecl;
begin
	IupSetInt(IupGetDialog(ih), 'VALUE', 1);
	Result := IUP_CLOSE;
end;

function btn_cancel_cb(ih : Ihandle) : Longint; cdecl;
begin
	IupSetInt(IupGetDialog(ih), 'VALUE', 0);
	Result := IUP_CLOSE;
end;
	
procedure ShowOptions;
var
	text_resources : Ihandle;
	text_patch : Ihandle;
	text_import : Ihandle;
	
	label_quality : Ihandle;
	val_quality : Ihandle;

	box : Ihandle;
	
	dlg : Ihandle;
begin
	text_resources := IupText(nil);
	IupSetAttribute(text_resources, 'VALUE', PAnsiChar(ResourcesPath));
	
	text_patch := IupText(nil);
	IupSetAttribute(text_patch, 'VALUE', PAnsiChar(PatchDirectory));

	text_import := IupText(nil);
	IupSetAttribute(text_import, 'VALUE', PAnsiChar(ImportDirectory));

	label_quality := IupLabel('Crunch Quality: ');
	IupSetAttribute(label_quality, 'TIP', 'Affects .512c, .1024c, .2048c files');

	val_quality := IupVal('HORIZONTAL');
	IupSetAttribute(val_quality, 'MIN', '0');
	IupSetAttribute(val_quality, 'MAX', '255');
	IupSetAttribute(val_quality, 'STEP', '1');
	IupSetInt(val_quality, 'VALUE', CrunchTextureQuality);

	box := IupGridBox(
		IupLabel('Resources directory: '), text_resources,
		IupLabel('Patch directory: '), text_patch,
		IupLabel('Import directory: '), text_import,
		label_quality, val_quality,
		IupHBox(iup.Button('Apply', @btn_apply_cb), iup.Button('Cancel', @btn_cancel_cb), nil),
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
	
	if IupGetInt(dlg, 'VALUE') = 1 then
	begin
		ResourcesPathNew := IupGetAttribute(text_resources, 'VALUE');
		PatchDirectory := IupGetAttribute(text_patch, 'VALUE');
		ImportDirectory := IupGetAttribute(text_import, 'VALUE');
		CrunchTextureQuality := IupGetInt(val_quality, 'VALUE');
		
		if ResourcesPath[Length(ResourcesPath)] = '\' then
			SetLength(ResourcesPath, Length(ResourcesPath)-1);
		if PatchDirectory[Length(PatchDirectory)] = '\' then
			SetLength(PatchDirectory, Length(PatchDirectory)-1);
		if ImportDirectory[Length(ImportDirectory)] = '\' then
			SetLength(ImportDirectory, Length(ImportDirectory)-1);
			
		if ResourcesPathNew <> ResourcesPath then
		begin
			ResourcesPathChanged := True;
			
			IupMessage('Message', 'Path to resources changed, please restart!');
			PostMessage(
				HWND(IupGetAttribute(IupGetHandle('MAINDIALOG'), 'HWND')),
				WM_CLOSE,
				0, 0
			);
		end;
	end;
	
	IupDestroy(dlg);
end;
	
initialization
	
F := TIniFile.Create('editor_data\image_library.ini');

ResourcesPath        := F.ReadString('image_library', 'resources_directory', ResourcesPath);
ImportDirectory      := F.ReadString('image_library', 'import_directory', ImportDirectory);
PatchDirectory       := F.ReadString('image_library', 'patch_directory', PatchDirectory);
CrunchTextureQuality := F.ReadInteger('image_library', 'crunch_texture_quality', 255);

F.Free;

finalization

F := TIniFile.Create('editor_data\image_library.ini');

if ResourcesPathChanged then
	F.WriteString('image_library', 'resources_directory', ResourcesPathNew)
else
	F.WriteString('image_library', 'resources_directory', ResourcesPath);
	
F.WriteString('image_library', 'import_directory', ImportDirectory);
F.WriteString('image_library', 'patch_directory', PatchDirectory);
F.WriteInteger('image_library', 'crunch_texture_quality', CrunchTextureQuality);

F.Free;

end.