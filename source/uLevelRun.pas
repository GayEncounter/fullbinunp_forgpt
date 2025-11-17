unit uLevelRun;

interface
uses uScene;

procedure RunOptions(version : TSceneVersion);
procedure RunGame(version : TSceneVersion; const map_name : String);

implementation 
uses SysUtils, Windows, Inifiles, Iup;

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

function SectionOfGame(version : TSceneVersion) : String;
var
	sect : String;
begin
	case version of
		sceneVer2033:     sect := 'game_2033';
		sceneVerLL:       sect := 'game_ll';
		sceneVerRedux:    sect := 'game_redux';
		sceneVerArktika1: sect := 'game_arktika.1';
		sceneVerExodus:   sect := 'game_exodus';
		else sect := '';
	end;
	SectionOfGame := sect;
end;

procedure RunOptions(version : TSceneVersion);
var
	F : TIniFile;
	sect : String;
	
	exe_path : String;
	working_folder : String;
	arguments : String;
	
	text_exe : Ihandle;
	text_folder : Ihandle;
	text_args : Ihandle;

	box : Ihandle;
	dlg : Ihandle;
begin
	sect := SectionOfGame(version);
	if sect = '' then
		Exit;
		
	F := TIniFile.Create('editor_data\level_editor.ini');
	exe_path       := F.ReadString(sect, 'exe_path', '');
	working_folder := F.ReadString(sect, 'working_folder', '');
	arguments      := F.ReadString(sect, 'arguments', '');
	
	text_exe := IupText(nil);
	IupSetAttribute(text_exe, 'VALUE', PAnsiChar(exe_path));
	
	text_folder := IupText(nil);
	IupSetAttribute(text_folder, 'VALUE', PAnsiChar(working_folder));

	text_args := IupText(nil);
	IupSetAttribute(text_args, 'VALUE', PAnsiChar(arguments));

	box := IupGridBox(
		IupLabel('Exe Path: '), text_exe,
		IupLabel('Working Folder: '), text_folder,
		IupLabel('Arguments: '), text_args,
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
		exe_path := IupGetAttribute(text_exe, 'VALUE');
		working_folder := IupGetAttribute(text_folder, 'VALUE');
		arguments := IupGetAttribute(text_args, 'VALUE');
		
		F.WriteString(sect, 'exe_path', exe_path);
		F.WriteString(sect, 'working_folder', working_folder);
		F.WriteString(sect, 'arguments', arguments);
	end;
	
	IupDestroy(dlg);
	
	F.Free;
end;

procedure RunGame(version : TSceneVersion; const map_name : String);
var
	F : TIniFile;
	sect : String;
	
	exe_path : String;
	working_folder : String;
	arguments : String;
	
	command_line : String;
	
	ret : BOOL;
	si : STARTUPINFO;
	pi : PROCESS_INFORMATION;
begin
	sect := SectionOfGame(version);
	if sect = '' then
		Exit;

	F := TIniFile.Create('editor_data\level_editor.ini');
	exe_path       := F.ReadString(sect, 'exe_path', '');
	working_folder := F.ReadString(sect, 'working_folder', '');
	arguments      := F.ReadString(sect, 'arguments', '');
	F.Free;
	
	if working_folder = '' then
		working_folder := ExtractFileDir(exe_path);
	
	command_line := '"' + exe_path + '" ' + arguments + ' ';
	
	if version = sceneVer2033 then
		command_line := command_line + '-server -map ' + map_name
	else
		command_line := command_line + '-map ' + map_name;
		
	FillChar(si, Sizeof(si), #0);
	FillChar(pi, Sizeof(pi), #0);
	
	si.cb := Sizeof(si);
	
	ret := CreateProcessA(
		PAnsiChar(exe_path),
		PAnsiChar(command_line),
		nil,
		nil,
		FALSE,
		0,
		nil,
		PAnsiChar(working_folder),
		@si,
		@pi
	);
	
	if ret then
	begin
		CloseHandle(pi.hProcess);
		CloseHandle(pi.hThread);
	end;
end;

end.
