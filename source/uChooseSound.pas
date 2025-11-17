unit uChooseSound;

interface

function ChooseSound(var filename : String) : Boolean;

implementation
uses
	Windows,                     // GDI
	SysUtils,                    // Working with paths and FileExists
	Engine,                      // ResoucesPath 
	Iup,                         // UI
	ActiveX, WindowsMediaPlayer, // Playing OGGs
	uLEOptions;

// ---- Sound
var
	g_WMP : IWMPPlayer4;
	g_WMPMedia : IWMPMedia;
	
function SUCCEDDED(hr : HRESULT) : Boolean;
begin
	SUCCEDDED := not FAILED(hr);
end;

procedure draw_volume(dlg : Ihandle);
var
	_hdc : HDC;
	r : RECT;
begin
	_hdc := HDC(IupGetAttribute(dlg, 'PREVIEWDC'));
	
	TextOut(_hdc, 10, 10, '0%', 2);
	
	r.left := 28; r.top := 8; r.right := 232; r.bottom := 27;
	FillRect(_hdc, r, COLOR_BTNTEXT+1);
	
	r.left := 29; r.top := 9; r.right := 231; r.bottom := 26;
	FillRect(_hdc, r, COLOR_BTNFACE+1);
	
	r.left := 30; r.top := 10; r.right := 30+uLEOptions.choose_sound_volume*2; r.bottom := 25;
	FillRect(_hdc, r, COLOR_BTNTEXT+1);
	
	TextOut(_hdc, 235, 10, '100% volume', 11);
end;	

function choose_sound_file_cb(dlg : Ihandle; file_name, status : PAnsiChar) : Longint; cdecl;
var
	hr : HRESULT;
	controls : IWMPControls;
	settings : IWMPSettings;
begin
	if status = 'INIT' then
	begin
		hr := CoCreateInstance(
			CLSID_WindowsMediaPlayer,
			nil,
			CLSCTX_INPROC_SERVER,
			IWMPPlayer4,
			g_WMP
		);
		
		if FAILED(hr) then
		begin
			WriteLn('CoCreateInstance failed for CLSID_WindowsMediaPlayer');
			g_WMP := nil;
		end;
		
		if g_WMP <> nil then
		begin
			hr := g_WMP.get_settings(settings);
			if SUCCEDDED(hr) then
			begin
				hr := settings.put_volume(uLEOptions.choose_sound_volume);
				if FAILED(hr) then
					WriteLn('IWMPSettings.put_volume failed!');
			end else
				WriteLn('IWMPPlayer4.get_settings failed!');
		end;
	end;

	if status = 'FINISH' then
	begin
		g_WMPMedia := nil;
		g_WMP := nil;
	end;
	
	if status = 'SELECT' then
	begin
		hr := g_WMP.newMedia(file_name, g_WMPMedia);
		if SUCCEDDED(hr) then
		begin
			hr := g_WMP.put_currentMedia(g_WMPMedia);
			if SUCCEDDED(hr) then
			begin
				hr := g_WMP.get_controls(controls);
				if SUCCEDDED(hr) then
				begin	
					controls.play;
				end else
					WriteLn('IWMPPlayer4.get_controls failed!');
			end else
				WriteLn('IWMPPlayer4.put_currentMedia failed for ''' + file_name + '''!');
		end else
			WriteLn('IWMPPlayer4.newMedia failed for ''' + file_name + '''!');	
	end;
	
	if status = 'PAINT' then
	begin
		draw_volume(dlg);
	end;
	
	Result := IUP_DEFAULT;
end;

function choose_sound_button_cb(ih : Ihandle; button, pressed, x, y : Longint; status : PAnsiChar) : Longint; cdecl;
var
	_hwnd : HWND;
	vol : Longint;
	
	hr : HRESULT;
	settings : IWMPSettings;
begin
	if (button = Ord('1')) and (pressed = 1) then
	begin
		if (x >= 30) and (x <= 230) and (y >= 10) and (y <= 25) then
		begin
			vol := (x - 30) div 2;
			if vol < 0 then vol := 0;
			if vol > 100 then vol := 100;
			uLEOptions.choose_sound_volume := vol;
			
			_hwnd := HWND(IupGetAttribute(ih, 'HWND'));
			RedrawWindow(_hwnd, nil, 0, RDW_INVALIDATE);
			
			IupSetInt(ih, 'bDragVolume', 1);
			
			if g_WMP <> nil then
			begin
				hr := g_WMP.get_settings(settings);
				if SUCCEDDED(hr) then
				begin
					hr := settings.put_volume(vol);
					if FAILED(hr) then
						WriteLn('IWMPSettings.put_volume failed!');
				end else
					WriteLn('IWMPPlayer4.get_settings failed!');
			end;
		end;
	end;
	
	if (button = Ord('1')) and (pressed = 0) then
	begin
		IupSetInt(ih, 'bDragVolume', 0);
	end;
	
	Result := IUP_DEFAULT;
end;

function choose_sound_motion_cb(ih : Ihandle; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
var
	_hwnd : HWND;
	vol : Longint;
	
	hr : HRESULT;
	settings : IWMPSettings;
begin
	if iup_isbutton1(status) then
	begin
		if IupGetInt(ih, 'bDragVolume') > 0 then
		begin
			vol := (x - 30) div 2;
			if vol < 0 then vol := 0;
			if vol > 100 then vol := 100;
			uLEOptions.choose_sound_volume := vol;
			
			_hwnd := HWND(IupGetAttribute(ih, 'HWND'));
			RedrawWindow(_hwnd, nil, 0, RDW_INVALIDATE);
			
			if g_WMP <> nil then
			begin
				hr := g_WMP.get_settings(settings);
				if SUCCEDDED(hr) then
				begin
					hr := settings.put_volume(vol);
					if FAILED(hr) then
						WriteLn('IWMPSettings.put_volume failed!');
				end else
					WriteLn('IWMPPlayer4.get_settings failed!');
			end;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function ChooseSound(var filename : String) : Boolean;
var
	sounds_dir : String;
	
	dlg : Ihandle;
	fn : String;

	I : Integer;
begin
	sounds_dir := ExpandFileName(ResourcesPath + '\sounds\');

	fn := sounds_dir + filename + '.vba';
	if not FileExists(fn) then
		fn := sounds_dir + filename + '.ogg';

	dlg := IupFileDlg;
	IupSetAttribute(dlg, 'DIALOGTYPE', 'OPEN');
	iup.SetStrAttribute(dlg, 'FILE', fn);
	IupSetAttribute(dlg, 'EXTFILTER', 'Sound files (*.ogg, *.vba)|*.ogg;*.vba|All files (*.*)|*.*');
	IupSetCallback(dlg, 'FILE_CB', @choose_sound_file_cb);
	
	IupSetAttribute(dlg, 'SHOWPREVIEW', 'YES');
	IupSetCallback(dlg, 'BUTTON_CB', @choose_sound_button_cb);
	IupSetCallback(dlg, 'MOTION_CB', @choose_sound_motion_cb);

	IupPopup(dlg, IUP_CURRENT, IUP_CURRENT);

	if IupGetInt(dlg, 'STATUS') = 0 then
	begin
		fn := IupGetAttribute(dlg, 'VALUE');
		I := Pos(LowerCase(sounds_dir), LowerCase(fn));
		if I > 0 then
		begin
			// cut path to meshes folder from file name
			fn := ExtractRelativePath(sounds_dir, fn);
			// cut extension
			fn := ChangeFileExt(fn, '');
			
			filename := fn;
			Result := True;
		end else
			Result := False;  
	end else
		Result := False;
	
	IupDestroy(dlg);
end;

end.
