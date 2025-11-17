unit uAprilFools;

interface

procedure ShowBlueScreen;

implementation
uses sysutils, Windows, chunkedFile, Iup;

var
	enabled : Boolean = False;
	
function blue_screen_k_any_cb(ih : Ihandle; c : Longint) : Longint; cdecl;
begin
	if c = $FF1B then // K_ESC
		Halt(0);
		
	Result := IUP_CONTINUE;
end;

procedure ShowBlueScreen;
var
	I : Longint;
	r : TMemoryReader;
	data : array of Byte;
	
	dev : DEVMODE;
	
	image : Ihandle;
	lbl : Ihandle;
	dlg : Ihandle;
begin
	if not enabled then
		Exit;

	r := TMemoryReader.CreateFromFile('editor_data\somefile.im24');
	r.pos := $20; // skip SUN rasterfile header
	
	// convert BGR to RGB
	SetLength(data, 1024*768*3);
	for I := 0 to 1024*768 - 1 do
	begin
		data[I*3+2] := r.ReadByte;
		data[I*3+1] := r.ReadByte;
		data[I*3  ] := r.ReadByte;
	end;
	
	r.Free;
	
	// change display resolution to 1024x768
	FillChar(dev, Sizeof(dev), #0);
	dev.dmSize := Sizeof(dev);
	dev.dmPelsWidth := 1024;
	dev.dmPelsHeight := 768;
	dev.dmBitsPerPel := 32;
	dev.dmFields := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL;
	
	ChangeDisplaySettings(@dev, CDS_FULLSCREEN);
	
	// create fullscreen dialog and show it
	image := IupImageRGB(1024, 768, @data[0]);
	
	lbl := IupLabel('BSOD');
	IupSetAttributeHandle(lbl, 'IMAGE', image);
	IupSetAttribute(lbl, 'CURSOR', 'NONE');
	
	dlg := IupDialog(lbl);
	IupSetAttribute(dlg, 'FULLSCREEN', 'YES');
	IupSetAttribute(dlg, 'TOPMOST', 'YES');
	IupSetCallback(dlg, 'K_ANY', @blue_screen_k_any_cb);
	
	IupPopup(dlg, 0, 0);
	
	IupDestroy(dlg);
	
	Halt(0);
end;

initialization

if LowerCase(ExtractFileName(ParamStr(0))) <> 'cartoder.exe' then
	enabled := True;

end.
