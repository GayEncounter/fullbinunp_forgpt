program float_decoder;
uses sysutils, classes, Iup;

function raw_bytes_changed_cb(ih : Ihandle) : Longint; cdecl;
var
	sl : TStringList;
	data : array[0..3] of Byte;
	float : Single absolute data;
	number : Ihandle;
begin
	sl := TStringList.Create;
	sl.Delimiter := ' ';
	sl.DelimitedText := IupGetAttribute(ih, 'VALUE');
	
	if sl.Count = 4 then
	begin
		data[0] := StrToInt('$'+sl[0]);
		data[1] := StrToInt('$'+sl[1]);
		data[2] := StrToInt('$'+sl[2]);
		data[3] := StrToInt('$'+sl[3]);
		
		number := IupGetDialogChild(ih, 'TEXT_NUMBER');
		IupSetAttribute(number, 'VALUE', PAnsiChar(FloatToStr(float)));
	end;
	
	sl.Free;
	
	Result := IUP_DEFAULT;
end;

function number_changed_cb(ih : Ihandle) : Longint; cdecl;
var
	N : Single;
	data : array[0..3] of Byte;
	raw_bytes : Ihandle;
	str : String;
begin
	try 
		N := StrToFloat(IupGetAttribute(ih, 'VALUE'));
	except
		on E: Exception do
			N := 0.0;
	end;
	
	Move(N, data, 4);
	
	str := 
		IntToHex(data[0], 2) + ' ' +
		IntToHex(data[1], 2) + ' ' +
		IntToHex(data[2], 2) + ' ' +
		IntToHex(data[3], 2);
		
	raw_bytes := IupGetDialogChild(ih, 'TEXT_RAWBYTES');
	IupSetAttribute(raw_bytes, 'VALUE', PAnsiChar(str));
	
	Result := IUP_DEFAULT;
end;

procedure CreateDialog;
var
	raw_bytes : Ihandle;
	number : Ihandle;
	box : Ihandle;

	dlg : Ihandle;
begin
	raw_bytes := IupText(nil);
	IupSetAttribute(raw_bytes, 'NAME', 'TEXT_RAWBYTES');
	IupSetAttribute(raw_bytes, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(raw_bytes, 'VALUE', '00 00 00 00');
	IupSetCallback(raw_bytes, 'VALUECHANGED_CB', @raw_bytes_changed_cb);

	number := IupText(nil);
	IupSetAttribute(number, 'NAME', 'TEXT_NUMBER');
	IupSetAttribute(number, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(number, 'VALUE', PAnsiChar(FloatToStr(0.0)));
	IupSetCallback(number, 'VALUECHANGED_CB', @number_changed_cb);

	box := IupVBox(raw_bytes, number, nil);
	IupSetAttribute(box, 'MARGIN', '10x10');
	IupSetAttribute(box, 'GAP', '5x5');

	dlg := IupDialog(box);
	IupSetHandle('MAINDIALOG', dlg);
	IupSetAttribute(dlg, 'TITLE', 'Float Decoder');
	IupShowXY(dlg, 500, 500);
end;

begin
	IupOpen(nil, nil);

	CreateDialog;
	IupMainLoop();

	IupClose;
end.
