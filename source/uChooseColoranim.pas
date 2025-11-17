unit uChooseColoranim;

interface

function ChooseColoranim(var ca : String) : Boolean;

implementation
uses Windows, sysutils, classes, Iup, vmath, framework, Konfig, Engine, KonfigLibrary;

// ---- Coloranim
type
	TCAKey = record
		color : TVec4;
		time : Longint;
	end;

type 
	TChooseColoranim = class
		ca_library : TTextKonfig;
		dlg : Ihandle;
		timer : Ihandle;
		
		anim_name : String;
		anim_length : Longword;
		anim_keys : array of TCAKey;
	
		constructor Create;
		destructor Destroy; override;
		
		function Execute(var ca : String) : Boolean;
		
		private
		procedure LoadColoranim(const name : String);
		procedure Update;
	end;

function list_anims_map_cb(ih : Ihandle) : Longint; cdecl;
var
	dialog : TChooseColoranim;
begin
	dialog := TChooseColoranim(IupGetAttribute(IupGetDialog(ih), 'TChooseColoranim->this'));
	iup.SetStrAttribute(ih, 'VALUESTRING', dialog.anim_name);
	dialog.LoadColoranim(dialog.anim_name);
	Result := IUP_DEFAULT;
end;
	
function list_anims_cb(ih : Ihandle; _text : PAnsiChar; item, state : Longint) : Longint; cdecl;
var
	dialog : TChooseColoranim;
begin
	dialog := TChooseColoranim(IupGetAttribute(IupGetDialog(ih), 'TChooseColoranim->this'));
	if state = 1 then
		dialog.LoadColoranim(_text);
	Result := IUP_DEFAULT;
end;

function timer_cb(ih : Ihandle) : Longint; cdecl;
var
	dialog : TChooseColoranim;
begin
	dialog := TChooseColoranim(IupGetAttribute(ih, 'TChooseColoranim->this'));
	dialog.Update();
	Result := IUP_DEFAULT;
end;

function btn_ok_cb(ih : Ihandle) : Longint; cdecl;
var
	dialog : TChooseColoranim;
begin
	dialog := TChooseColoranim(IupGetAttribute(ih, 'TChooseColoranim->this'));
	iup.SetStrAttribute(dialog.dlg, 'VALUE', dialog.anim_name);
	Result := IUP_CLOSE;
end;

function btn_cancel_cb(ih : Ihandle) : Longint; cdecl;
var
	dialog : TChooseColoranim;
begin
	dialog := TChooseColoranim(IupGetAttribute(ih, 'TChooseColoranim->this'));
	IupSetAttribute(dialog.dlg, 'VALUE', '');
	Result := IUP_CLOSE;
end;
	
constructor TChooseColoranim.Create;
var
	I, J : Longint;
	K : TKonfig;
	js : TFramework;
	arr : TSection;
	elem : TSimpleValue;
	
	label_length : Ihandle;
	canvas : Ihandle;
	
	list_anims : Ihandle;
	btn_ok : Ihandle;
	btn_cancel : Ihandle;
	
	box1, box2, box3 : Ihandle;
begin
	inherited;
	
	K := KonfigLibrary.GetKonfig('color_anim_lib');
	if K <> nil then
	begin
		js := TFramework.Create;	
		if Engine.version >= eVerLLBeta15102012 then
			ca_library := js.DecompileKonfig(K, 'js\ll\color_anim_lib.js')
		else
			ca_library := js.DecompileKonfig(K, 'js\2033\color_anim_lib.js');
		js.Free;
		
		K.Free;
	end;

	list_anims := IupList(nil);
	IupSetAttribute(list_anims, 'EXPAND', 'YES');
	IupSetAttribute(list_anims, 'SORT', 'YES');
	IupSetAttribute(list_anims, 'VISIBLELINES', '45');
	IupSetAttribute(list_anims, 'VISIBLECOLUMNS', '70');
	IupSetCallback(list_anims, 'ACTION', @list_anims_cb);
	IupSetCallback(list_anims, 'MAP_CB', @list_anims_map_cb);
	
	I := 1;
	if Assigned(ca_library) then
	begin
		arr := ca_library.root.GetSect('color_anims');
		for J := 0 to arr.ParamCount-1 do
		begin
			elem := arr.GetParam(J);
			if elem is TSection then
			begin
				iup.SetStrAttribute(list_anims, IntToStr(I), elem.name);
				Inc(I);
			end;
		end;
	end;
	
	iup.SetStrAttribute(list_anims, IntToStr(I), '<none>');

	btn_ok := iup.Button('OK', @btn_ok_cb);
	btn_cancel := iup.Button('Cancel', @btn_cancel_cb);
	
	label_length := IupLabel('Length: 0.0 sec, 0 keys');
	IupSetAttribute(label_length, 'NAME', 'LABEL_LENGTH');
	IupSetAttribute(label_length, 'EXPAND', 'HORIZONTAL');
	
	canvas := IupCanvas(nil);
	IupSetAttribute(canvas, 'NAME', 'CANVAS_COLOR');
	
	box3 := IupHBox(btn_ok, btn_cancel, nil);
	box2 := IupVBox(list_anims, box3, nil);
	box1 := IupHBox(box2, IupVBox(label_length, canvas, nil), nil);
	
	IupSetAttribute(box1, 'MARGIN', '5x5');
	
	IupSetAttribute(box3, 'MARGIN', '1x1');
	IupSetAttribute(box3, 'PADDING', '8x3');
	IupSetAttribute(box3, 'GAP', '8x3');
	
	dlg := IupDialog(box1);
	IupSetAttribute(dlg, 'TITLE', 'Select coloranim');
	IupSetAttribute(dlg, 'TChooseColoranim->this', Pointer(self));
	
	timer := IupTimer();
	IupSetAttribute(timer, 'TIME', '11');
	IupSetCallback(timer, 'ACTION_CB', @timer_cb);
	IupSetAttribute(timer, 'TChooseColoranim->this', Pointer(self));
end;

destructor TChooseColoranim.Destroy;
begin
	IupDestroy(timer);
	IupDestroy(dlg);
	ca_library.Free;
	inherited;
end;

function TChooseColoranim.Execute(var ca : String) : Boolean;
var
	value : PAnsiChar;
begin
	anim_name := ca;

	IupPopup(dlg, IUP_CURRENT, IUP_CURRENT);
	
	value := IupGetAttribute(dlg, 'VALUE');
	if (value <> nil) and (value <> '') then
	begin
		if value = '<none>' then
			ca := ''
		else
			ca := value;
			
		Result := True;
	end else
		Result := False;
end;

procedure TChooseColoranim.LoadColoranim(const name : String);
var
	anim : TSection;
	keys, key : TSection;
	I, J : Longint;
	
	anim_length : Longword;
	anim_keys : array of TCAKey;
begin
	// unload animation
	self.anim_length := 0;
	SetLength(self.anim_keys, 0);
		
	IupSetAttribute(timer, 'RUN', 'NO');
	
	// load animation	
	anim_name := name;
	
	if anim_name <> '<none>' then
		anim := ca_library.root.GetSect('color_anims').GetSect(anim_name, False)
	else
		anim := nil;

	if anim <> nil then
	begin
		try
			J := 0;
			anim_length := anim.GetInt('length', 'u32');
			keys := anim.GetSect('keys');
			for I := 0 to keys.ParamCount-1 do
				if keys.GetParam(I) is TSection then
				begin
					SetLength(anim_keys, J+1);
					
					key := TSection(keys.GetParam(I));
					anim_keys[J].color := key.GetVec4('value', 'color, vec4f');
					anim_keys[J].time := key.GetInt('time', 's32');
					Inc(J);
				end;
		
			self.anim_length := anim_length;
			self.anim_keys := anim_keys;
				
			IupSetAttribute(timer, 'RUN', 'YES');
		except
			on E: Exception do
				IupMessageError(dlg, 
					PAnsiChar(
						'Error loading color animation ''' + anim_name + ''''#10 +
						E.ClassName + ': ' + E.Message
					)
				);
		end;
	end;
	
	iup.SetStrAttribute(IupGetDialogChild(dlg, 'LABEL_LENGTH'), 'TITLE', 
	'Length: ' + FloatToStr(anim_length / 1000) + ' sec, ' + IntToStr(Length(anim_keys)) + ' keys.'); 
end;

procedure TChooseColoranim.Update;
var
	time, I : Longint;
	t1, t2 : Longint;
	canvas : Ihandle;
	
	color : TVec4;
	scale : Single;
	color_str : String;
begin
	canvas := IupGetDialogChild(dlg, 'CANVAS_COLOR');
	
	if anim_length > 0 then
	begin
		time := GetTickCount64() mod anim_length;
		
		I := Length(anim_keys);
	
		if time < anim_keys[0].time then
			color := anim_keys[0].color
		else if time >= anim_keys[I-1].time then
			color := anim_keys[I-1].color
		else begin
			I := 1;
			while anim_keys[I].time < time do
				Inc(I);
				
			t1 := anim_keys[I-1].time;
			t2 := anim_keys[I].time;
			scale := (time - t1) / (t2 - t1);
			//WriteLn('time = ', time, ' t1 = ', t1, ' t2 = ', t2, ' scale = ', scale:1:3);
			color.x := anim_keys[I-1].color.x * (1.0-scale) + anim_keys[I].color.x * scale;
			color.y := anim_keys[I-1].color.y * (1.0-scale) + anim_keys[I].color.y * scale;
			color.z := anim_keys[I-1].color.z * (1.0-scale) + anim_keys[I].color.z * scale;
			color.w := anim_keys[I-1].color.w * (1.0-scale) + anim_keys[I].color.w * scale;
		end;
		
		// mul by alpha		
		//color.x := color.x * color.w;
		//color.y := color.y * color.w;
		//color.z := color.z * color.w;
				
		color_str := 
			IntToStr(Trunc(color.x * 255)) + ' ' + 
			IntToStr(Trunc(color.y * 255)) + ' ' +
			IntToStr(Trunc(color.z * 255)) + ' 255';
	end else
		color_str := '255 255 255 255';
	
	//WriteLn('set color ', color_str);
		
	IupSetStrAttribute(canvas, 'BGCOLOR', PAnsiChar(color_str));
	IupRedraw(canvas, 0);
end;

function ChooseColoranim(var ca : String) : Boolean;
var
	dialog : TChooseColoranim;
begin
	dialog := TChooseColoranim.Create;
	Result := dialog.Execute(ca);
	dialog.Free;
end;

end.
