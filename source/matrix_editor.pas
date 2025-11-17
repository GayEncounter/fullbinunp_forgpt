unit matrix_editor;

interface
uses vmath;

function EditMatrix(var m : TMatrix; show_scale : Boolean) : Boolean;

implementation
uses Sysutils, Math, Iup;

//
//	IC	void	getHPB	(T& h, T& p, T& b) const
//	{
//        T cy = _sqrt(j.y*j.y + i.y*i.y);
//        if (cy > 16.0f*type_epsilon(T)) {
//            h = (T) -atan2(k.x, k.z);
//            p = (T) -atan2(-k.y, cy);
//            b = (T) -atan2(i.y, j.y);
//        } else {
//            h = (T) -atan2(-i.z, i.x);
//            p = (T) -atan2(-k.y, cy);
//            b = 0;
//        }
//    }


procedure GetHPB(const m : TMatrix; out h, p, b : Single);
var
	cy : Single;
begin
	cy := Sqrt(m[1,2]*m[1,2] + m[2,2]*m[2,2]);
	if cy > 0.00001 then
	begin
		h := -arctan2(m[3,1], m[3,3]);
		p := -arctan2(-m[3,2], cy);
		b := -arctan2(m[1,2], m[2,2]);
	end else
	begin
		h := -arctan2(-m[1,3], m[1,1]);
		p := -arctan2(-m[3,2], cy);
		b := 0.0;
	end;
end;

procedure GetScale(const m : TMatrix; out x, y, z : Single);
begin
	x := Sqrt(m[1,1]*m[1,1] + m[1,2]*m[1,2] + m[1,3]*m[1,3]);
	y := Sqrt(m[2,1]*m[2,1] + m[2,2]*m[2,2] + m[2,3]*m[2,3]);
	z := Sqrt(m[3,1]*m[3,1] + m[3,2]*m[3,2] + m[3,3]*m[3,3]);
end;

procedure ParseXYZ(const str : String; out x, y, z : Single);
var
	I,E:Longint;
begin
	I := 1; E := 1;
	while str[E] <> ' ' do Inc(E);
	x := StrToFloat(Copy(str, I, E-I));
	I := E+1; E := E+1;
	while str[E] <> ' ' do Inc(E);
	y := StrToFloat(Copy(str, I, E-I));
	I := E+1; E := E+1;
	while (E <= Length(str)) and (str[E] <> ' ') do Inc(E); 
	z := StrToFloat(Copy(str, I, E-I));
end;

// translate * rotatey * rotatex * rotatez
procedure MatrixBone2033(out m : TMatrix; yaw, pitch, roll : Single; x, y, z : Single);
var
	sh, ch, sp, cp, sb, cb : Single;
begin
	sh := Sin(yaw);
	ch := Cos(yaw);
	sp := Sin(pitch);
	cp := Cos(pitch);
	sb := Sin(roll);
	cb := Cos(roll);

	m[1][1] := ch * cb + sh * sp * sb;
	m[1][2] := sb * cp;
	m[1][3] := -sh * cb + ch * sp * sb;
	m[1][4] := 0.0;
	m[2][1] := -ch * sb + sh * sp * cb;
	m[2][2] := cb * cp;
	m[2][3] := sb * sh + ch * sp * cb;
	m[2][4] := 0.0;
	m[3][1] := sh * cp;
	m[3][2] := -sp;
	m[3][3] := ch * cp;
	m[3][4] := 0.0;
	m[4][1] := x;
	m[4][2] := y;
	m[4][3] := z;
	m[4][4] := 1.0;
end;

function btn_ok_cb(ih : Ihandle) : Longint; cdecl;
begin
	IupSetAttribute(IupGetDialog(ih), 'VALUE', '1');
	Result := IUP_CLOSE;
end;

function btn_cancel_cb(ih : Ihandle) : Longint; cdecl;
begin
	IupSetAttribute(IupGetDialog(ih), 'VALUE', '0');
	Result := IUP_CLOSE;
end;

function EditMatrix(var m : TMatrix; show_scale : Boolean) : Boolean;
var
	text_translate, text_rotate, text_scale : Ihandle;
	btn_ok, btn_cancel : Ihandle;
	dlg : Ihandle;
	
	pos : TVec3;
	rot : TVec3;
	scale : TVec3;
	
	mat, mat_scale : TMatrix;
	
	done : Boolean;
begin
	Result := False;
			
	text_translate := IupText(nil);
	text_rotate := IupText(nil);
	text_scale := IupText(nil);
	
	IupSetAttributes(text_translate, 'EXPAND=HORIZONTAL, VISIBLECOLUMNS=30');
	IupSetAttributes(text_rotate, 'EXPAND=HORIZONTAL, VISIBLECOLUMNS=30');
	IupSetAttributes(text_scale, 'EXPAND=HORIZONTAL, VISIBLECOLUMNS=30');
	
	iup.SetStrAttribute(text_translate, 'VALUE',
		FloatToStr(m[4,1]) + ' ' + FloatToStr(m[4,2]) + ' ' + FloatToStr(m[4,3])
	);
	
	GetHPB(m, rot.y, rot.x, rot.z);
	
	iup.SetStrAttribute(text_rotate, 'VALUE',
		FloatToStr(rot.x/(PI/180.0)) + ' ' + FloatToStr(rot.y/(PI/180.0)) + ' ' + FloatToStr(rot.z/(PI/180.0))
	);	
	
	GetScale(m, scale.x, scale.y, scale.z);
	
	iup.SetStrAttribute(text_scale, 'VALUE',
		FloatToStr(scale.x) + ' ' + FloatToStr(scale.y) + ' ' + FloatToStr(scale.z)
	);
	
	btn_ok := iup.Button('OK', @btn_ok_cb);
	btn_cancel := iup.Button('Cancel', @btn_cancel_cb);
	
	dlg := IupDialog(
		IupSetAttributes(IupGridbox(
			IupLabel('Translate: '), text_translate, 
			IupLabel('Rotate: '), text_rotate, 
			IupLabel('Scale: '), text_scale,
			IupSetAttributes(IupHBox(btn_ok, btn_cancel, nil), 'GAP=5'), 
			nil
		), 'NUMDIV=2, GAPCOL=5, GAPLIN=5')
	);
	
	IupSetAttribute(dlg, 'TITLE', 'Edit Matrix');
	IupSetAttributeHandle(dlg, 'DEFAULTENTER', btn_ok);
	IupSetAttributeHandle(dlg, 'DEFAULTESC', btn_cancel);
	IupSetAttribute(dlg, 'MARGIN', '5x5');
	
	repeat
		IupPopup(dlg, IUP_CENTER, IUP_CENTER);
		done := True;
		
		if IupGetAttribute(dlg, 'VALUE') = '1' then
		try
			ParseXYZ(Trim(IupGetAttribute(text_translate, 'VALUE')), pos.x, pos.y, pos.z);
			ParseXYZ(Trim(IupGetAttribute(text_rotate, 'VALUE')), rot.x, rot.y, rot.z);
			ParseXYZ(Trim(IupGetAttribute(text_scale, 'VALUE')), scale.x, scale.y, scale.z);
			
			MatrixBone2033(mat, -rot.y*(PI/180), -rot.x*(PI/180), -rot.z*(PI/180), pos.x, pos.y, pos.z);
			VMath.Scale(mat_scale, scale);
			Mul44(mat, mat_scale);
			
			m := mat;
			Result := True;
		except
			on E : Exception do
			begin
				IupMessageError(nil, PAnsiChar(E.Message));
				done := False;
			end;
		end;
	until done;
	
	IupDestroy(dlg);
end;

end.
