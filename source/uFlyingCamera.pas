unit uFlyingCamera;

interface
uses vmath;

type
	TFlyingCamera = record
		camera_type : Byte;

		anglex, angley : Single;
	
		position : TVec3;
		direction : TVec3;

		center : TVec3;
		distance : Single;
		
		procedure Init;
		
		function OnMouseMove(diff_x, diff_y : Longint; alt, btn1, btn2, btn3 : Boolean) : Boolean;
		function OnKeyboard(key : Char) : Boolean;
	
		procedure ApplyMatrix;
	end;

implementation
uses windows, Math, uLEOptions, GL, GLU;

procedure TFlyingCamera.Init;
begin
	distance := 5.0;
end;

function TFlyingCamera.OnMouseMove(diff_x, diff_y : Longint; alt, btn1, btn2, btn3 : Boolean) : Boolean;
var
	sinh, cosh : Single;
begin
	Result := False;
	
	if (alt and btn1 and btn3) or (not alt and btn3) then // крутить
	begin
		anglex := anglex - camera_rotate_sens * diff_y;
		angley := angley - camera_rotate_sens * diff_x;

		if anglex >= 89.9 then
			anglex := 89.9;
		if anglex <= -89.9 then
			anglex := -89.9;

		direction.x := -Sin(angley*(PI/180)) * Cos(anglex*(PI/180));
		direction.y := Sin(anglex*(PI/180));
		direction.z := Cos(angley*(PI/180)) * Cos(anglex*(PI/180));

		Result := True;
	end else
	if alt and btn1 then // перемещать в плоскости XZ
	begin
		sinh := Sin(angley*(PI/180));
		cosh := Cos(angley*(PI/180));
		
		position.x := position.x + (camera_move_sens * diff_y) * sinh;
		position.z := position.z + (camera_move_sens * diff_y) * -cosh;
		
		position.x := position.x + (camera_move_sens * diff_x) * cosh;
		position.z := position.z + (camera_move_sens * diff_x) * sinh;
		
		Result := True;		
	end else
	if alt and btn3 then // перемещать по оси Y
	begin
		position.y := position.y - (camera_move_sens * diff_y);
		
		Result := True;
	end else
	if btn2 then
	begin
		distance := distance + (diff_y / 8);
		Result := True;
	end;
end;

function TFlyingCamera.OnKeyboard(key : Char) : Boolean;
var
	sinx, cosx, siny, cosy : Single;
	m : TVec3;
	
	updated : Boolean;
begin
	sinx := Sin(anglex * (PI/180));
	cosx := Cos(anglex * (PI/180));
	siny := Sin(angley * (PI/180));
	cosy := Cos(angley * (PI/180));

	case key of
		'W', 'w' :
		begin
			m.x := -(siny * cosx);
			m.y := sinx;
			m.z := cosy * cosx;
			updated := True;
		end;
		'S', 's' :
		begin
			m.x := siny * cosx;
			m.y := -sinx;
			m.z := -(cosy * cosx);
			updated := True;
		end;
		'D', 'd' :
		begin
			m.x := cosy;
			m.y := 0;
			m.z := siny;
			updated := True;
		end;
		'A', 'a' :
		begin
			m.x := -cosy;
			m.y := 0;
			m.z := -siny;
			updated := True;
		end;
		else
			updated := False;
	end;

	if updated then
	begin
		if (GetASyncKeyState(VK_SHIFT) and $8000) <> 0 then
		begin
			m.x := m.x * camera_fly_speed_fast;
			m.y := m.y * camera_fly_speed_fast;
			m.z := m.z * camera_fly_speed_fast;
		end else
		begin
			m.x := m.x * camera_fly_speed;
			m.y := m.y * camera_fly_speed;
			m.z := m.z * camera_fly_speed;
		end;		
	
		position.x := position.x + m.x;
		position.y := position.y + m.y;
		position.z := position.z + m.z;
	end;
	
	Result := updated;
end;

procedure TFlyingCamera.ApplyMatrix;
var
	sinx, cosx : Single;
	siny, cosy : Single;
	m : TMatrix;
begin
	if camera_type = 0 then
	begin
		sinx := Sin(anglex * (PI/180));
		cosx := Cos(anglex * (PI/180));
	
		siny := Sin(angley * (PI/180));
		cosy := Cos(angley * (PI/180));
	
		LookAtLH(m, distance*siny*cosx, distance*sinx, distance*cosy*cosx, center.x, center.y, center.z, 0, 1, 0);
		glLoadMatrixf(@m);
	end;
	
	if camera_type = 1 then
	begin
		glLoadIdentity;
		glRotatef(anglex, 1, 0, 0);
		glRotatef(angley, 0, 1, 0);
		glTranslatef(-position.x, -position.y, -position.z);
	end;	
end;

end.