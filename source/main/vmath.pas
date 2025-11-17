unit vmath;
interface

type
  TVec2 = record
    x, y : Single;
  end;
  PVec2 = ^TVec2;

  TVec2S16 = record
    x, y : Smallint;
  end;
  PVec2S16 = ^TVec2S16;

  TVec3 = record
    x, y, z : Single;
  end;
  PVec3 = ^TVec3;

  TVec4 = record
    x, y, z, w : Single;
  end;
  PVec4 = ^TVec4;

  TVec4S16 = record
    x, y, z, w : Smallint;
  end;
  PVec4S16 = ^TVec4S16;

  TMatrix = array[1..4,1..4] of Single;
  TMatrix33 = array[1..3,1..3] of Single;

  TAABB = record
    min, max : TVec3;
  end;

  TSphere = record
    center : TVec3;
    radius : Single;
  end;

  TPlane = record
    x, y, z, d : Single;
  end;

procedure Normalize(var v : TVec4); overload;
procedure Normalize(var v : TVec3); overload;
procedure NormalizeLen(var v : TVec3; out len : Single);
procedure Normalize(var v : TVec2); overload;
function Dot(const v1 : TVec3; const v2 : TVec3) : Single; overload;
function Dot(const v1 : TVec2; const v2 : TVec2) : Single; overload;

procedure Cross(out c : TVec3; const v1, v2 : TVec3);

function Distance(const v1 : TVec3; const v2 : TVec3) : Single;

procedure Transform(var v : TVec3; const m : TMatrix);
procedure Transform33(var v : TVec3; const m : TMatrix);

procedure AABBMerge(var a : TAABB; const b : TAABB);
procedure AABBMergePoint(var a : TAABB; const p : TVec3);
procedure AABBTransform(var a : TAABB; const m : TMatrix);
procedure AABBCorners(out c : array of TVec3; const box : TAABB);
procedure AABBCenter(out c : TVec3; const box : TAABB);
procedure CalcBSphere(out s : TSphere; const b : TAABB);

function PackNormal(const normal : TVec3) : Longword;
procedure UnpackNormal(var n : TVec3; normal : Longword);

procedure FrustumFromMatrix(out frustum : array of TPlane; const vp : TMatrix);
function AABBVisible(const frustum : array of TPlane; const box : TAABB) : Boolean;

procedure Identity(var m : TMatrix);
procedure Translate(var m : TMatrix; const v : TVec3); overload;
procedure Translate(var m : TMatrix; x, y, z : Single); overload;
procedure Scale(var m : TMatrix; const v : TVec3); overload;
procedure Scale(var m : TMatrix; x, y, z : Single); overload;
procedure Mul44(var a : TMatrix; const b : TMatrix); overload;
procedure Mul44(out mat : TMatrix; const a : TMatrix; const b : TMatrix); overload;
procedure Mul44Transpose(out mat : TMatrix; const a : TMatrix; const b : TMatrix); overload;
procedure Invert43(var mat : TMatrix); overload;
procedure Invert43(out mat : TMatrix; const m : TMatrix); overload;
procedure RotateAxis(out m : TMatrix; const axis : TVec3; angle : Single); overload;
procedure RotateAxis(out m : TMatrix; ax, ay, az, angle : Single); overload;
procedure RotateQuaternion(out m : TMatrix; const q : TVec4); overload;
procedure RotateQuaternion(out m : TMatrix; qx, qy, qz, qw : Single); overload;
procedure Transpose(var m : TMatrix); overload;
procedure Transpose(out m : TMatrix; const s : TMatrix); overload;
procedure NormalizeMatrixAndGetScale(var mat : TMatrix; out scale : TVec3);

procedure PerspectiveLH(out m : TMatrix; fov, aspect, znear, zfar : Single);
procedure LookAtLH(out m : TMatrix; const eye, center, up : TVec3); overload;
procedure LookAtLH(out m : TMatrix; eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz : Single); overload;

procedure GetHPB(const m : TMatrix; out h, p, b : Single);
procedure SetHPB(out m : TMatrix; yaw, pitch, roll : Single);

procedure MakeMatrixFromDir(out v1, v2 : TVec3; const v3 : TVec3);

procedure Decompose(const m : TMatrix; out translate, rot, scale : TVec3);

procedure QuatRotateAxis(out q : TVec4; const axis : TVec3; angle : Single); overload;
procedure QuatRotateAxis(out q : TVec4; x, y, z, angle : Single); overload;
procedure QuatMul(var q : TVec4; const q2 : TVec4);

implementation
uses Math;

procedure Normalize(var v : TVec4);
var
  len : Single;
begin
  len := Sqrt(v.x*v.x + v.y*v.y + v.z*v.z + v.w*v.w);
  v.x := v.x / len;
  v.y := v.y / len;
  v.z := v.z / len;
  v.w := v.w / len;
end;

procedure Normalize(var v : TVec3);
var
  len : Single;
begin
  len := Sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
  v.x := v.x / len;
  v.y := v.y / len;
  v.z := v.z / len;
end;

procedure NormalizeLen(var v : TVec3; out len : Single);
begin
  len := Sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
  if len <> 0.0 then
  begin
    v.x := v.x / len;
    v.y := v.y / len;
    v.z := v.z / len;
  end;
end;

procedure Normalize(var v : TVec2);
var
  len : Single;
begin
  len := Sqrt(v.x*v.x + v.y*v.y);
  v.x := v.x / len;
  v.y := v.y / len;
end;

function Dot(const v1 : TVec3; const v2 : TVec3) : Single;
begin
  Dot := v1.x*v2.x + v1.y*v2.y + v1.z*v2.z;
end;

function Dot(const v1 : TVec2; const v2 : TVec2) : Single;
begin
  Dot := v1.x*v2.x + v1.y*v2.y;
end;

procedure Cross(out c : TVec3; const v1, v2 : TVec3);
begin
  c.x := v1.y * v2.z - v1.z * v2.y;
  c.y := v1.z * v2.x - v1.x * v2.z;
  c.z := v1.x * v2.y - v1.y * v2.x;
end;

function Distance(const v1 : TVec3; const v2 : TVec3) : Single;
var
	diff : TVec3;
begin
	diff.x := v1.x - v2.x;
	diff.y := v1.y - v2.y;
	diff.z := v1.z - v2.z;
	Result := Sqrt(diff.x*diff.x + diff.y*diff.y + diff.z*diff.z);
end;

procedure Transform(var v : TVec3; const m : TMatrix);
var
  x, y, z, w : Single;
begin
  x := v.x;
  y := v.y;
  z := v.z;
  w := m[1,4] + m[2,4] + m[3,4] + m[4,4];

  v.x := x*m[1,1] + y*m[2,1] + z*m[3,1] + m[4,1];
  v.y := x*m[1,2] + y*m[2,2] + z*m[3,2] + m[4,2];
  v.z := x*m[1,3] + y*m[2,3] + z*m[3,3] + m[4,3];

  v.x := v.x / w;
  v.y := v.y / w;
  v.z := v.z / w;
end;

procedure Transform33(var v : TVec3; const m : TMatrix);
var
  x, y, z : Single;
begin
  x := v.x;
  y := v.y;
  z := v.z;

  v.x := x*m[1,1] + y*m[2,1] + z*m[3,1];
  v.y := x*m[1,2] + y*m[2,2] + z*m[3,2];
  v.z := x*m[1,3] + y*m[2,3] + z*m[3,3];
end;

procedure AABBMerge(var a : TAABB; const b : TAABB);
begin
  if a.min.x > b.min.x then
    a.min.x := b.min.x;
  if a.min.y > b.min.y then
    a.min.y := b.min.y;
  if a.min.z > b.min.z then
    a.min.z := b.min.z;
  if a.max.x < b.max.x then
    a.max.x := b.max.x;
  if a.max.y < b.max.y then
    a.max.y := b.max.y;
  if a.max.z < b.max.z then
    a.max.z := b.max.z;
end;

procedure AABBMergePoint(var a : TAABB; const p : TVec3);
begin
  if p.x < a.min.x then
    a.min.x := p.x;
  if p.y < a.min.y then
    a.min.y := p.y;
  if p.z < a.min.z then
    a.min.z := p.z;
  if p.x > a.max.x then
    a.max.x := p.x;
  if p.y > a.max.y then
    a.max.y := p.y;
  if p.z > a.max.z then
    a.max.z := p.z;
end;

procedure AABBTransform(var a : TAABB; const m : TMatrix);
var
  I : Integer;
  c : array[0..7] of TVec3;
begin
  AABBCorners(c, a);

	Transform(c[0], m);
	a.min := c[0];
	a.max := c[0];

  for I := 1 to 7 do
  begin
    Transform(c[I], m);
    AABBMergePoint(a, c[I]);
  end;
end;

procedure AABBCorners(out c : array of TVec3; const box : TAABB);
begin
  c[0] := box.min;

  c[1].x := box.min.x;
  c[1].y := box.min.y;
  c[1].z := box.max.z;

  c[2].x := box.max.x;
  c[2].y := box.min.y;
  c[2].z := box.max.z;

  c[3].x := box.max.x;
  c[3].y := box.min.y;
  c[3].z := box.min.z;

  c[4].x := box.min.x;
  c[4].y := box.max.y;
  c[4].z := box.min.z;

  c[5].x := box.min.x;
  c[5].y := box.max.y;
  c[5].z := box.max.z;

  c[6] := box.max;

  c[7].x := box.max.x;
  c[7].y := box.max.y;
  c[7].z := box.min.z;
end;

procedure AABBCenter(out c : TVec3; const box : TAABB);
begin
	c.x := box.min.x + ((box.max.x-box.min.x) / 2);
	c.y := box.min.y + ((box.max.y-box.min.y) / 2);
	c.z := box.min.z + ((box.max.z-box.min.z) / 2);
end;

procedure CalcBSphere(out s : TSphere; const b : TAABB);
var
  half : TVec3;
begin
  half.x := (b.max.x-b.min.x)/2;
  half.y := (b.max.y-b.min.y)/2;
  half.z := (b.max.z-b.min.z)/2;
  s.center.x := b.min.x+half.x;
  s.center.y := b.min.y+half.y;
  s.center.z := b.min.z+half.z;
  s.radius := Sqrt(half.x*half.x+half.y*half.y+half.z*half.z);
end;

function PackNormal(const normal : TVec3) : Longword;
var
  nx, ny, nz : Byte;
begin
  nx := Trunc(((normal.x+1)/2)*255);
  ny := Trunc(((normal.y+1)/2)*255);
  nz := Trunc(((normal.z+1)/2)*255);

  Result := (nx shl 16) or (ny shl 8) or nz;
end;

procedure UnpackNormal(var n : TVec3; normal : Longword);
begin
  n.x := (((normal shr 16) and $FF) / 255 * 2) - 1;
  n.y := (((normal shr 8) and $FF) / 255 * 2) - 1;
  n.z := ((normal and $FF) / 255 * 2) - 1;
end;

procedure FrustumFromMatrix(out frustum : array of TPlane; const vp : TMatrix);
begin
	frustum[0].x := vp[1,4] + vp[1,1];
	frustum[0].y := vp[2,4] + vp[2,1];
	frustum[0].z := vp[3,4] + vp[3,1];
	frustum[0].d := vp[4,4] + vp[4,1];

	frustum[1].x := vp[1,4] - vp[1,1];
	frustum[1].y := vp[2,4] - vp[2,1];
	frustum[1].z := vp[3,4] - vp[3,1];
	frustum[1].d := vp[4,4] - vp[4,1];

	frustum[2].x := vp[1,4] - vp[1,2];
	frustum[2].y := vp[2,4] - vp[2,2];
	frustum[2].z := vp[3,4] - vp[3,2];
	frustum[2].d := vp[4,4] - vp[4,2];

	frustum[3].x := vp[1,4] + vp[1,2];
	frustum[3].y := vp[2,4] + vp[2,2];
	frustum[3].z := vp[3,4] + vp[3,2];
	frustum[3].d := vp[4,4] + vp[4,2];

	frustum[4].x := vp[1,4] - vp[1,3];
	frustum[4].y := vp[2,4] - vp[2,3];
	frustum[4].z := vp[3,4] - vp[3,3];
	frustum[4].d := vp[4,4] - vp[4,3];

	frustum[5].x := vp[1,4] + vp[1,3];
	frustum[5].y := vp[2,4] + vp[2,3];
	frustum[5].z := vp[3,4] + vp[3,3];
	frustum[5].d := vp[4,4] + vp[4,3];
end;

{
function AABBVisible(const frustum : array of TPlane; const box : TAABB) : Boolean;
var
  I : Integer;
  c : array[0..7] of TVec3;
  dist : Single;
begin
  AABBCorners(c, box);

  Result := True;

  for I := 0 to 5 do
  begin
    dist := c[0].x * frustum[I].x + c[0].y * frustum[I].y + c[0].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[1].x * frustum[I].x + c[1].y * frustum[I].y + c[1].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[2].x * frustum[I].x + c[2].y * frustum[I].y + c[2].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[3].x * frustum[I].x + c[3].y * frustum[I].y + c[3].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[4].x * frustum[I].x + c[4].y * frustum[I].y + c[4].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[5].x * frustum[I].x + c[5].y * frustum[I].y + c[5].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[6].x * frustum[I].x + c[6].y * frustum[I].y + c[6].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[7].x * frustum[I].x + c[7].y * frustum[I].y + c[7].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;

    Result := False;
    Break;
  end;

end;
}
function AABBVisible(const frustum : array of TPlane; const box : TAABB) : Boolean;
var
	I : Integer;
	{n,} p : TVec3;
begin
	Result := True;
	
	for I := 0 to 5 do
	begin
		if frustum[I].x >= 0.0 then
		begin
			p.x := box.max.x;
			//n.x := box.min.x;
		end else
		begin
			p.x := box.min.x;
			//n.x := box.max.x;
		end;
		if frustum[I].y >= 0.0 then
		begin
			p.y := box.max.y;
			//n.y := box.min.y;
		end else
		begin
			p.y := box.min.y;
			//n.y := box.max.y;
		end;
		if frustum[I].z >= 0.0 then
		begin
			p.z := box.max.z;
			//n.z := box.min.z;
		end else
		begin
			p.z := box.min.z;
			//n.z := box.max.z;
		end;
		
		if (p.x*frustum[I].x + p.y*frustum[I].y + p.z*frustum[I].z + frustum[I].d) < 0.0 then
		begin
			Result := False;
			Exit;
		end;
	end;
	
end;

procedure Identity(var m : TMatrix);
begin
	m[1,1] := 1.0; m[1,2] := 0.0; m[1,3] := 0.0; m[1,4] := 0.0;
	m[2,1] := 0.0; m[2,2] := 1.0; m[2,3] := 0.0; m[2,4] := 0.0;
	m[3,1] := 0.0; m[3,2] := 0.0; m[3,3] := 1.0; m[3,4] := 0.0;
	m[4,1] := 0.0; m[4,2] := 0.0; m[4,3] := 0.0; m[4,4] := 1.0;
end;

procedure Translate(var m : TMatrix; x, y, z : Single); overload;
begin
	m[1,1] := 1.0; m[1,2] := 0.0; m[1,3] := 0.0; m[1,4] := 0.0;
	m[2,1] := 0.0; m[2,2] := 1.0; m[2,3] := 0.0; m[2,4] := 0.0;
	m[3,1] := 0.0; m[3,2] := 0.0; m[3,3] := 1.0; m[3,4] := 0.0;
	m[4,1] := x; m[4,2] := y; m[4,3] := z; m[4,4] := 1.0;
end;

procedure Translate(var m : TMatrix; const v : TVec3); overload;
begin
	m[1,1] := 1.0; m[1,2] := 0.0; m[1,3] := 0.0; m[1,4] := 0.0;
	m[2,1] := 0.0; m[2,2] := 1.0; m[2,3] := 0.0; m[2,4] := 0.0;
	m[3,1] := 0.0; m[3,2] := 0.0; m[3,3] := 1.0; m[3,4] := 0.0;
	m[4,1] := v.x; m[4,2] := v.y; m[4,3] := v.z; m[4,4] := 1.0;
end;

procedure Scale(var m : TMatrix; x, y, z : Single); overload;
begin
	m[1,1] := x  ; m[1,2] := 0.0; m[1,3] := 0.0; m[1,4] := 0.0;
	m[2,1] := 0.0; m[2,2] := y  ; m[2,3] := 0.0; m[2,4] := 0.0;
	m[3,1] := 0.0; m[3,2] := 0.0; m[3,3] := z  ; m[3,4] := 0.0;
	m[4,1] := 0.0; m[4,2] := 0.0; m[4,3] := 0.0; m[4,4] := 1.0;
end;

procedure Scale(var m : TMatrix; const v : TVec3); overload;
begin
	m[1,1] := v.x; m[1,2] := 0.0; m[1,3] := 0.0; m[1,4] := 0.0;
	m[2,1] := 0.0; m[2,2] := v.y; m[2,3] := 0.0; m[2,4] := 0.0;
	m[3,1] := 0.0; m[3,2] := 0.0; m[3,3] := v.z; m[3,4] := 0.0;
	m[4,1] := 0.0; m[4,2] := 0.0; m[4,3] := 0.0; m[4,4] := 1.0;
end;

procedure Mul44(var a : TMatrix; const b : TMatrix);
var
	m : TMatrix;
begin
	m := a;
	a[1,1] := m[1,1] * b[1,1] + m[2,1] * b[1,2] + m[3,1] * b[1,3] + m[4,1] * b[1,4];
	a[1,2] := m[1,2] * b[1,1] + m[2,2] * b[1,2] + m[3,2] * b[1,3] + m[4,2] * b[1,4];
	a[1,3] := m[1,3] * b[1,1] + m[2,3] * b[1,2] + m[3,3] * b[1,3] + m[4,3] * b[1,4];
	a[1,4] := m[1,4] * b[1,1] + m[2,4] * b[1,2] + m[3,4] * b[1,3] + m[4,4] * b[1,4];
	
	a[2,1] := m[1,1] * b[2,1] + m[2,1] * b[2,2] + m[3,1] * b[2,3] + m[4,1] * b[2,4];
	a[2,2] := m[1,2] * b[2,1] + m[2,2] * b[2,2] + m[3,2] * b[2,3] + m[4,2] * b[2,4];
	a[2,3] := m[1,3] * b[2,1] + m[2,3] * b[2,2] + m[3,3] * b[2,3] + m[4,3] * b[2,4];
	a[2,4] := m[1,4] * b[2,1] + m[2,4] * b[2,2] + m[3,4] * b[2,3] + m[4,4] * b[2,4];
	
	a[3,1] := m[1,1] * b[3,1] + m[2,1] * b[3,2] + m[3,1] * b[3,3] + m[4,1] * b[3,4];
	a[3,2] := m[1,2] * b[3,1] + m[2,2] * b[3,2] + m[3,2] * b[3,3] + m[4,2] * b[3,4];
	a[3,3] := m[1,3] * b[3,1] + m[2,3] * b[3,2] + m[3,3] * b[3,3] + m[4,3] * b[3,4];
	a[3,4] := m[1,4] * b[3,1] + m[2,4] * b[3,2] + m[3,4] * b[3,3] + m[4,4] * b[3,4];
	
	a[4,1] := m[1,1] * b[4,1] + m[2,1] * b[4,2] + m[3,1] * b[4,3] + m[4,1] * b[4,4];
	a[4,2] := m[1,2] * b[4,1] + m[2,2] * b[4,2] + m[3,2] * b[4,3] + m[4,2] * b[4,4];
	a[4,3] := m[1,3] * b[4,1] + m[2,3] * b[4,2] + m[3,3] * b[4,3] + m[4,3] * b[4,4];
	a[4,4] := m[1,4] * b[4,1] + m[2,4] * b[4,2] + m[3,4] * b[4,3] + m[4,4] * b[4,4];
end;

procedure Mul44(out mat : TMatrix; const a : TMatrix; const b : TMatrix);
begin
	mat[1,1] := a[1,1] * b[1,1] + a[2,1] * b[1,2] + a[3,1] * b[1,3] + a[4,1] * b[1,4];
	mat[1,2] := a[1,2] * b[1,1] + a[2,2] * b[1,2] + a[3,2] * b[1,3] + a[4,2] * b[1,4];
	mat[1,3] := a[1,3] * b[1,1] + a[2,3] * b[1,2] + a[3,3] * b[1,3] + a[4,3] * b[1,4];
	mat[1,4] := a[1,4] * b[1,1] + a[2,4] * b[1,2] + a[3,4] * b[1,3] + a[4,4] * b[1,4];
	
	mat[2,1] := a[1,1] * b[2,1] + a[2,1] * b[2,2] + a[3,1] * b[2,3] + a[4,1] * b[2,4];
	mat[2,2] := a[1,2] * b[2,1] + a[2,2] * b[2,2] + a[3,2] * b[2,3] + a[4,2] * b[2,4];
	mat[2,3] := a[1,3] * b[2,1] + a[2,3] * b[2,2] + a[3,3] * b[2,3] + a[4,3] * b[2,4];
	mat[2,4] := a[1,4] * b[2,1] + a[2,4] * b[2,2] + a[3,4] * b[2,3] + a[4,4] * b[2,4];
	
	mat[3,1] := a[1,1] * b[3,1] + a[2,1] * b[3,2] + a[3,1] * b[3,3] + a[4,1] * b[3,4];
	mat[3,2] := a[1,2] * b[3,1] + a[2,2] * b[3,2] + a[3,2] * b[3,3] + a[4,2] * b[3,4];
	mat[3,3] := a[1,3] * b[3,1] + a[2,3] * b[3,2] + a[3,3] * b[3,3] + a[4,3] * b[3,4];
	mat[3,4] := a[1,4] * b[3,1] + a[2,4] * b[3,2] + a[3,4] * b[3,3] + a[4,4] * b[3,4];
	
	mat[4,1] := a[1,1] * b[4,1] + a[2,1] * b[4,2] + a[3,1] * b[4,3] + a[4,1] * b[4,4];
	mat[4,2] := a[1,2] * b[4,1] + a[2,2] * b[4,2] + a[3,2] * b[4,3] + a[4,2] * b[4,4];
	mat[4,3] := a[1,3] * b[4,1] + a[2,3] * b[4,2] + a[3,3] * b[4,3] + a[4,3] * b[4,4];
	mat[4,4] := a[1,4] * b[4,1] + a[2,4] * b[4,2] + a[3,4] * b[4,3] + a[4,4] * b[4,4];
end;

procedure Mul44Transpose(out mat : TMatrix; const a : TMatrix; const b : TMatrix);
begin
	mat[1,1] := a[1,1] * b[1,1] + a[2,1] * b[1,2] + a[3,1] * b[1,3] + a[4,1] * b[1,4];
	mat[2,1] := a[1,2] * b[1,1] + a[2,2] * b[1,2] + a[3,2] * b[1,3] + a[4,2] * b[1,4];
	mat[3,1] := a[1,3] * b[1,1] + a[2,3] * b[1,2] + a[3,3] * b[1,3] + a[4,3] * b[1,4];
	mat[4,1] := a[1,4] * b[1,1] + a[2,4] * b[1,2] + a[3,4] * b[1,3] + a[4,4] * b[1,4];
	
	mat[1,2] := a[1,1] * b[2,1] + a[2,1] * b[2,2] + a[3,1] * b[2,3] + a[4,1] * b[2,4];
	mat[2,2] := a[1,2] * b[2,1] + a[2,2] * b[2,2] + a[3,2] * b[2,3] + a[4,2] * b[2,4];
	mat[3,2] := a[1,3] * b[2,1] + a[2,3] * b[2,2] + a[3,3] * b[2,3] + a[4,3] * b[2,4];
	mat[4,2] := a[1,4] * b[2,1] + a[2,4] * b[2,2] + a[3,4] * b[2,3] + a[4,4] * b[2,4];
	
	mat[1,3] := a[1,1] * b[3,1] + a[2,1] * b[3,2] + a[3,1] * b[3,3] + a[4,1] * b[3,4];
	mat[2,3] := a[1,2] * b[3,1] + a[2,2] * b[3,2] + a[3,2] * b[3,3] + a[4,2] * b[3,4];
	mat[3,3] := a[1,3] * b[3,1] + a[2,3] * b[3,2] + a[3,3] * b[3,3] + a[4,3] * b[3,4];
	mat[4,3] := a[1,4] * b[3,1] + a[2,4] * b[3,2] + a[3,4] * b[3,3] + a[4,4] * b[3,4];
	
	mat[1,4] := a[1,1] * b[4,1] + a[2,1] * b[4,2] + a[3,1] * b[4,3] + a[4,1] * b[4,4];
	mat[2,4] := a[1,2] * b[4,1] + a[2,2] * b[4,2] + a[3,2] * b[4,3] + a[4,2] * b[4,4];
	mat[3,4] := a[1,3] * b[4,1] + a[2,3] * b[4,2] + a[3,3] * b[4,3] + a[4,3] * b[4,4];
	mat[4,4] := a[1,4] * b[4,1] + a[2,4] * b[4,2] + a[3,4] * b[4,3] + a[4,4] * b[4,4];
end;

procedure Invert43(var mat : TMatrix); overload;
var
	m : TMatrix;
begin
	m := mat;
	Invert43(mat, m);
end;

procedure Invert43(out mat : TMatrix; const m : TMatrix); overload;
var
	det, det_inv : Single;
begin

	det := 
		m[1,1] * (m[2,2] * m[3,3] - m[2,3] * m[3,2]) - 
		m[1,2] * (m[2,1] * m[3,3] - m[2,3] * m[3,1]) +
		m[1,3] * (m[2,1] * m[3,2] - m[2,2] * m[3,1]);
	det_inv := 1.0 / det;
	
	mat[1,1] :=  det_inv * (m[2,2] * m[3,3] - m[2,3] * m[3,2]);
	mat[1,2] := -det_inv * (m[1,2] * m[3,3] - m[1,3] * m[3,2]);
	mat[1,3] :=  det_inv * (m[1,2] * m[2,3] - m[1,3] * m[2,2]);
	mat[1,4] := 0.0;
	
	mat[2,1] := -det_inv * (m[2,1] * m[3,3] - m[2,3] * m[3,1]);
	mat[2,2] :=  det_inv * (m[1,1] * m[3,3] - m[1,3] * m[3,1]);
	mat[2,3] := -det_inv * (m[1,1] * m[2,3] - m[1,3] * m[2,1]);
	mat[2,4] := 0.0;
	
	mat[3,1] :=  det_inv * (m[2,1] * m[3,2] - m[2,2] * m[3,1]);
	mat[3,2] := -det_inv * (m[1,1] * m[3,2] - m[1,2] * m[3,1]);
	mat[3,3] :=  det_inv * (m[1,1] * m[2,2] - m[1,2] * m[2,1]);
	mat[3,4] := 0.0;
	
	mat[4,1] := -(m[4,1] * mat[1,1] + m[4,2] * mat[2,1] + m[4,3] * mat[3,1]);
	mat[4,2] := -(m[4,1] * mat[1,2] + m[4,2] * mat[2,2] + m[4,3] * mat[3,2]);
	mat[4,3] := -(m[4,1] * mat[1,3] + m[4,2] * mat[2,3] + m[4,3] * mat[3,3]);
	mat[4,4] := 1.0;
end;

procedure RotateAxis(out m : TMatrix; const axis : TVec3; angle : Single); overload;
var
	Cosine : Single;
	Sine : Single;
begin
	Cosine	:= Cos(angle);
	Sine		:= Sin(angle);
	m[1][1] := axis.x * axis.x + ( 1 - axis.x * axis.x) * Cosine;
	m[1][2] := axis.x * axis.y * ( 1 - Cosine ) + axis.z * Sine;
	m[1][3] := axis.x * axis.z * ( 1 - Cosine ) - axis.y * Sine;
	m[1][4] := 0;
	m[2][1] := axis.x * axis.y * ( 1 - Cosine ) - axis.z * Sine;
	m[2][2] := axis.y * axis.y + ( 1 - axis.y * axis.y) * Cosine;
	m[2][3] := axis.y * axis.z * ( 1 - Cosine ) + axis.x * Sine;
	m[2][4] := 0;
	m[3][1] := axis.x * axis.z * ( 1 - Cosine ) + axis.y * Sine;
	m[3][2] := axis.y * axis.z * ( 1 - Cosine ) - axis.x * Sine;
	m[3][3] := axis.z * axis.z + ( 1 - axis.z * axis.z) * Cosine;
	m[3][4] := 0; m[4][1] := 0; m[4][2] := 0;
	m[4][3] := 0; m[4][4] := 1;
end;

procedure RotateAxis(out m : TMatrix; ax, ay, az, angle : Single); overload;
var
	axis : TVec3;
begin
	axis.x := ax;
	axis.y := ay;
	axis.z := az;
	RotateAxis(m, axis, angle);
end;

procedure RotateQuaternion(out m : TMatrix; const q : TVec4);
var
	tx, ty, tz : Single;
	twx, twy, twz : Single;
	txx, txy, txz : Single;
	tyy, tyz, tzz : Single;
begin
	tx  := q.x + q.x;
	ty  := q.y + q.y;
	tz  := q.z + q.z;
	twx := tx * q.w;
	twy := ty * q.w;
	twz := tz * q.w;
	txx := tx * q.x;
	txy := ty * q.x;
	txz := tz * q.x;
	tyy := ty * q.y;
	tyz := tz * q.y;
	tzz := tz * q.z;
	
	m[1,1] := 1.0 - (tyy + tzz); m[1,2] := txy - twz; m[1,3] := txz + twy; m[1,4] := 0.0;
	m[2,1] := txy + twz; m[2,2] := 1.0 - (txx + tzz); m[2,3] := tyz - twx; m[2,4] := 0.0;
	m[3,1] := txz - twy; m[3,2] := tyz + twx; m[3,3] := 1.0 - (txx + tyy); m[3,4] := 0.0;
	m[4,1] := 0.0; m[4,2] := 0.0; m[4,3] := 0.0; m[4,4] := 1.0;
end;

procedure RotateQuaternion(out m : TMatrix; qx, qy, qz, qw : Single);
var
	q : TVec4;
begin
	q.x := qx;
	q.y := qy;
	q.z := qz;
	q.w := qw;
	RotateQuaternion(m, q);
end;

procedure Transpose(var m : TMatrix); overload;
var
	c : TMatrix;
begin
	c := m;
	Transpose(m, c);
end;

procedure Transpose(out m : TMatrix; const s : TMatrix); overload;
begin
	m[1,1] := s[1,1]; m[1,2] := s[2,1]; m[1,3] := s[3,1]; m[1,4] := s[4,1];
	m[2,1] := s[1,2]; m[2,2] := s[2,2]; m[2,3] := s[3,2]; m[2,4] := s[4,2];
	m[3,1] := s[1,3]; m[3,2] := s[2,3]; m[3,3] := s[3,3]; m[3,4] := s[4,3];
	m[4,1] := s[1,4]; m[4,2] := s[2,4]; m[4,3] := s[3,4]; m[4,4] := s[4,4];
end;

procedure NormalizeMatrixAndGetScale(var mat : TMatrix; out scale : TVec3);
begin
	scale.x := Sqrt(mat[1,1]*mat[1,1] + mat[1,2]*mat[1,2] + mat[1,3]*mat[1,3]);
	scale.y := Sqrt(mat[2,1]*mat[2,1] + mat[2,2]*mat[2,2] + mat[2,3]*mat[2,3]);
	scale.z := Sqrt(mat[3,1]*mat[3,1] + mat[3,2]*mat[3,2] + mat[3,3]*mat[3,3]);
	
	mat[1,1] := mat[1,1] / scale.x; mat[1,2] := mat[1,2] / scale.x; mat[1,3] := mat[1,3] / scale.x;
	mat[2,1] := mat[2,1] / scale.y; mat[2,2] := mat[2,2] / scale.y; mat[2,3] := mat[2,3] / scale.y;
	mat[3,1] := mat[3,1] / scale.z; mat[3,2] := mat[3,2] / scale.z; mat[3,3] := mat[3,3] / scale.z;
end;

procedure PerspectiveLH(out m : TMatrix; fov, aspect, znear, zfar : Single);
var
	t : Single;
begin
	t := tan(fov/2.0);
	
	m[1,1] := 1.0 / (aspect * t); 
	m[1,2] := 0.0; 
	m[1,3] := 0.0; 
	m[1,4] := 0.0;
	
	m[2,1] := 0.0; 
	m[2,2] := 1.0 / t; 
	m[2,3] := 0.0; 
	m[2,4] := 0.0;
	
	m[3,1] := 0.0; 
	m[3,2] := 0.0; 
	m[3,3] := zfar / (zfar - znear); 
	m[3,4] := 1.0;
	
	m[4,1] := 0.0; 
	m[4,2] := 0.0; 
	m[4,3] := (zfar * znear) / (znear - zfar); 
	m[4,4] := 0.0;
end;

procedure LookAtLH(out m : TMatrix; const eye, center, up : TVec3); overload;
var
	right : TVec3;
	upn : TVec3;
	dir : TVec3;
begin	
	dir.x := center.x - eye.x;
	dir.y := center.y - eye.y;
	dir.z := center.z - eye.z;
	Normalize(dir);
	
	Cross(right, up, dir);
	Normalize(right);
	
	Cross(upn, dir, right);
	Normalize(upn);
	
	m[1,1] := right.x; m[1,2] := upn.x; m[1,3] := dir.x; m[1,4] := 0.0;
	m[2,1] := right.y; m[2,2] := upn.y; m[2,3] := dir.y; m[2,4] := 0.0;
	m[3,1] := right.z; m[3,2] := upn.z; m[3,3] := dir.z; m[3,4] := 0.0;
	m[4,1] := -Dot(right, eye); m[4,2] := -Dot(upn, eye); m[4,3] := -Dot(dir, eye); m[4,4] := 1.0;
end;

procedure LookAtLH(out m : TMatrix; eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz : Single); overload;
var
	eye, center, up : TVec3;
begin
	eye.x := eyex;
	eye.y := eyey;
	eye.z := eyez;
	center.x := centerx;
	center.y := centery;
	center.z := centerz;
	up.x := upx;
	up.y := upy;
	up.z := upz;
	LookAtLH(m, eye, center, up);
end;

// GetHPB - this one is from X-Ray engine

////////////////////////////////////////////////////

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

// SetHPB - this one is from iOrange I guess (MatrixBone2033)
// 

procedure SetHPB(out m : TMatrix; yaw, pitch, roll : Single);
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
	m[4][1] := 0.0;
	m[4][2] := 0.0;
	m[4][3] := 0.0;
	m[4][4] := 1.0;
end;

// Откуда я скопировал этот код?
procedure MakeMatrixFromDir(out v1, v2 : TVec3; const v3 : TVec3);
var
	l, d : Single;
begin
	d := v3.x*0 + v3.y*1 + v3.z*0;
	v2.x := 0-(v3.x*d);
	v2.y := 1-(v3.y*d);
	v2.z := 0-(v3.z*d);
	l := v2.x*v2.x + v2.y*v2.y + v2.z*v2.z;
	if l < 0.001 then
	begin
		//WriteLn('UP = 1, 0, 0');
		d := v3.x*1 + v3.y*0 + v3.z*0;
		v2.x := 1-(v3.x*d);
		v2.y := 0-(v3.y*d);
		v2.z := 0-(v3.z*d);
		l := v2.x*v2.x + v2.y*v2.y + v2.z*v2.z;
	end;
	l := Sqrt(l);
	v2.x := v2.x / l;
	v2.y := v2.y / l;
	v2.z := v2.z / l;
	Cross(v1, v2, v3);
end;

procedure GetScale(const m : TMatrix; out x, y, z : Single);
begin
	x := Sqrt(m[1,1]*m[1,1] + m[1,2]*m[1,2] + m[1,3]*m[1,3]);
	y := Sqrt(m[2,1]*m[2,1] + m[2,2]*m[2,2] + m[2,3]*m[2,3]);
	z := Sqrt(m[3,1]*m[3,1] + m[3,2]*m[3,2] + m[3,3]*m[3,3]);
end;

procedure Decompose(const m : TMatrix; out translate, rot, scale : TVec3);
begin
	GetHPB(m, rot.y, rot.x, rot.z);
	GetScale(m, scale.x, scale.y, scale.z);
	translate.x := m[4,1];
	translate.y := m[4,2];
	translate.z := m[4,3];
end;

procedure QuatRotateAxis(out q : TVec4; const axis : TVec3; angle : Single); overload;
var
	a2 : Single;
	sine : Single;
begin
	a2 := angle * 0.5;
	sine := Sin(a2);
	q.x := axis.x * sine;
	q.y := axis.y * sine;
	q.z := axis.z * sine;
	q.w := Cos(a2);
end;

procedure QuatRotateAxis(out q : TVec4; x, y, z, angle : Single); overload;
var
	a2 : Single;
	sine : Single;
begin
	a2 := angle * 0.5;
	sine := Sin(a2);
	q.x := x * sine;
	q.y := y * sine;
	q.z := z * sine;
	q.w := Cos(a2);
end;

procedure QuatMul(var q : TVec4; const q2 : TVec4);
var
	q1 : TVec4;
begin
	q1 := q;
	
	q.w := q1.w * q2.w - q1.x * q2.x - q1.y * q2.y - q1.z * q2.z;
	q.x := q1.w * q2.x + q1.x * q2.w + q1.y * q2.z - q1.z * q2.y;
	q.y := q1.w * q2.y + q1.y * q2.w + q1.z * q2.x - q1.x * q2.z;
	q.z := q1.w * q2.z + q1.z * q2.w + q1.x * q2.y - q1.y * q2.x;
	
	{
				ret[3] = q1[3] * q2[3] - q1[0] * q2[0] - q1[1] * q2[1] - q1[2] * q2[2];
				ret[0] = q1[3] * q2[0] + q1[0] * q2[3] + q1[1] * q2[2] - q1[2] * q2[1];
				ret[1] = q1[3] * q2[1] + q1[1] * q2[3] + q1[2] * q2[0] - q1[0] * q2[2];
				ret[2] = q1[3] * q2[2] + q1[2] * q2[3] + q1[0] * q2[1] - q1[1] * q2[0];
	}
end;

end.
