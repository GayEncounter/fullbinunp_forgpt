unit Manipulator;

interface
uses vmath;

type
	IGizmo = Pointer;

type
	TManipulator = class
		FMatrix : TMatrix;
		FMatrix_orig : TMatrix;
		FDiff		: TMatrix;
		lineWidth : Longint;
		world : Boolean;
		
		active : Boolean;
		gizmo : IGizmo;

		constructor Create(isworld : Boolean; line_width : Longint);
		destructor Destroy; override;

		procedure Draw; virtual;

		function Activate(x, y : Longint) : Boolean;
		procedure Deactivate;
		function IsActive : Boolean;

		procedure Update(x, y : Longint);

		protected
		procedure SetMatrix(const m : TMatrix);

		procedure OnActivate; virtual; abstract;
		procedure OnDeactivate; virtual; abstract;
		procedure OnUpdate(x, y : Longint); virtual; abstract;

		public
		property Matrix : TMatrix read FMatrix write SetMatrix;
		property Diff : TMatrix read FDiff;
	end;

	TMoveManipulator = class(TManipulator)
		constructor Create(isworld : Boolean; line_width : Longint);

		procedure Draw; override;

		protected
		procedure OnActivate; override;
		procedure OnDeactivate; override;
		procedure OnUpdate(x, y : Longint); override;
	end;

	TRotateManipulator = class(TManipulator)
		constructor Create(isworld : Boolean; line_width : Longint);

		procedure Draw; override;

		protected
		procedure OnActivate; override;
		procedure OnDeactivate; override;
		procedure OnUpdate(x, y : Longint); override;
	end;
	
	TScaleManipulator = class(TManipulator)
		uniform : Boolean;
	
		constructor Create(isuniform : Boolean; line_width : LongInt);
	
		procedure Draw; override;
		
		protected
		procedure OnActivate; override;
		procedure OnDeactivate; override;
		procedure OnUpdate(x, y : Longint); override;
	end;

implementation
uses common, GL, GLU;

CONST
	LOCATION_VIEW  = 0;
	LOCATION_WORLD = 1;
	LOCATION_LOCAL = 2;
	
CONST
	AXIS_X         = 1;
	AXIS_Y         = 2;
	AXIS_Z         = 4;
	AXIS_TRACKBALL = 8;
	AXIS_SCREEN    = 16;
	AXIS_ALL       = 31;
	AXIS_XYZ_SCALE = 32;

function  IGizmo_CreateMoveGizmo : IGizmo; cdecl; external 'libgizmo.dll';
function  IGizmo_CreateRotateGizmo : IGizmo; cdecl; external 'libgizmo.dll';
function  IGizmo_CreateScaleGizmo : IGizmo; cdecl; external 'libgizmo.dll';

procedure IGizmo_SetEditMatrix(this : IGizmo; matrix : PSingle); cdecl; external 'libgizmo.dll';
procedure IGizmo_SetCameraMatrix(this : IGizmo; model, proj : PSingle); cdecl; external 'libgizmo.dll';
procedure IGizmo_SetScreenDimension(this : IGizmo; x, y : Longint); cdecl; external 'libgizmo.dll';
function  IGizmo_OnMouseDown(this : IGizmo; x, y : Longint) : Boolean; cdecl; external 'libgizmo.dll';
procedure IGizmo_OnMouseMove(this : IGizmo; x, y : Longint); cdecl; external 'libgizmo.dll';
procedure IGizmo_OnMouseUp(this : IGizmo; x, y : Longint); cdecl; external 'libgizmo.dll';
procedure IGizmo_SetLocation(this : IGizmo; loc : Longint); cdecl; external 'libgizmo.dll';
procedure IGizmo_SetAxisMask(this : IGizmo; mask : Longint); cdecl; external 'libgizmo.dll';
procedure IGizmo_Draw(this : IGizmo); cdecl; external 'libgizmo.dll';

constructor TManipulator.Create(isworld : Boolean; line_width : Longint);
begin
	inherited Create;

	world := isworld;
	lineWidth := line_width;

	Identity(FMatrix);
	Identity(FDiff);
end;

destructor TManipulator.Destroy;
begin
	inherited;
end;

procedure TManipulator.Draw;
begin
	glLineWidth(lineWidth);
	glPushAttrib(GL_ENABLE_BIT);

	if Assigned(gizmo) then
	begin
		IGizmo_SetCameraMatrix(gizmo, @modelview, @proj);
		IGizmo_SetScreenDimension(gizmo, viewport[2], viewport[3]);
		IGizmo_Draw(gizmo);
	end;
	
	glPopAttrib();
	glLineWidth(1.0);
end;

function TManipulator.Activate(x: Integer; y: Integer) : Boolean;
begin
	if IGizmo_OnMouseDown(gizmo, x, y) then
	begin
		active := True;
		FMatrix_orig := FMatrix;
		Activate := True;
	end else
		Activate := False;
end;

procedure TManipulator.Deactivate;
begin
	active := False;
	IGizmo_OnMouseUp(gizmo, 0, 0);
	
	OnDeactivate;
end;

function TManipulator.IsActive : Boolean;
begin
	IsActive := active;
end;

procedure TManipulator.Update(x, y : Longint);
var
	inv_orig : TMatrix;
begin
	IGizmo_OnMouseMove(gizmo, x, y);
	
	//if active then
	begin
		Invert43(inv_orig, FMatrix_orig);
		Mul44(inv_orig, FMatrix);
		FDiff := inv_orig;
		FMatrix_orig := FMatrix;
		
		OnUpdate(x, y);
	end;
end;

procedure TManipulator.SetMatrix(const m: TMatrix);
begin
	if world then
		Translate(FMatrix, m[4,1], m[4,2], m[4,3])
	else
		FMatrix := m;
		
	IGizmo_SetEditMatrix(gizmo, @FMatrix);
end;

constructor TMoveManipulator.Create(isworld : Boolean; line_width : Longint);
begin
	inherited;
	
	gizmo := IGizmo_CreateMoveGizmo;
	if isworld then
		IGizmo_SetLocation(gizmo, LOCATION_WORLD)
	else
		IGizmo_SetLocation(gizmo, LOCATION_LOCAL);
end;

procedure TMoveManipulator.Draw;
begin
	inherited;
end;

procedure TMoveManipulator.OnActivate;
begin
end;

procedure TMoveManipulator.OnDeactivate;
begin
end;

procedure TMoveManipulator.OnUpdate(x, y : Longint);
begin
end;

constructor TRotateManipulator.Create(isworld : Boolean; line_width : Longint);
begin
	inherited;
	
	gizmo := IGizmo_CreateRotateGizmo;
	if isworld then
		IGizmo_SetLocation(gizmo, LOCATION_WORLD)
	else
		IGizmo_SetLocation(gizmo, LOCATION_LOCAL);
end;

procedure TRotateManipulator.Draw;
begin
	inherited;
end;

procedure TRotateManipulator.OnActivate;
begin
end;

procedure TRotateManipulator.OnDeactivate;
begin
end;

procedure TRotateManipulator.OnUpdate(x, y : Longint);
begin
end;

constructor TScaleManipulator.Create(isuniform : Boolean; line_width : Longint);
begin
	Inherited Create(False, line_width);
	uniform := isuniform;
	
	gizmo := IGizmo_CreateScaleGizmo;
	IGizmo_SetLocation(gizmo, LOCATION_LOCAL);
	if uniform then
		IGizmo_SetAxisMask(gizmo, AXIS_XYZ_SCALE)
end;

procedure TScaleManipulator.Draw;
begin
	inherited;
end;

procedure TScaleManipulator.OnActivate;
begin
end;

procedure TScaleManipulator.OnDeactivate;
begin
end;

procedure TScaleManipulator.OnUpdate(x, y : Longint);
begin
end;

end.
