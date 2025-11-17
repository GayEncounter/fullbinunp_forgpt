unit uEnvZone;

interface
uses vmath, PhysX, Konfig, uSelectable;

type
	TEnvZone = class(ISelectable)
		data : TSection;
		param_tris : TFloatArrayValue;
		
		ph_scene : TPHScene; // TODO use global variable?
		ph : TPHActor;
		ph_meshes : array of TPHTrimesh;
		
		constructor Create(owner : TPHScene; data : TSection);
		destructor Destroy; override;
		
		procedure GetBBox(out b : TAABB); override;
		procedure GetCenter(out c : TVec3);
		
		procedure Transform(const m : TMatrix);
		
		procedure AddLayer(layer : TSection);
		procedure RemoveLayer(const name : String);
		function GetLayer(const name : String) : TSection;
		
		procedure AddQuads(data : array of Single);
		procedure RemoveQuad(id : Longint);
		
		protected
		procedure UpdatePhysics;
		
		function GetName : String;
		procedure SetName(const name : String);
		
		public
		property Name : String read GetName write SetName;
	end;
	
	TEnvZoneArray = array of TEnvZone;
	
	// utils
	function GetCenter(arr : TEnvZoneArray) : TVec3; overload;

implementation
uses Iup, PHGroups, sysutils;

constructor TEnvZone.Create(owner : TPHScene; data : TSection);
begin
	inherited Create;
	
	ph_scene := owner;
	
	self.data := data;
	param_tris := data.GetParam('tris', 'fp32_array') as TFloatArrayValue;
	
	if (param_tris = nil) then
		raise Exception.Create('Invalid env zone description!');
		
	if (Length(param_tris.data) mod (3*6)) <> 0 then
	begin
		IupMessageError(IupGetHandle('MAINDIALOG'), 
		'Invalid env zone description! (Length(param_tris.data) mod (3*6)) <> 0');
	end;
	
	UpdatePhysics;
end;

destructor TEnvZone.Destroy;
var I : Longint;
begin
	data.Free;
	
	PHRemoveActor(ph_scene, ph);
		
	for I := 0 to Length(ph_meshes)-1 do
		PHFreeTrimesh(ph_meshes[I]);
		
	inherited;
end;

procedure TEnvZone.GetBBox(out b : TAABB);
var
	I, npoints : Longint;
	p : TVec3;
begin
	npoints := Length(param_tris.data) div 3;
	
	b.min.x := param_tris.data[0];
	b.min.y := param_tris.data[1];
	b.min.z := param_tris.data[2];
	
	b.max := b.min;
	
	for I := 1 to npoints-1 do
	begin
		p.x := param_tris.data[I*3  ];
		p.y := param_tris.data[I*3+1];
		p.z := param_tris.data[I*3+2];
		AABBMergePoint(b, p);
	end;
end;

procedure TEnvZone.GetCenter(out c : TVec3);
var
	bb : TAABB;
begin
	GetBBox(bb);
	AABBCenter(c, bb);
end;

procedure TEnvZone.Transform(const m : TMatrix);
var
	I : Longint;
	v, c : TVec3;
begin
	GetCenter(c);
	
	for I := 0 to (Length(param_tris.data) div 3) - 1 do
	begin
		v.x := param_tris.data[I*3  ] - c.x;
		v.y := param_tris.data[I*3+1] - c.y;
		v.z := param_tris.data[I*3+2] - c.z;
		
		vmath.Transform(v, m);
		
		param_tris.data[I*3  ] := v.x;// + c.x;
		param_tris.data[I*3+1] := v.y;// + c.y;
		param_tris.data[I*3+2] := v.z;// + c.z;		
	end;
	
	UpdatePhysics;
end;

procedure TEnvZone.AddLayer(layer : TSection);
var
	layers : TSection;
begin
	layers := data.GetSect('layers');
	layers.items.Add(layer);
	
	(layers.GetParam('count', 'u32') as TIntegerValue).num := layers.ParamCount-1;
end;

procedure TEnvZone.RemoveLayer(const name : String);
var
	layers : TSection;
	item : TSection;
begin
	layers := data.GetSect('layers');
	item := layers.GetSect(name, False);
	
	if item <> nil then
	begin
		layers.items.Remove(item);
		item.Free;
	end;
	
	(layers.GetParam('count', 'u32') as TIntegerValue).num := layers.ParamCount-1;
end;

function TEnvZone.GetLayer(const name : String) : TSection;
var
	layers : TSection;
begin
	layers := data.GetSect('layers');
	Result := layers.GetSect(name, False);
end;

procedure TEnvZone.AddQuads(data : array of Single);
var start : Longint;
begin
	start := Length(param_tris.data);
	SetLength(param_tris.data, Length(param_tris.data) + Length(data));
	Move(data[0], param_tris.data[start], Length(data)*Sizeof(Single));
	UpdatePhysics;
end;

procedure TEnvZone.RemoveQuad(id : Longint);
begin
	if id < (Length(param_tris.data) div (3*6)) then
	begin
		Delete(param_tris.data, id*3*6, 3*6);
		UpdatePhysics;
	end;
end;

procedure TEnvZone.UpdatePhysics;
const
	indices : array[1..12] of Longint = (0, 1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 0);
var
	I : Longint;
	nquads : Longint;
	descs : array of Pointer;
begin		
	PHRemoveActor(ph_scene, ph);
	
	for I := 0 to Length(ph_meshes)-1 do
		PHFreeTrimesh(ph_meshes[I]);

	nquads := Length(param_tris.data) div (3*6);
	
	SetLength(descs, nquads);
	SetLength(ph_meshes, nquads);
	
	for I := 0 to nquads-1 do
	begin
		ph_meshes[I] := PHCreateTrimesh(6, 4, @param_tris.data[I*6*3], @indices, Sizeof(TVec3), 12, 0);
		descs[I] := PHShapeTrimesh(ph_meshes[I]);
	end;
	
	ph := PHCreateActor(ph_scene, 1, Length(descs), @descs[0], nil);
	PHSetUserdata(ph, self);
	
	for I := 0 to PHGetShapeCount(ph) - 1 do
	begin
		PHSetShapeUserdata(PHGetShape(ph,I), Pointer(I));
		PHSetGroup(PHGetShape(ph,I), PH_GROUP_ENV_ZONE);
	end;
end;

function TEnvZone.GetName : String;
begin
	GetName := data.GetStr('name');
end;

procedure TEnvZone.SetName(const name : String);
var param_name : TStringValue;
begin
	param_name := data.GetParam('name', 'stringz') as TStringValue;
	param_name.str := name;
end;

function GetCenter(arr : TEnvZoneArray) : TVec3; overload;
var
	c : TVec3;
	bb, bb2 : TAABB;
	I : Longint;
begin
	if Length(arr) > 0 then
	begin
		arr[0].GetBBox(bb);
	
		for I := 1 to Length(arr) - 1 do
		begin
			arr[I].GetBBox(bb2);
			AABBMerge(bb, bb2);
		end;
		
		AABBCenter(c, bb);
	end else
	begin
		c.x := 0;
		c.y := 0;
		c.z := 0;
	end;
		
	GetCenter := c;
end;

end.
