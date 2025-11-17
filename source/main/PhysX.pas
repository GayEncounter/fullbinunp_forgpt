unit PhysX;

interface

const
	PH_DLL = 'PhysXHelper.dll';

const
	NX_MF_FLIPNORMALS		 = $1;
	NX_MF_16_BIT_INDICES = $2;
	NX_MF_HARDWARE_MESH	 = $4;
	
const
	NX_PLATFORM_PC           = 0;
	NX_PLATFORM_XENON        = 1;
	NX_PLATFORM_PLAYSTATION3 = 2;
	NX_PLATFORM_GAMECUBE     = 3;
	NX_PLATFORM_WII          = 4;
	
type
	_PHScene = record end;
	_PHActor = record end;
	_PHShape = record end;
	_PHTriMesh = record end;
	
	TPHScene = ^_PHScene;
	TPHActor = ^_PHActor;
	TPHShape = ^_PHShape;
	TPHTriMesh = ^_PHTriMesh;

// cooking
function  InitCooking : Longint; stdcall; external PH_DLL;
function  CloseCooking : Longint; stdcall; external PH_DLL;
function  CookTriangleMesh(
	numVertices, numTriangles : Longint; points, tris : Pointer;
	pointstride, trianglestride : Longint; flags : Longint;
	data : Pointer; size : PLongword) : Integer; stdcall; external PH_DLL;
	
function  SetCookingParams(platform : Longint; skinWidth : Single; hintCollisionSpeed : Longint) : Longint; stdcall; external PH_DLL;
procedure GetCookingParams(platform : PLongint; skinWidth : PSingle; hintCollisionSpeed : PLongint); stdcall; external PH_DLL;

function  InitCooking3 : Longint; stdcall; external PH_DLL;
function  CloseCooking3 : Longint; stdcall; external PH_DLL;

function  CookTriangleMesh3(
	numVertices, numTriangles : Longint; points, tris : Pointer;
	pointstride, trianglestride : Longint; flags : Longint;
	data : Pointer; size : PLongword) : Integer; stdcall; external PH_DLL;

// physics SDK
function  PHInitialize : Longint; stdcall; external PH_DLL;
procedure PHFinalize; stdcall; external PH_DLL;

function  PHCreateTriMesh(
	numVertices, numTriangles : Longint; points, tris : Pointer;
	pointstride, trianglestride : Longint; flags : Longint) : TPHTriMesh; stdcall; external PH_DLL;
function  PHLoadTriMesh(data : Pointer; size : Longint) : TPHTriMesh; stdcall; external PH_DLL;
procedure PHFreeTriMesh(mesh : TPHTriMesh); stdcall; external PH_DLL;

// scene
function  PHCreateScene : TPHScene; stdcall; external PH_DLL;
procedure PHDestroyScene(scene : TPHScene); stdcall; external PH_DLL;

function  PHCullShapes(scene : TPHScene; num_planes : Longint; planes : PSingle; num_shapes : Longint; shapes : PPointer; group : Longint = -1) : Longint; stdcall; external PH_DLL;

function  PHRaycastAnyShape(scene : TPHScene; p, dir : PSingle; dist : Single; group : Longint = -1) : Longint; stdcall; external PH_DLL;
function  PHRaycastClosestShape(scene : TPHScene; p, dir : PSingle; dist : Single; hit_pos : PSingle = nil; hit_nrm : PSingle = nil; uv : PSingle = nil; group : Longint = -1) : TPHShape; stdcall; external PH_DLL;
function  PHRaycastClosestActor(scene : TPHScene; p, dir : PSingle; dist : Single; hit_pos : PSingle = nil; hit_nrm : PSingle = nil; uv : PSingle = nil; group : Longint = -1) : TPHActor; stdcall; external PH_DLL;

// shape descs
function  PHShapeBox(dim : PSingle; offset : PSingle) : Pointer; stdcall; external PH_DLL;
function  PHShapeBoxM44(dim : PSingle; mat : PSingle) : Pointer; stdcall; external PH_DLL;

function  PHShapeSphere(r : Single; offset : PSingle) : Pointer; stdcall; external PH_DLL;
function  PHShapeSphereM44(dim : PSingle; mat : PSingle) : Pointer; stdcall; external PH_DLL;

function  PHShapeTrimesh(m : TPHTriMesh; scale : PSingle = nil; offset : PSingle = nil) : Pointer; stdcall; external PH_DLL;
function  PHShapeTrimeshM44(m : TPHTriMesh; scale : PSingle = nil; mat : PSingle = nil) : Pointer; stdcall; external PH_DLL;

// actor
function  PHCreateActor(scene : TPHScene; isdynamic : Longint; cnt : Longint; shapes : PPointer; mat : PSingle) : TPHActor; stdcall; external PH_DLL;
procedure PHRemoveActor(scene : TPHScene; actor : TPHActor); stdcall; external PH_DLL;

procedure PHSetGlobalPoseM44(actor : TPHActor; m : PSingle); stdcall; external PH_DLL;

function  PHSetUserdata(actor : TPHActor; userdata : Pointer) : Pointer; stdcall; external PH_DLL;
function  PHGetUserdata(actor : TPHActor) : Pointer; stdcall; external PH_DLL;

function  PHGetShapeCount(actor : TPHActor) : Longint; stdcall; external PH_DLL;
function  PHGetShape(actor : TPHActor; id : Longint) : TPHShape; stdcall; external PH_DLL; 

// real shapes
function  PHAddShape(actor : TPHActor; desc : Pointer) : TPHShape; stdcall; external PH_DLL;
procedure PHRemoveShape(shape : TPHShape); stdcall; external PH_DLL;

function  PHGetActor(shape : TPHShape) : TPHActor; stdcall; external PH_DLL;

function  PHSetShapeUserdata(shape : TPHShape; userdata : Pointer) : Pointer; stdcall; external PH_DLL;
function  PHGetShapeUserdata(shape : TPHShape) : Pointer; stdcall; external PH_DLL;

procedure PHSetGroup(shape : TPHShape; group : Longword); stdcall; external PH_DLL;
function  PHGetGroup(shape : TPHShape) : Longword; stdcall; external PH_DLL;

function  PHIsBoxShape(shape : TPHShape) : Longint; stdcall; external PH_DLL;
function  PHIsSphereShape(shape : TPHShape) : Longint; stdcall; external PH_DLL;
function  PHIsTrimeshShape(shape : TPHShape) : Longint; stdcall; external PH_DLL;

procedure PHSetBoxDim(shape : TPHShape; dim : PSingle); stdcall; external PH_DLL;
procedure PHSetSphereRadius(shape : TPHShape; radius : Single); stdcall; external PH_DLL;
procedure PHSetMeshScale(shape : TPHShape; scale : PSingle); stdcall; external PH_DLL;

implementation

end.
