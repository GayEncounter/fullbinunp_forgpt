unit assimp;

interface

uses aiTypes, aiMatrix4x4, aiMatrix3x3, aiMesh, aiScene, aiMaterial, aiColor4d, aiVector3D;

const ASSIMP_DLL = 'assimp-vc120-mt.dll';
//const ASSIMP_DLL = 'libassimp.dll';

type
  TaiExportFormatDesc = record
    id            : PAnsiChar;
    description   : PAnsiChar;
    fileExtension : PAnsiChar;
  end;
  PaiExportFormatDesc = ^TaiExportFormatDesc;
  
function  aiGetExportFormatCount : Cardinal; cdecl; external ASSIMP_DLL;
function  aiGetExportFormatDescription(index : Cardinal) : PaiExportFormatDesc; cdecl; external ASSIMP_DLL; 
procedure aiReleaseExportFormatDescription(desc : PaiExportFormatDesc) cdecl; external ASSIMP_DLL;    

type
	TaiImporterDesc = record
		mName           : PAnsiChar;
		mAuthor         : PAnsiChar;
		mMaintainer     : PAnsiChar;
		mComments       : PAnsiChar;
		mFlags          : Cardinal;
		mMinMajor       : Cardinal;
		mMinMinor       : Cardinal;
		mMaxMajor       : Cardinal;
		mMaxMinor       : Cardinal;		
		mFileExtensions : PAnsiChar;
	end;
	PaiImporterDesc = ^TaiImporterDesc;
	
function  aiGetImportFormatCount : Cardinal; cdecl; external ASSIMP_DLL;
function  aiGetImportFormatDescription(index : Cardinal) : PaiImporterDesc;  cdecl; external ASSIMP_DLL;
function  aiGetImporterDesc(extension : PAnsiChar) : PaiImporterDesc;  cdecl; external ASSIMP_DLL;

// aiPropertyStore
type
	TaiPropertyStore = record
		s : AnsiChar;
	end;
	PaiPropertyStore = ^TaiPropertyStore;

const
	AI_CONFIG_PP_SLM_TRIANGLE_LIMIT = 'PP_SLM_TRIANGLE_LIMIT';
	AI_CONFIG_PP_SLM_VERTEX_LIMIT   = 'PP_SLM_VERTEX_LIMIT';

function  aiCreatePropertyStore : PaiPropertyStore; cdecl; external ASSIMP_DLL;
procedure aiReleasePropertyStore(p : PaiPropertyStore); cdecl; external ASSIMP_DLL;

procedure aiSetImportPropertyInteger(p : PaiPropertyStore; name : PAnsiChar; value : Longint); cdecl; external ASSIMP_DLL;
procedure aiSetImportPropertyFloat(p : PaiPropertyStore; name : PAnsiChar; value : Single); cdecl; external ASSIMP_DLL;
procedure aiSetImportPropertyString(p : PaiPropertyStore; name : PAnsiChar; st : PaiString); cdecl; external ASSIMP_DLL;
  
const
	aiProcess_CalcTangentSpace			= $00000001;
	aiProcess_JoinIdenticalVertices	= $00000002;
	aiProcess_MakeLeftHanded				= $00000004;
	aiProcess_Triangulate						= $00000008;
	aiProcess_RemoveComponent				= $00000010;
	aiProcess_GenNormals						= $00000020;
	aiProcess_GenSmoothNormals			= $00000040;
	aiProcess_SplitLargeMeshes			= $00000080;
	aiProcess_PreTransformVertices	= $00000100;
	aiProcess_LimitBoneWeights			= $00000200;
	aiProcess_ValidateDataStructure	= $00000400;
	aiProcess_ImproveCacheLocality	= $00000800;
	aiProcess_RemoveRedundantMaterials = $00001000;
	aiProcess_FixInfacingNormals		= $00002000;
	aiProcess_SortByPType						= $00008000;
	aiProcess_FindDegenerates				= $00010000;
	aiProcess_FindInvalidData				= $00020000;
	aiProcess_GenUVCoords						= $00040000;
	aiProcess_TransformUVCoords			= $00080000;
	aiProcess_FindInstances					= $00100000;
	aiProcess_OptimizeMeshes				= $00200000;
	aiProcess_OptimizeGraph					= $00400000;
	aiProcess_FlipUVs								= $00800000;
	aiProcess_FlipWindingOrder			= $01000000;
	aiProcess_SplitByBoneCount			= $02000000;
	aiProcess_Debone								= $04000000;
	aiProcess_GlobalScale						= $08000000;
	aiProcess_EmbedTextures					= $10000000;

procedure aiCopyScene(pIn : PaiScene; pOut : Pointer); cdecl; external ASSIMP_DLL; 

function  aiExportScene(pScene : PaiScene; pFormatId : PAnsiChar; pFileName : PAnsiChar; pPreprocessing : Cardinal) : Cardinal; cdecl; external ASSIMP_DLL; 

function  aiImportFile(filename: pchar; pFlags: Cardinal): PaiScene; cdecl; external ASSIMP_DLL;
function  aiImportFileEx(filename : PAnsiChar; pFlags : Cardinal; pFS : Pointer) : PaiScene; cdecl; external ASSIMP_DLL;
function  aiImportFileExWithProperties(filename : PAnsiChar; pFlags : Cardinal; pFS : Pointer; pProps : PaiPropertyStore) : PaiScene; cdecl; external ASSIMP_DLL;
procedure aiReleaseImport( pScene: pointer); cdecl; external ASSIMP_DLL;
function  aiGetErrorString(): PChar; cdecl; external ASSIMP_DLL;
function  aiIsExtensionSupported(extension : PAnsiChar) : Longint; cdecl; external ASSIMP_DLL;

//procedure aiDecomposeMatrix( var mat: TaiMatrix4x4; var scaling: TaiVector3D; var rotation: TaiQuaternion; var position: TaiVector3D); cdecl; external ASSIMP_DLL;
procedure aiTransposeMatrix4( var mat: TaiMatrix4x4); cdecl; external ASSIMP_DLL;
procedure aiTransposeMatrix3( var mat: TaiMatrix3x3); cdecl; external ASSIMP_DLL;
procedure aiTransformVecByMatrix3( var vec: TaiVector3D; var mat: TaiMatrix3x3); cdecl; external ASSIMP_DLL;
procedure aiTransformVecByMatrix4( var vec: TaiVector3D; var mat: TaiMatrix4x4); cdecl; external ASSIMP_DLL;

procedure aiMultiplyMatrix4(var dst: TaiMatrix4x4; var src: TaiMatrix4x4); cdecl; external ASSIMP_DLL;
procedure aiMultiplyMatrix3(var dst: TaiMatrix3x3; var src: TaiMatrix3x3); cdecl; external ASSIMP_DLL;


procedure aiIdentityMatrix3(var mat: TaiMatrix3x3); cdecl; external ASSIMP_DLL;
procedure aiIdentityMatrix4(var mat: TaiMatrix4x4); cdecl; external ASSIMP_DLL;


//----- from aiMaterial.h
function aiGetMaterialProperty( pMat: PaiMaterial; pKey: PChar; nType: Cardinal; nIndex: Cardinal; pPropOut: pointer): aiReturn; cdecl; external ASSIMP_DLL;
function aiGetMaterialFloatArray( var pMat: TaiMaterial; pKey: PChar; nType: Cardinal; nIndex: Cardinal; var pOut: Single; var pMax: Cardinal): aiReturn; cdecl; external ASSIMP_DLL;
function aiGetMaterialFloat( var pMat: TaiMaterial; pKey: PChar; nType: Cardinal; nIndex: Cardinal; var pOut: Single): aiReturn;
function aiGetMaterialIntegerArray(var pMat: TaiMaterial; pKey: PChar; nType: Cardinal; nIndex: Cardinal; var pOut: Integer; var pMax: Cardinal): aiReturn; cdecl; external ASSIMP_DLL;
function aiGetMaterialInteger(var pMat: TaiMaterial; pKey: PChar; nType: Cardinal; nIndex: Cardinal; var pOut: Integer): aiReturn;
function aiGetMaterialColor(var pMat: TaiMaterial; pKey: PChar; nType: Cardinal; nIndex: Cardinal; var pOut: TaiColor4d): aiReturn; cdecl; external ASSIMP_DLL;
function aiGetMaterialString(var pMat: TaiMaterial; pKey: PChar; nType: Cardinal; nIndex: Cardinal; var pOut: aiString): aiReturn; cdecl; external ASSIMP_DLL;
function aiGetMaterialTextureCount(var pMat: TaiMaterial; nType: TaiTextureType): Cardinal; cdecl; external ASSIMP_DLL;
function aiGetMaterialTexture(var mat: TaiMaterial; nType: TaiTextureType; nIndex: Cardinal; var path: aiString; var mapping: TaiTextureMapping; var uvindex: Cardinal; var blend: single; var op: TaiTextureOp; var mapmode: TaiTextureMapMode; var flags: Cardinal): aiReturn; cdecl; external ASSIMP_DLL;

implementation

function aiGetMaterialFloat( var pMat: TaiMaterial; pKey: PChar; nType: Cardinal; nIndex: Cardinal; var pOut: Single): aiReturn;
var
   n: cardinal;
begin
   n := 0;
   result := aiGetMaterialFloatArray( pMat, pKey, nType, nIndex, pOut, n);
end;

function aiGetMaterialInteger(var pMat: TaiMaterial; pKey: PChar; nType: Cardinal; nIndex: Cardinal; var pOut: integer): aiReturn;
var
   n: cardinal;
begin
   n := 0;
   result := aiGetMaterialIntegerArray( pMat, pKey, nType, nIndex, pOut, n);
end;

end.
