{$POINTERMATH ON}
unit aiMesh;

interface

uses aiTypes, aiMatrix4x4, aiVector3D, aiColor4D;

const
   AI_MAX_NUMBER_OF_COLOR_SETS = $8;
   AI_MAX_NUMBER_OF_TEXTURECOORDS = $8;

type TaiFace = record
   mNumIndicies: cardinal;
   mIndices: PCardinalArray;
end;
type PaiFace = ^TaiFace;
type PaiFaceArray = ^PaiFace;
type PTaiFaceArray = ^TaiFace;

type TaiVertexWeight = record
   mVertexId: cardinal;
   mWeight: single;
end;
type PTaiVertexWeightArray = ^TaiVertexWeight;

type TaiBone = record
   mName: aiString;
   mNumWeights: cardinal;
   mArmature: Pointer;
   mNode: Pointer;
   mWeights: PTaiVertexWeightArray;
   mOffsetMatrix: TaiMatrix4x4;
end;
type PaiBone = ^TaiBone;
type PPaiBoneArray = ^PaiBone;

const
	aiPrimitiveType_POINT       = $1;
	aiPrimitiveType_LINE        = $2;
	aiPrimitiveType_TRIANGLE    = $4;
	aiPrimitiveType_POLYGON     = $8;

type TaiMesh = record
   mPrimitiveTypes: cardinal;
   mNumVertices: cardinal;
   mNumFaces: cardinal;
   mVertices: PTaiVector3DArray;
   mNormals: PTaiVector3DArray;
   mTangents: PTaiVector3DArray;
   mBitangents: PTaiVector3DArray;
   mColors: array[0..AI_MAX_NUMBER_OF_COLOR_SETS-1] of PTaiColor4Darray;
   mTextureCoords: array [0..AI_MAX_NUMBER_OF_TEXTURECOORDS-1] of PTaiVector3DArray;
   mNumUVComponents: array[0..AI_MAX_NUMBER_OF_TEXTURECOORDS-1] of Cardinal;
   mFaces: PTaiFaceArray;
   mNumBones: cardinal;
   mBones: PPaiBoneArray;
   mMaterialIndex: cardinal;
   mName: aiString;
   mNumAniMeshes: cardinal;
   mAniMeshes: pointer;
   mMethod: cardinal;
end;
type PaiMesh = ^TaiMesh;
type PPaiMesh = ^PaiMesh;
type PPaiMeshArray = ^PaiMesh;



implementation

end.
