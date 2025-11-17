{$POINTERMATH ON}
unit aiScene;

interface

uses aiTypes, aiMatrix4x4, aiAnimation, aiLight, aiMesh, aiMaterial, aiTexture;


type
	PaiNode = ^TaiNode;
	PPaiNode = ^PaiNode;
	PPaiNodeArray = ^PaiNode;

	PaiMetadata = Pointer;

	TaiNode = record
		mName: aiString;
		mTransformation: TaiMatrix4x4;
		mParent: PaiNode;
		mNumChildren: cardinal;
		mChildren: PPaiNodeArray;
		mNumMeshes: cardinal;
		mMeshes: PCardinalArray;
		mMetadata: PaiMetadata;
	end;

	TaiScene = record
		mFlags: cardinal;
		mRootNode: PaiNode;
		mNumMeshes: Cardinal;
		mMeshes: PPaiMeshArray;
		mNumMaterials: Cardinal;
		mMaterials: PPaiMaterialArray;
		mNumAnimations: Cardinal;
		mAnimations: PPaiAnimationArray;
		mNumTextures: Cardinal;
		mTextures: PPaiTextureArray;
		mNumLights: Cardinal;
		mLights: PPaiLightArray;
		mNumCameras: Cardinal;
		mCameras: Pointer;
		mMetadata: PaiMetadata;
	 
		mPrivate: Pointer;
	end;
	PaiScene = ^TaiScene;

implementation

end.
