unit aiLight;

interface
uses aiTypes, aiColor3D, aiVector2D, aiVector3D;

{$Z4}
type TaiLightSourceType = (
	aiLightSource_UNDEFINED			= $00,
	aiLightSource_DIRECTIONAL		= $01,
	aiLightSource_POINT					= $02,
	aiLightSource_SPOT					= $03,
	aiLightSource_AMBIENT				= $04,
	aiLightSource_AREA					= $05
);

type 
	TaiLight = record
		mName: aiString;
		mType: TaiLightSourceType;
		mPosition: TaiVector3D;
		mDirection: TaiVector3D;
		mUp: TaiVector3D;
		mAttenuationConstant: Single;
		mAttenuationLinear: Single;
		mAttenuationQuadratic: Single;
		mColorDiffuse: TaiColor3D;
		mColorSpecular: TaiColor3D;
		mColorAmbient: TaiColor3D;
		mAngleInnerCone: Single;
		mAngleOuterCone: Single;
		mSize: TaiVector2D;
	end;
	PaiLight = ^TaiLight;
	PPaiLightArray = ^PaiLight;

implementation

end.