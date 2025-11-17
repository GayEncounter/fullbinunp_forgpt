unit uImport;

interface
uses vmath, aiScene, aiMaterial, aiMesh;

function  GetTextureName(mat : PAIMaterial) : String;
procedure VerifyMesh(mesh : PAIMesh);
function  GetExt(const path : String) : String;
procedure ListImportFormats(out extfilter : String);

// >.<
type
	TConvFlags = set of (cfSwapYZ, cfMirrorZ);

function ImportAIScene(const path : String; out cflags : TConvFlags) : PAIScene;

type
	TImportModel = record
		mesh : Cardinal;
		transform : TMatrix;
	end;
	TImportModelArray = array of TImportModel;
	
procedure CollectModelsForImport(scene : PAIScene; var arr : TImportModelArray);

implementation
uses sysutils, assimp, aiTypes, aiMatrix4x4;

function GetTextureName(mat : PAIMaterial) : String;
var
	path : aiString;
	mapping : TAITextureMapping;
	uvindex : Cardinal;
	blend : Single;
	op : TAITextureOp;
	mapmode : TAITextureMapMode;
	flags : Cardinal;
	
	ret : aiReturn;
	s : String;
	p : Longint;
begin
	ret := aiGetMaterialTexture(mat^, aiTextureType_DIFFUSE, 0, path, mapping, uvindex,
		blend, op, mapmode, flags);
		
	if ret = AI_SUCCESS then
	begin
		s := aiStringToDelphiString(path);
		p := Pos('content\textures\', s);
		if p <> 0 then
			s := Copy(s, p+17, Length(s)-(p+17-1));
			
		Result := ChangeFileExt(s, '');
	end else
		Result := '';
end;

procedure VerifyMesh(mesh : PAIMesh);
begin
	if mesh^.mPrimitiveTypes <> aiPrimitiveType_TRIANGLE then
		raise Exception.Create('Invalid mesh! Must contain only triangle primitives');
	if mesh^.mNumVertices > 65535 then
		raise Exception.Create('Invalid mesh! Has more than 65535 vertices');
	if mesh^.mTextureCoords[0] = nil then
		raise Exception.Create('Invalid mesh! Must contain texcoords');
	if mesh^.mNumUVComponents[0] <> 2 then
		raise Exception.Create('Invalid mesh! Number of UV components must be 2');
	if mesh^.mNormals = nil then
		raise Exception.Create('Invalid mesh! Must contain normals');
	if mesh^.mTangents = nil then
		raise Exception.Create('Invalid mesh! Must contain tangents');
	if mesh^.mBitangents = nil then
		raise Exception.Create('Invalid mesh! Must contain binormals');
end;

function GetExt(const path : String) : String;
begin
	GetExt := LowerCase(ExtractFileExt(path));
end;

procedure ListImportFormats(out extfilter : String);
var
	I : Longint;
	d : PaiImporterDesc;
	s : String;
begin
	extfilter := 'All Files (*.*)|*.*|';
	
	for I := 0 to aiGetImportFormatCount - 1 do
	begin
		d := aiGetImportFormatDescription(I);
		s := '*.' + StringReplace(d.mFileExtensions, ' ', ';*.', [rfReplaceAll]);
		extfilter := extfilter + d.mName + ' (' + s + ')' + '|' + s + '|';
	end;
	
	extfilter := extfilter + 'X-Ray Open Graphics Format (*.ogf)|*.ogf|';
end;

function ImportAIScene(const path : String; out cflags : TConvFlags) : PAIScene;
var
  import_flags : Longword;
  props : PAIPropertyStore;
  scene : PAIScene;

  I : Integer;
  m : PAIMesh;
  
  ext : String;
  flags : TConvFlags;
begin
  import_flags := 
    aiProcess_CalcTangentSpace or
    aiProcess_Triangulate or
    //aiProcess_PreTransformVertices or
    aiProcess_SplitLargeMeshes or
    aiProcess_FlipUVs;
    
  props := aiCreatePropertyStore;
  try
	  aiSetImportPropertyInteger(props, AI_CONFIG_PP_SLM_VERTEX_LIMIT, 65535);
	  aiSetImportPropertyInteger(props, AI_CONFIG_PP_SLM_TRIANGLE_LIMIT, 100000); // set to 65535/3 ?
	  
	  flags := [];
	  ext := GetExt(path);
	  
	  if ext = '.blend' then
	  	Include(flags, cfSwapYZ);
	  if (ext = '.3ds') or (ext = '.obj') or (ext = '.fbx') then
	  	Include(flags, cfMirrorZ);
	  	
	  cflags := flags;
	
	  scene := aiImportFileExWithProperties(PAnsiChar(path), import_flags, nil, props);
	  if scene <> nil then
	  begin
	    for I := 0 to scene^.mNumMeshes - 1 do
	    begin
	      m := scene^.mMeshes[I];
	      WriteLn('mesh #', I, ' vertex count: ', m^.mNumVertices, ' face count: ', m^.mNumFaces);
	    end;    
	  
	    Result := scene;
	  end else
	    raise Exception.Create('Import failed. ' + aiGetErrorString());
  finally
  	aiReleasePropertyStore(props);
  end;
end;

procedure _CollectModelNodes(node : PAINode; var arr : TImportModelArray);
var
	I, len : Longint;
	transform : TAIMatrix4x4;
begin
	if node^.mNumMeshes > 0 then
	begin
		len := Length(arr);
		SetLength(arr, len+node^.mNumMeshes);
		
		for I := 0 to node^.mNumMeshes-1 do
		begin
			arr[len+I].mesh := node^.mMeshes[I];
			transform := node^.mTransformation;
			aiTransposeMatrix4(transform);
			Move(transform, arr[len+I].transform, Sizeof(TMatrix));
		end;
	end;
	
	for I := 0 to node^.mNumChildren - 1 do
		_CollectModelNodes(node^.mChildren[I], arr);
end;

procedure CollectModelsForImport(scene : PAIScene; var arr : TImportModelArray);
begin
	SetLength(arr, 0);
	_CollectModelNodes(scene^.mRootNode, arr);
end;

end.