unit uExportScene;

interface
uses classes, fouramdl, skeleton, vmath, aiScene, aiMaterial, aiMesh, aiLight;

type
	TExportMaterial = class
		id : Cardinal;
		material : TAIMaterial;
		
		destructor Destroy; override;
	end;
	
	TExportModel = class
		meshes : array of TAIMesh;
		
		start, count : Longint; // coordinates in aiScene structure :)
		
		destructor Destroy; override;
	end;
	
	TExportObject = class
		node : TAINode;
		model_id : Longint;
	
		destructor Destroy; override;
		
		protected
		procedure FreeNode(n : PAINode);
	end;
	
	TExportLight = class
		//light : TAILight;
		name : String;
		position : TVec3;
		rotation : TVec3;
		ltype : Shortint;
		color : TVec4;
		brightness : Single;
		angle : Single;
		range : Single;
	end;

	TExportScene = class
		materials : TStringList;
		models : TStringList;
		objects : TStringList;
		lights : TStringList;
	
		constructor Create;
		destructor Destroy; override;
		
		function AddMaterial(const texture_name : String) : Longint;
		function AddModel(const name : String; model : T4AModelSkeleton; mtlset : Longint = -1; useSkeleton : Boolean = False) : Longint; overload;
		function AddModel(const name : String; model : T4AModelHierrarhy; mtlset : Longint = -1) : Longint; overload;
		function AddLevel(const name : String; level : T4ALevel) : Longint;
		function AddObject(const name : String; const transform : TMatrix; model_id : Longint) : Longint;
		function AddLight(const name : String; const matrix : TMatrix; ltype : Shortint; const color : TVec4; brightness, range, angle : Single) : Longint; 
		procedure AddSkeleton(s : T4ASkeleton);
	
		procedure DoExport(const format, filename : String);
		
		protected
		function AddBone(s : T4ASkeleton; b : P4ABone) : PAINode;
		procedure SetupBones(s : T4ASkeleton; m : T4AModelSkinnedMesh; d : PAIMesh);
	end;

implementation
uses sysutils, Texture, texturePrefs, Math,
  Engine, assimp, aiTypes, aiVector3D, aiColor4D;

destructor TExportModel.Destroy;
var
	I, J : Longint;
begin
	for I := 0 to Length(meshes) - 1 do
	begin
		FreeMem(meshes[I].mVertices);
		FreeMem(meshes[I].mNormals);
		FreeMem(meshes[I].mTextureCoords[0]);
		FreeMem(meshes[I].mColors[0]);
		FreeMem(meshes[I].mFaces[0].mIndices);
		FreeMem(meshes[I].mFaces);
		
		for J := 0 to meshes[I].mNumBones-1 do
		begin
			FreeMem(meshes[I].mBones[J].mWeights);
			Dispose(meshes[I].mBones[J]);
		end;
			
		FreeMem(meshes[I].mBones);
	end;
end;

destructor TExportMaterial.Destroy;
var
	I : Longint;
begin
	for I := 0 to material.mNumProperties - 1 do
	begin
		FreeMem(material.mProperties[I].mData);
		Dispose(material.mProperties[I]);
	end;
	
	FreeMem(material.mProperties);

	inherited
end;

destructor TExportObject.Destroy;
begin
	FreeNode(@node);
	inherited
end;

procedure TExportObject.FreeNode(n : PAINode);
var
	I : Longint;
begin
	for I := 0 to n^.mNumChildren - 1 do
	begin
		FreeNode(n^.mChildren[I]);
		Dispose(n^.mChildren[I]);
	end;
	
	FreeMem(n^.mChildren);
	FreeMem(n^.mMeshes);
end;

constructor TExportScene.Create;
begin
	inherited;
	
	materials := TStringList.Create;
	models := TStringList.Create;
	objects := TStringList.Create;
	lights := TStringList.Create;
	
	materials.OwnsObjects := True;
	models.OwnsObjects := True;
	objects.OwnsObjects := True;
	lights.OwnsObjects := True;
end;

destructor TExportScene.Destroy;
begin
	materials.Free;
	models.Free;
	objects.Free;
	lights.Free;

	inherited
end;

function TExportScene.AddMaterial(const texture_name : String) : Longint;
var
	mtl : TExportMaterial;
	id : Longint;
	
	prop, tex, tflags : PAIMaterialProperty;
	pprop : PPAIMaterialPropertyArray;
	
	tex_name, tex_path : String;
begin

	// get real texture name
	tex_name := GetRealTextureName(texture_name);
	
	id := materials.IndexOf(tex_name);
	if id = -1 then
	begin
		New(prop);
		FillChar(prop^, Sizeof(TAIMaterialProperty), #0);
		
		aiStringFromDelphiString(prop.mKey, AI_MATKEY_NAME);
		prop.mSemantic := 0;//Cardinal(aiTextureType_Diffuse);
		prop.mIndex := 0;
		prop.mDataLength := 4+Length(tex_name)+1;
		prop.mType := aiPTI_String;
		GetMem(prop.mData, 4+Length(tex_name)+1);
		PCardinal(prop.mData)^ := Length(tex_name);
		StrPCopy(prop.mData+4, tex_name);
		
		tex_path := ResourcesPath + '\textures\' + tex_name + '.dds';
		New(tex);
		FillChar(tex^, Sizeof(TAIMaterialProperty), #0);
		
		aiStringFromDelphiString(tex.mKey, _AI_MATKEY_TEXTURE_BASE);
		tex.mSemantic := Cardinal(aiTextureType_Diffuse);
		tex.mIndex := 0;
		tex.mDataLength := 4+Length(tex_path)+1;
		tex.mType := aiPTI_String;
		GetMem(tex.mData, 4+Length(tex_path)+1);
		PCardinal(tex.mData)^ := Length(tex_path);
		StrPCopy(tex.mData+4, tex_path);
		
		New(tflags);
		FillChar(tflags^, Sizeof(TAIMaterialProperty), #0);

		aiStringFromDelphiString(tflags.mKey, _AI_MATKEY_TEXFLAGS_BASE);
		tflags.mSemantic := Cardinal(aiTextureType_Diffuse);
		tflags.mIndex := 0;
		tflags.mDataLength := 4;
		tflags.mType := aiPTI_Integer;
		GetMem(tflags.mData, 4);
		PCardinal(tflags.mData)^ := Cardinal(aiTextureFlags_IgnoreAlpha);	
		
		
		GetMem(pprop, 3*Sizeof(Pointer));
		pprop[0] := prop;
		pprop[1] := tex;
		pprop[2] := tflags;
		
		id := materials.Add(tex_name);
		
		mtl := TExportMaterial.Create;
		mtl.id := id;
		
		mtl.material.mProperties := PPAIMaterialPropertyArray(pprop);
		mtl.material.mNumProperties := 3;
		mtl.material.mNumAllocated := 3;
		
		materials.Objects[id] := mtl;
	end;
	
	AddMaterial := id;
end;

function TExportScene.AddModel(const name : String; model : T4AModelSkeleton; mtlset : Longint; useSkeleton : Boolean) : Longint;
var
	idx : Longint;
	I, J : Longint;
	
	export_model : TExportModel;
	mesh : PAIMesh;
	
	srcmeshes : TList;
	m : T4AModelSkinnedMesh;
	
	scale : Single;
	normal : TVec3;
	ao : Single;
	
	texture, shader : String;
begin
	idx := models.IndexOf(name);
	if idx <> -1 then
	begin
		Result := idx;
		Exit;
	end;
		
	srcmeshes := TList.Create;
	for I := 0 to Length(model.meshes[0]) - 1 do
		for J := 0 to Length(model.meshes[0,I].meshes) - 1 do
			srcmeshes.Add(model.meshes[0,I].meshes[J]);
	
	if srcmeshes.Count < 1 then
	begin
		WriteLn('Warning! Empty model [', name, ']');
		srcmeshes.Free;
		Result := -1;
		Exit;
	end;
		
	WriteLn('AddModel ', name);
		
	export_model := TExportModel.Create;
	SetLength(export_model.meshes, srcmeshes.Count);
	
	for I := 0 to srcmeshes.Count - 1 do
	begin
		m := T4AModelSkinnedMesh(srcmeshes[I]); 	
		scale := m.GetPointScale;
		
		mesh := @export_model.meshes[I];
		
		aiStringFromDelphiString(mesh.mName, name);
		mesh.mPrimitiveTypes := aiPrimitiveType_Triangle;
		mesh.mNumVertices := Length(m.vertices);
		mesh.mNumFaces := Length(m.indices) div 3;
		
		GetMem(mesh.mVertices, (mesh^.mNumVertices * Sizeof(TAIVector3D)));
		GetMem(mesh.mNormals, (mesh^.mNumVertices * Sizeof(TAIVector3D)));
		GetMem(mesh.mTextureCoords[0], (mesh^.mNumVertices * Sizeof(TAIVector3D)));
		GetMem(mesh.mColors[0], (mesh^.mNumVertices * Sizeof(TAIColor4D)));
		mesh.mNumUVComponents[0] := 2;
		
		for J := 0 to Length(m.vertices) - 1 do
		begin
			mesh.mVertices[J].x := m.vertices[J].point.x * scale;
			mesh.mVertices[J].y := m.vertices[J].point.y * scale;
			mesh.mVertices[J].z := m.vertices[J].point.z * scale;
			
			UnpackNormal(normal, m.vertices[J].normal);
			mesh.mNormals[J].x := normal.x;
			mesh.mNormals[J].y := normal.y;
			mesh.mNormals[J].z := normal.z;
			
			mesh.mTextureCoords[0][J].x := m.vertices[J].tc.x / 2048;
			mesh.mTextureCoords[0][J].y := m.vertices[J].tc.y / 2048;
			mesh.mTextureCoords[0][J].z := 0;
			
			ao := ((m.vertices[J].normal and $FF000000) shr 24) / 255;
			with mesh.mColors[0][J] do
			begin
				r := ao;
				g := ao;
				b := ao;
				a := 1.0;
			end;
		end;
		
		GetMem(mesh^.mFaces, (mesh^.mNumFaces * Sizeof(TAIFace)));
		GetMem(mesh^.mFaces[0].mIndices, mesh^.mNumFaces * Sizeof(Cardinal) * 3);
		
		for J := 0 to mesh^.mNumFaces - 1 do
		begin
			with mesh.mFaces[J] do
			begin
				mNumIndicies := 3;
				mIndices := PCardinalArray(@mesh.mFaces[0].mIndices[J*3]);
				
				mIndices[0] := m.indices[J*3  ];
				mIndices[1] := m.indices[J*3+1];
				mIndices[2] := m.indices[J*3+2];
			end;
		end;
		
		model.GetMaterialOverride(mtlset, m.name, texture, shader);
		if texture = '' then
			texture := m.texture;
			
		texture := model.GetTextureSubst(texture);
		mesh.mMaterialIndex := AddMaterial(texture);
		
		if useSkeleton and Assigned(model.skeleton) then
			SetupBones(model.skeleton, m, mesh);
	end;
	
	srcmeshes.Free;
	
	idx := models.Add(name);
	models.Objects[idx] := export_model;
	
	Result := idx;
end;

function TExportScene.AddModel(const name : String; model : T4AModelHierrarhy; mtlset : Longint) : Longint; overload;
var
	idx : Longint;
	I, J : Longint;
	
	export_model : TExportModel;
	mesh : PAIMesh;
	m : T4AModelSimple;
	
	normal : TVec3;
	ao : Single;
	
	texture, shader : String;
begin
	idx := models.IndexOf(name);
	if idx <> -1 then
	begin
		Result := idx;
		Exit;
	end;
		
	WriteLn('AddModel ', name);
	
	if Length(model.meshes) < 1 then
	begin
		WriteLn('Warning! Empty model [', name, ']');
		Result := -1;
		Exit;
	end;
		
	export_model := TExportModel.Create;
	SetLength(export_model.meshes, Length(model.meshes));
	
	for I := 0 to Length(model.meshes) - 1 do
	begin
		m := model.meshes[I];
		mesh := @export_model.meshes[I];
		
		aiStringFromDelphiString(mesh.mName, name);
		mesh.mPrimitiveTypes := aiPrimitiveType_Triangle;
		mesh.mNumVertices := Length(m.vertices);
		mesh.mNumFaces := Length(m.indices) div 3;
		
		GetMem(mesh.mVertices, (mesh^.mNumVertices * Sizeof(TAIVector3D)));
		GetMem(mesh.mNormals, (mesh^.mNumVertices * Sizeof(TAIVector3D)));
		GetMem(mesh.mTextureCoords[0], (mesh^.mNumVertices * Sizeof(TAIVector3D)));
		GetMem(mesh.mColors[0], (mesh^.mNumVertices * Sizeof(TAIColor4D)));
		mesh.mNumUVComponents[0] := 2;
		
		for J := 0 to Length(m.vertices) - 1 do
		begin
			mesh.mVertices[J].x := m.vertices[J].point.x;
			mesh.mVertices[J].y := m.vertices[J].point.y;
			mesh.mVertices[J].z := m.vertices[J].point.z;
			
			UnpackNormal(normal, m.vertices[J].normal);
			mesh.mNormals[J].x := normal.x;
			mesh.mNormals[J].y := normal.y;
			mesh.mNormals[J].z := normal.z;
			
			mesh.mTextureCoords[0][J].x := m.vertices[J].tc.x;
			mesh.mTextureCoords[0][J].y := m.vertices[J].tc.y;
			mesh.mTextureCoords[0][J].z := 0;
			
			ao := ((m.vertices[J].normal and $FF000000) shr 24) / 255;
			with mesh.mColors[0][J] do
			begin
				r := ao;
				g := ao;
				b := ao;
				a := 1.0;
			end;
		end;
		
		GetMem(mesh^.mFaces, (mesh^.mNumFaces * Sizeof(TAIFace)));
		GetMem(mesh^.mFaces[0].mIndices, mesh^.mNumFaces * Sizeof(Cardinal) * 3);
		
		for J := 0 to mesh^.mNumFaces - 1 do
		begin
			with mesh.mFaces[J] do
			begin
				mNumIndicies := 3;
				mIndices := PCardinalArray(@mesh.mFaces[0].mIndices[J*3]);
				
				mIndices[0] := m.indices[J*3  ];
				mIndices[1] := m.indices[J*3+1];
				mIndices[2] := m.indices[J*3+2];
			end;
		end;
		
		model.GetMaterialOverride(mtlset, m.name, texture, shader);
		if texture = '' then
			texture := m.texture;
		
		mesh.mMaterialIndex := AddMaterial(texture);
	end;
	
	idx := models.Add(name);
	models.Objects[idx] := export_model;
	
	Result := idx;
end;

function TExportScene.AddLevel(const name : String; level : T4ALevel) : Longint;
var
	idx : Longint;
	I, J : Longint;
	
	export_model : TExportModel;
	mesh : PAIMesh;
	
	srcmeshes : TList;
	m : T4AModelRef;

	v : P4AVertLevel;	
	normal : TVec3;
	ao : Single;
begin
	idx := models.IndexOf(name);
	if idx <> -1 then
	begin
		Result := idx;
		Exit;
	end;
		
	srcmeshes := TList.Create;
	for I := 0 to Length(level.visuals) - 1 do
		if level.visuals[I] is T4AModelRef then
			srcmeshes.Add(level.visuals[I]);
	
	if srcmeshes.Count < 1 then
	begin
		WriteLn('Warning! Empty model [', name, ']');
		srcmeshes.Free;
		Result := -1;
		Exit;
	end;
		
	WriteLn('AddModel ', name);
		
	export_model := TExportModel.Create;
	SetLength(export_model.meshes, srcmeshes.Count);
	
	for I := 0 to srcmeshes.Count - 1 do
	begin
		m := T4AModelRef(srcmeshes[I]);
		mesh := @export_model.meshes[I];
		
		aiStringFromDelphiString(mesh.mName, name);
		mesh.mPrimitiveTypes := aiPrimitiveType_Triangle;
		mesh.mNumVertices := m.vertexcount;
		mesh.mNumFaces := m.indexcount div 3;
		
		GetMem(mesh.mVertices, (mesh^.mNumVertices * Sizeof(TAIVector3D)));
		GetMem(mesh.mNormals, (mesh^.mNumVertices * Sizeof(TAIVector3D)));
		GetMem(mesh.mTextureCoords[0], (mesh^.mNumVertices * Sizeof(TAIVector3D)));
		GetMem(mesh.mColors[0], (mesh^.mNumVertices * Sizeof(TAIColor4D)));
		mesh.mNumUVComponents[0] := 2;
		
		for J := 0 to m.vertexcount - 1 do
		begin
			v := @level.vbuffer[m.vertexoffset+J];
			mesh.mVertices[J].x := v.point.x;
			mesh.mVertices[J].y := v.point.y;
			mesh.mVertices[J].z := v.point.z;
			
			UnpackNormal(normal, v.normal);
			mesh.mNormals[J].x := normal.x;
			mesh.mNormals[J].y := normal.y;
			mesh.mNormals[J].z := normal.z;
			
			mesh.mTextureCoords[0][J].x := v.tc.x / 1024;
			mesh.mTextureCoords[0][J].y := v.tc.y / 1024;
			mesh.mTextureCoords[0][J].z := 0;
			
			ao := ((v.normal and $FF000000) shr 24) / 255;
			with mesh.mColors[0][J] do
			begin
				r := ao;
				g := ao;
				b := ao;
				a := 1.0;
			end;
		end;
		
		GetMem(mesh^.mFaces, (mesh^.mNumFaces * Sizeof(TAIFace)));
		GetMem(mesh^.mFaces[0].mIndices, mesh^.mNumFaces * Sizeof(Cardinal) * 3);
		
		for J := 0 to mesh^.mNumFaces - 1 do
		begin
			with mesh.mFaces[J] do
			begin
				mNumIndicies := 3;
				mIndices := PCardinalArray(@mesh.mFaces[0].mIndices[J*3]);
				
				mIndices[0] := level.ibuffer[m.indexoffset+J*3  ];
				mIndices[1] := level.ibuffer[m.indexoffset+J*3+1];
				mIndices[2] := level.ibuffer[m.indexoffset+J*3+2];
			end;
		end;
		
		mesh.mMaterialIndex := AddMaterial(level.materials[m.shaderid].texture);
	end;
	
	srcmeshes.Free;

	idx := models.Add(name);
	models.Objects[idx] := export_model;	

	Result := idx;
end;

procedure TExportScene.SetupBones(s : T4ASkeleton; m : T4AModelSkinnedMesh; d : PAIMesh);
var
	I, J, K, L : Longint;
	b : PAIBone;
	
	mat : TMatrix;
	
	w : array of TAIVertexWeight;
begin
	d.mNumBones := Length(s.bones);
	GetMem(d.mBones, d.mNumBones * Sizeof(PAIBone));
	
	for I := 0 to Length(s.bones)-1 do
	begin
		New(b);
		//FillChar(b^, Sizeof(TAIBone), #0);
		
		// Name
		aiStringFromDelphiString(b^.mName, s.bones[I].name);
		
		// Weights
		SetLength(w, 0);
		L := 0;
		
		for J := 0 to Length(m.vertices)-1 do
			for K := 0 to 3 do
				if (m.bone_ids[m.vertices[J].bones[K] div 3] = I) and (m.vertices[J].weights[K] > 0) then
				begin
					SetLength(w, L+1);
					
					w[L].mVertexId := J;
					w[L].mWeight := m.vertices[J].weights[K] / 255.0;
					
					Inc(L);
				end;
				
		b^.mNumWeights := Length(w);
		GetMem(b^.mWeights, b^.mNumWeights * Sizeof(TAIVertexWeight));
		Move(w[0], b^.mWeights[0], b^.mNumWeights * Sizeof(TAIVertexWeight));
		
		// Offset matrix
		s.GetTransform(s.bones[I].name, mat);
		Invert43(mat);
		
		Move(mat, b^.mOffsetMatrix, Sizeof(TMatrix));
		aiTransposeMatrix4(b^.mOffsetMatrix);
		
		//
		d.mBones[I] := b;
	end;
end;

function TExportScene.AddObject(const name : String; const transform : TMatrix; model_id : Longint) : Longint;
var
	idx : Longint;
	o : TExportObject;
begin
	o := TExportObject.Create;
	aiStringFromDelphiString(o.node.mName, name);
	
	Move(transform, o.node.mTransformation, Sizeof(TMatrix));
	aiTransposeMatrix4(o.node.mTransformation);
	
	o.model_id := model_id;
	
	idx := objects.Add(name);
	objects.Objects[idx] := o;
	
	Result := idx;
end;

function TExportScene.AddLight(const name : String; const matrix : TMatrix; ltype : Shortint; const color : TVec4; brightness, range, angle : Single) : Longint; 
var
	idx : Longint;
	l : TExportLight;
	
	horde : Single;
begin
	l := TExportLight.Create;
	
	l.name := name;
	l.ltype := ltype;
	l.position.x := matrix[4,1];
	l.position.y := matrix[4,2];
	l.position.z := matrix[4,3];
	
	horde := Sqrt(matrix[2,3]*matrix[2,3] + matrix[3,3]*matrix[3,3]);
	
	l.rotation.x := arctan2(-matrix[2,3], matrix[3,3]);
	l.rotation.y := arctan2(matrix[1,3], horde);
	l.rotation.z := arctan2(-matrix[1,2], matrix[1,1]);

	l.color := color;
	l.brightness := brightness;
	l.angle := angle;
	l.range := range;
	
	idx := lights.Add(name);
	lights.Objects[idx] := l;
	
	Result := idx;
end;

procedure TExportScene.AddSkeleton(s : T4ASkeleton);
var
	I : Longint;
	b : P4ABone;
	o : TExportObject;
	node : PAINode;
begin
	for I := 0 to Length(s.bones)-1 do
		if s.bones[I].parent_name = '' then
			Break;
			
	b := @s.bones[I];
		
	o := TExportObject.Create;
	o.model_id := -1;
	
	node := AddBone(s, b);
	Move(node^, o.node, Sizeof(TAINode));
	Dispose(node);
	
	for I := 0 to node.mNumChildren - 1 do
		node.mChildren^[I].mParent := @node;
	
	objects.Objects[objects.Add(b^.name)] := o;
end;

function TExportScene.AddBone(s : T4ASkeleton; b : P4ABone) : PAINode;
var
	I : Longint;
	node : PAINode;
	matrix : TMatrix;
	J : Longint;
begin
	New(node);
	FillChar(node^, Sizeof(TAINode), #0);
	
	aiStringFromDelphiString(node^.mName, b^.name);
	
	s.GetTransformLocal(b^.name, matrix);
	Move(matrix, node^.mTransformation, Sizeof(TMatrix));
	aiTransposeMatrix4(node^.mTransformation);
	
	J := 0;
	for I := 0 to Length(s.bones)-1 do
		if s.bones[I].parent_name = b^.name then
			Inc(J);
			
	node^.mNumChildren := J;
	GetMem(node^.mChildren, J * Sizeof(PAINode));
	
	J := 0;
	for I := 0 to Length(s.bones)-1 do
		if s.bones[I].parent_name = b^.name then
		begin
			node^.mChildren[J] := AddBone(s, @s.bones[I]);
			node^.mChildren[J]^.mParent := node;
			Inc(J);
		end;
		
	Result := node;
end; 

function Rad2Deg(val : Single) : Single;
begin
	Rad2Deg := val / (PI/180);
end;

function VectorStr3(const v : TVec3) : String;
var
	S : String;
begin
	with v do
		WriteStr(S, X:1:6, ', ', Y:1:6, ', ', Z:1:6);
	Result := S;
end;

function VectorStr3R(const v : TVec3) : String;
var
	S : String;
begin
	with v do
		WriteStr(S, Rad2Deg(X):1:6, ', ', Rad2Deg(Y):1:6, ', ', Rad2Deg(Z):1:6);
	Result := S;
end;

function VectorStr4(const v : TVec4) : String;
var
	S : String;
begin
	with v do
		WriteStr(S, X:1:6, ', ', Y:1:6, ', ', Z:1:6, ', ', W:1:6);
	Result := S;
end;

procedure TExportScene.DoExport(const format, filename : String);
var
	I, J, K : Longint;
	
	arr_materials : array of PAIMaterial;
	arr_meshes : array of PAIMesh;
	arr_objects : array of PAINode;
	
	scn : TAIScene;
	root : TAINode;
	
	o : TExportObject;
	em : TExportModel;
	l : TExportLight;
	
	flags : Cardinal;
	
	lt : Text;
begin
	
	// Materials
	SetLength(arr_materials, materials.Count);
	for I := 0 to materials.Count - 1 do
		arr_materials[I] := @TExportMaterial(materials.Objects[I]).material;
		
	// Meshes
	SetLength(arr_meshes, 0);
	for I := 0 to models.Count - 1 do
	begin
		em := TExportModel(models.Objects[I]);
		
		J := Length(arr_meshes);
		SetLength(arr_meshes, J + Length(em.meshes));
		
		em.start := J;
		em.count := Length(em.meshes);
		
		for K := 0 to Length(em.meshes) - 1 do
			arr_meshes[J+K] := @em.meshes[K];
	end;
	
	// Objects
	SetLength(arr_objects, objects.Count);
	for I := 0 to objects.Count - 1 do
	begin
		o := TExportObject(objects.Objects[I]);
		o.node.mParent := @root;
		
		if o.model_id <> -1 then
		begin
			em := TExportModel(models.Objects[o.model_id]);
			
			if o.node.mMeshes <> nil then
				FreeMem(o.node.mMeshes);
			
			o.node.mNumMeshes := em.count;
			GetMem(o.node.mMeshes, em.Count * Sizeof(Cardinal));
			for J := 0 to em.Count - 1 do
				o.node.mMeshes[J] := em.start+J;
		end;
		
		arr_objects[I] := @o.node;			
	end;
	
	FillChar(root, Sizeof(root), #0);
	aiIdentityMatrix4(root.mTransformation);
	root.mNumChildren := Length(arr_objects);
	root.mChildren := PPAINodeArray(arr_objects);
	
	// Lights
	if lights.Count > 0 then
	begin
		Assign(lt, ChangeFileExt(filename, '_lights.txt'));
		ReWrite(lt);
		
		for I := 0 to lights.Count - 1 do
		begin
			l := TExportLight(lights.Objects[I]);
			WriteLn(lt, '[', l.name, ']');
			WriteLn(lt, 'position = ', VectorStr3(l.position));
			WriteLn(lt, 'rotation = ', VectorStr3R(l.rotation));
			Write(lt, 'type = ');
			case l.ltype of
				1: WriteLn(lt, 'POINT');
				2: WriteLn(lt, 'POINT_SHADOWED');
				3: WriteLn(lt, 'POINT_AMBIENT');
				4,7: WriteLn(lt, 'SPOT');
				5,8: WriteLn(lt, 'SPOT_SHADOWED');
				6,9: WriteLn(lt, 'SPOT_AMBIENT');
				10: WriteLn(lt, 'ELLIPTIC');
				11: WriteLn(lt, 'ELLIPTIC_AMBIENT');
				12: WriteLn(lt, 'DIRECTIONAL');
				13: WriteLn(lt, 'DIRECTIONAL_SHADOWED');
				14: WriteLn(lt, 'HALFOMNI');
				15: WriteLn(lt, 'HALFOMNI_AMBIENT');
				16: WriteLn(lt, 'IBL_PROBE');
				else WriteLn(lt, 'UNKNOWN');
			end;
			
			WriteLn(lt, 'color = ', VectorStr4(l.color));
			WriteLn(lt, 'brightness = ', l.brightness:1:6);
			case l.ltype of 
				4..9: WriteLn(lt, 'angle = ', Rad2Deg(l.angle):1:6);
			end;
			WriteLn(lt, 'range = ', l.range:1:6);
				
			WriteLn(lt);
		end;
		
		Close(lt);
	end;
	
	FillChar(scn, Sizeof(scn), #0);
	scn.mNumMeshes := Length(arr_meshes);
	scn.mMeshes := PPAIMeshArray(arr_meshes);
	scn.mRootNode := @root;
	scn.mNumMaterials := Length(arr_materials);
	scn.mMaterials := PPAIMaterialArray(arr_materials);
	
	WriteLn('exporting...');
	
	flags := aiProcess_MakeLeftHanded or aiProcess_FlipUVs or aiProcess_FlipWindingOrder;
	if aiExportScene(@scn, PAnsiChar(format), PAnsiChar(filename), flags) <> 0 then
			WriteLn(aiGetErrorString);
			
	WriteLn('export finished');
end;

end.
