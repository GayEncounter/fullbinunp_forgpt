unit uImportStatic;

interface
uses fouramdl;

function ImportModelStatic(const path : String) : T4AModelHierrarhy;

implementation
uses sysutils, vmath, assimp, aiTypes, aiScene, aiMesh, aiVector3D, aiMaterial, uImport, OGFImport;

procedure CalcAABB(out bb : TAABB; m : T4AModelHierrarhy); overload;
var
	I : Longint;
begin
	bb := m.meshes[0].bbox;
	
	for I := 1 to Length(m.meshes)-1 do
		AABBMerge(bb, m.meshes[I].bbox);
end;

function FromAISceneTo4A(scene : PAIScene; flags : TConvFlags) : T4AModelHierrarhy;
var
	import : TImportModelArray;
	
  model : T4AModelHierrarhy;
  mesh : T4AModelSimple;

  src : PAIMesh;
  matrix : TMatrix;
  p, n : TVec3;

	ao : Single;

  I, J : Longint;
  
  bb : TAABB;
  
  function ConvertVec3(const v : TAIVector3D) : TVec3;
  begin
  	if cfSwapYZ in flags then begin
  		Result.x := v.x;
  		Result.y := v.z;
  		Result.z := v.y;
  	end else begin
  		Result.x := v.x;
  		Result.y := v.y;
  		Result.z := v.z;
  	end;
  	
  	if cfMirrorZ in flags then
  		Result.z := -Result.z;
  end;
begin
	CollectModelsForImport(scene, import);
	
	model := T4AModelHierrarhy.Create;
	SetLength(model.meshes, Length(import));
	for I := 0 to Length(model.meshes) - 1 do
		model.meshes[I] := nil;
	
	try
		for I := 0 to Length(import) - 1 do
		begin
			src := scene^.mMeshes[import[I].mesh];
			matrix := import[I].transform;
			VerifyMesh(src);
				
			mesh := T4AModelSimple.Create;
			model.meshes[I] := mesh;
			
			mesh.name := aiStringToDelphiString(src^.mName);
			mesh.texture := GetTextureName(scene^.mMaterials[src^.mMaterialIndex]);
			mesh.shader := 'geometry\default';
			mesh.material := 'materials\wood';
	
			SetLength(mesh.vertices, src^.mNumVertices);
			for J := 0 to src^.mNumVertices - 1 do
			begin
				p := ConvertVec3(src^.mVertices[J]);
				Transform(p, matrix);
				
				if J = 0 then
				begin
					bb.min := p;
					bb.max := p;
				end else
					AABBMergePoint(bb, p);
				
				mesh.vertices[J].point := p;
				
				if src^.mColors[0] <> nil then
				begin
					with src^.mColors[0][J] do
						ao := (r + g + b) / 3;
				end else
					ao := 0.5;
				
				n := ConvertVec3(src^.mNormals[J]);
				Transform33(n, matrix);
				Normalize(n);
	      mesh.vertices[J].normal := PackNormal(n) or (Trunc(ao * 255) shl 24);
	
				n := ConvertVec3(src^.mTangents[J]);
				Transform33(n, matrix);
				Normalize(n);
				mesh.vertices[J].tangent := PackNormal(n);    
	
				n := ConvertVec3(src^.mBitangents[J]);
				n.x := n.x * -1.0; n.y := n.y * -1.0; n.z := n.z * -1.0;
				Transform33(n, matrix);
				Normalize(n);
				mesh.vertices[J].binormal := PackNormal(n);   
	
				mesh.vertices[J].tc.x := src^.mTextureCoords[0][J].x;
				mesh.vertices[J].tc.y := src^.mTextureCoords[0][J].y;
	    end;
	
			SetLength(mesh.indices, src^.mNumFaces * 3);
			for J := 0 to src^.mNumFaces - 1 do
			begin
				if (cfSwapYZ in flags) or (cfMirrorZ in flags) then
				begin
					mesh.indices[J*3  ] := src^.mFaces[J].mIndices[2];
					mesh.indices[J*3+1] := src^.mFaces[J].mIndices[1];
					mesh.indices[J*3+2] := src^.mFaces[J].mIndices[0];
				end else
				begin
					mesh.indices[J*3  ] := src^.mFaces[J].mIndices[0];
					mesh.indices[J*3+1] := src^.mFaces[J].mIndices[1];
					mesh.indices[J*3+2] := src^.mFaces[J].mIndices[2];
				end;
			end;
	
			mesh.bbox := bb;
			CalcBSphere(mesh.bsphere, mesh.bbox);
		end;
		
		CalcAABB(model.bbox, model);
		CalcBSphere(model.bsphere, model.bbox);
	
		Result := model;
	except
		on E: Exception do
		begin
			model.Free;
			raise;
		end;
	end;
end;

function ImportModelStatic(const path : String) : T4AModelHierrarhy;
var
	scene : PAIScene;
	flags : TConvFlags;
begin
	if GetExt(path) = '.ogf' then
	begin
		Result := ImportOGF(path);
	end else
	begin
		scene := ImportAIScene(path, flags);
		try
			Result := FromAISceneTo4A(scene, flags);
		finally
			aiReleaseImport(scene);
		end;
	end;
end;

end.