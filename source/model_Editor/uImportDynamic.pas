unit uImportDynamic;

interface
uses fouramdl, skeleton;

function ImportModelDynamic(const path : String; skeleton : T4ASkeleton) : T4AModelSkinned;

implementation
uses sysutils, vmath, assimp, aiTypes, aiScene, aiMesh, aiVector3D, aiMaterial, uImport;

type sort_cb = function(a, b : Pointer) : Longint; cdecl;
procedure qsort(first : Pointer; number, size : QWord; cb : sort_cb); external 'msvcrt.dll';

procedure CalcAABB(out bb : TAABB; m : T4AModelSkinned); overload;
var
	I : Longint;
begin
	bb := m.meshes[0].bbox;
	
	for I := 1 to Length(m.meshes)-1 do
		AABBMerge(bb, m.meshes[I].bbox);
end;

type
	TVertexWeight = record
		bone_id : Byte;
		weight : Single;
	end;
	PVertexWeight = ^TVertexWeight;
	
	TWeightMap = array of array of TVertexWeight;
	TBoneMap = array of Byte;
	
function weight_sort_cb(a, b : Pointer) : Longint; cdecl;
var
	d : Single;
begin
	d := PVertexWeight(B)^.weight - PVertexWeight(A)^.weight;
	if d < 0.0 then
		Result := -1
	else if d > 0.0 then
		Result := 1
	else
		Result := 0;
end;
	
procedure BuildWeightMap(var bones : TBoneMap; var map : TWeightMap; mesh : PAIMesh; s : T4ASkeleton);
var
	I, J, K, L : Longint;
	
	b : PAIBone;
	id, mesh_id : Longint;
	name : String;
	
	unassigned : Longint;
	num1, num2 : Longint;
	wsum : Longint;
begin
	WriteLn('Building weight map for mesh ''' + aiStringToDelphiString(mesh^.mName) + '''');

	SetLength(bones, 0);
	SetLength(map, mesh^.mNumVertices);
	for I := 0 to mesh^.mNumVertices - 1 do
		SetLength(map[I], 0);

	for I := 0 to mesh^.mNumBones - 1 do
	begin
		b := mesh^.mBones[I];
		
		name := aiStringToDelphiString(b^.mName);
		id := s.GetBoneID(name);
			
		if id = -1 then
			raise Exception.Create('Bone [' + name + '] doesn''t exist!');
		
		if b^.mNumWeights > 0 then
		begin
			L := Length(bones);
			SetLength(bones, L+1);
			bones[L] := id;
			
			mesh_id := L*3;
				
			for J := 0 to b^.mNumWeights - 1 do
			begin		
				K := b^.mWeights[J].mVertexId;
				
				L := Length(map[K]);
				SetLength(map[K], L+1);
				map[K,L].bone_id := mesh_id;
				map[K,L].weight := b^.mWeights[J].mWeight;
			end;
		end;
		
	end;
	
	// sort weights
	for I := 0 to Length(map) - 1 do
		qsort(@map[I,0], Length(map[I]), Sizeof(TVertexWeight), weight_sort_cb);
	
	// verify we hasn't unassigned vertices
	unassigned := 0;
	for I := 0 to Length(map) - 1 do
		if Length(map[I]) = 0 then
			Inc(unassigned);
			
	if unassigned > 0 then
		raise Exception.Create('Mesh has ' + IntToStr(unassigned) + ' unassigned vertices');

	// check for zero weights and vertices infuenced with more than 4 bones
	num1 := 0;
	num2 := 0;
	for I := 0 to Length(map) - 1 do
	begin
		for J := 0 to Length(map[I]) - 1 do
		begin
			if map[I,J].weight = 0 then
				Inc(num1);
		end;
		
		if Length(map[I]) > 4 then
			Inc(num2)
	end;

	if num1 > 0 then
		WriteLn(num1, ' zero weights');
	if num2 > 0 then
		WriteLn(num2, ' vertices influenced with more than 4 bones');
	
{	
	for I := 0 to Length(map)-1 do
	begin
		wsum := 0;
		for J := 0 to Length(map[I])-1 do
			wsum := wsum+map[I,J].weight;
			
		if wsum <> 255 then
		begin
			writeln('wsum = ', wsum);
			
			wsum := 0;
			for J := 1 to Length(map[I])-1 do
				wsum := wsum+map[I,J].weight;
			
			map[I,0].weight := 255-wsum;
		end;
	end;
}
end;

procedure CalcBoneOBB(m : T4AModelSkinnedMesh; s : T4ASkeleton);
var
	I, J : Longint;
	
	bone_id : Longint;
	mesh_bone_id : Longint;

	bone_transform : TMatrix;
	inv_bone_transform : TMatrix;
	
	affect : Longint;
	bb : TAABB;
	p, c : TVec3;
	
	function BoneAffectsVertex(const v : T4AVertSkin; bone : Longint) : Boolean;
	var I : Longint;
	begin
		BoneAffectsVertex := False;
		for I := 0 to 3 do
			if (v.bones[I] = bone) and (v.weights[I] > 0) then
				BoneAffectsVertex := True;
	end;
begin
	for I := 0 to Length(m.bone_ids) - 1 do
	begin
		bone_id := m.bone_ids[I];
		mesh_bone_id := I*3;
		
		s.GetTransform(s.bones[bone_id].name, bone_transform);
		Invert43(inv_bone_transform, bone_transform);
		
		affect := 0;
		for J := 0 to Length(m.vertices) - 1 do
		begin
		
			if BoneAffectsVertex(m.vertices[J], mesh_bone_id) then
			begin
				p.x := m.vertices[J].point.x * m.GetPointScale;
				p.y := m.vertices[J].point.y * m.GetPointScale;
				p.z := m.vertices[J].point.z * m.GetPointScale;
			
				Transform(p, inv_bone_transform);
				
				if affect = 0 then
				begin
					bb.min := p;
					bb.max := p;
				end else
					AABBMergePoint(bb, p);
			
				Inc(affect);
			end;
			
		end;
		
		with m.bone_obb[I] do
		begin
			rot[1,1] := 1.0; rot[1,2] := 0.0; rot[1,3] := 0.0;
			rot[2,1] := 0.0; rot[2,2] := 1.0; rot[2,3] := 0.0;
			rot[3,1] := 0.0; rot[3,2] := 0.0; rot[3,3] := 1.0;
			
			if affect = 0 then
			begin
				offset.x := 0.0; offset.y := 0.0; offset.z := 0.0;
				half_size.x := 0.0; 
				half_size.y := 0.0; 
				half_size.z := 0.0;
			end else
			begin
				AABBCenter(c, bb);
					
				offset.x := c.x; offset.y := c.y; offset.z := c.z;
				half_size.x := bb.max.x-c.x; 
				half_size.y := bb.max.y-c.y; 
				half_size.z := bb.max.z-c.z;
			end;
		end;
		
	end;
end;

function FromAISceneTo4A(scene : PAIScene; flags : TConvFlags; s : T4ASkeleton) : T4AModelSkinned;
var
	import : TImportModelArray;
	
  model : T4AModelSkinned;
  mesh : T4AModelSkinnedMesh;

  src : PAIMesh;
  matrix : TMatrix;
  p, n : TVec3;
  
  ao : Single;

  I, J : Longint;
  
  bb : TAABB;
  
  bones : TBoneMap;
  map : TWeightMap;
  
  coef : Single;
  
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

	model := T4AModelSkinned.Create;
	SetLength(model.meshes, Length(import));
	for I := 0 to Length(model.meshes) - 1 do
		model.meshes[I] := nil;
	
	try
		for I := 0 to Length(import) - 1 do
		begin
			src := scene^.mMeshes[import[I].mesh];
			matrix :=  import[I].transform;
			VerifyMesh(src);
			
			BuildWeightMap(bones, map, src, s);
				
			mesh := T4AModelSkinnedMesh.Create;
			model.meshes[I] := mesh;
			
			mesh.name := aiStringToDelphiString(src^.mName);
			mesh.texture := GetTextureName(scene^.mMaterials[src^.mMaterialIndex]);
			mesh.shader := 'geometry\default';
			mesh.material := 'materials\wood';
			
			SetLength(mesh.bone_ids, Length(bones));
			SetLength(mesh.bone_obb, Length(bones));
			for J := 0 to Length(bones) - 1 do
			begin
				mesh.bone_ids[J] := bones[J];
				
				// TODO calc obb
				with mesh.bone_obb[J] do
				begin
					rot[1,1] := 1.0; rot[1,2] := 0.0; rot[1,3] := 0.0;
					rot[2,1] := 0.0; rot[2,2] := 1.0; rot[2,3] := 0.0;
					rot[3,1] := 0.0; rot[3,2] := 0.0; rot[3,3] := 1.0;
					
					offset.x := 0; offset.y := 0; offset.z := 0;
					half_size.x := 0.2; half_size.y := 0.2; half_size.z := 0.2;
				end;
			end;
	
			SetLength(mesh.vertices, src^.mNumVertices);
			for J := 0 to src^.mNumVertices - 1 do
			begin
				p := ConvertVec3(src^.mVertices[J]);
				Transform(p, matrix);
				
				if J = 0 then begin
					bb.min := p;
					bb.max := p;
				end else
					AABBMergePoint(bb, p);
					
				mesh.vertices[J].point.x := Trunc(p.x * 2720);
				mesh.vertices[J].point.y := Trunc(p.y * 2720);
				mesh.vertices[J].point.z := Trunc(p.z * 2720);
				mesh.vertices[J].point.w := 1;
				
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
				
				// bone order in vertices must be 2, 1, 0, 3 (by descending weight)
				case Length(map[J]) of
					1: begin
						coef := map[J,0].weight;
						mesh.vertices[J].bones[2] := map[J,0].bone_id;
						mesh.vertices[J].weights[2] := Round(map[J,0].weight / coef * 255);
						mesh.vertices[J].bones[1] := 0;
						mesh.vertices[J].weights[1] := 0;
						mesh.vertices[J].bones[0] := 0;
						mesh.vertices[J].weights[0] := 0;
						mesh.vertices[J].bones[3] := 0;
						mesh.vertices[J].weights[3] := 0;
					end;
					2: begin
						coef := map[J,0].weight + map[J,1].weight;
						mesh.vertices[J].bones[2] := map[J,0].bone_id;
						mesh.vertices[J].weights[2] := Round(map[J,0].weight / coef * 255);
						mesh.vertices[J].bones[1] := map[J,1].bone_id;
						mesh.vertices[J].weights[1] := Round(map[J,1].weight / coef * 255);
						mesh.vertices[J].bones[0] := 0;
						mesh.vertices[J].weights[0] := 0;
						mesh.vertices[J].bones[3] := 0;
						mesh.vertices[J].weights[3] := 0;
					end;
					3: begin
						coef := map[J,0].weight + map[J,1].weight + map[J,2].weight;
						mesh.vertices[J].bones[2] := map[J,0].bone_id;
						mesh.vertices[J].weights[2] := Round(map[J,0].weight / coef * 255);
						mesh.vertices[J].bones[1] := map[J,1].bone_id;
						mesh.vertices[J].weights[1] := Round(map[J,1].weight / coef * 255);
						mesh.vertices[J].bones[0] := map[J,2].bone_id;
						mesh.vertices[J].weights[0] := Round(map[J,2].weight / coef * 255);
						mesh.vertices[J].bones[3] := 0;
						mesh.vertices[J].weights[3] := 0;
					end;
					else begin
						coef := map[J,0].weight + map[J,1].weight + map[J,2].weight + map[J,3].weight;
						mesh.vertices[J].bones[2] := map[J,0].bone_id;
						mesh.vertices[J].weights[2] := Round(map[J,0].weight / coef * 255);
						mesh.vertices[J].bones[1] := map[J,1].bone_id;
						mesh.vertices[J].weights[1] := Round(map[J,1].weight / coef * 255);
						mesh.vertices[J].bones[0] := map[J,2].bone_id;
						mesh.vertices[J].weights[0] := Round(map[J,2].weight / coef * 255);
						mesh.vertices[J].bones[3] := map[J,3].bone_id;
						mesh.vertices[J].weights[3] := Round(map[J,3].weight / coef * 255);
					end;
				end;
	
				mesh.vertices[J].tc.x := Trunc(src^.mTextureCoords[0][J].x * 2048);
				mesh.vertices[J].tc.y := Trunc(src^.mTextureCoords[0][J].y * 2048);
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
			
			CalcBoneOBB(mesh, s);
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

function ImportModelDynamic(const path : String; skeleton : T4ASkeleton) : T4AModelSkinned;
var
	scene : PAIScene;
	flags : TConvFlags;
begin
	scene := ImportAIScene(path, flags);
	try
		Result := FromAISceneTo4A(scene, flags, skeleton);
	finally
		aiReleaseImport(scene);
	end;
end;

end.