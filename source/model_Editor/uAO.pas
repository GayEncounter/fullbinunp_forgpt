unit uAO;
// ambient occlusion calculation unit
// dumb and must be rewritten

interface
uses vmath, fouramdl;

type
	TRandomDir = procedure(var dir : TVec3);

procedure CalculateAO(mdl : T4AModelHierrarhy; rdir : TRandomDir; samples, smooth : Integer);

implementation 
uses classes, cform_utils, PhysX;

var 
	ambientOcclusion : array of array of Single;

procedure FindAdjacents(
	const p : TVec3;
	var adjs : array of PSingle;
	var adjCnt : Integer;
	mdl : T4AModelHierrarhy
);
var
	I, J : Integer;
	m : T4AModelSimple;
	s : P4AVertStatic;

	procedure AddAdj(adj : Word);
	var
		K : Integer;
	begin
		for K := 0 to adjCnt - 1 do
		begin
			if adjs[K] = @ambientOcclusion[I, adj] then
				Exit;
		end;

		Inc(adjCnt);
		adjs[adjCnt-1] := @ambientOcclusion[I, adj];
	end;

	function equal(const a, b : TVec3) : Boolean;
	begin
		equal := (a.x = b.x) and (a.y = b.y) and (a.z = b.z);
	end;
begin
	for I := 0 to Length(mdl.meshes) - 1 do
	begin
		m := mdl.meshes[I];

		for J := 0 to (Length(m.indices) div 3) - 1 do
		begin

			s := P4AVertStatic(m.vertices);
			Inc(s, m.indices[J*3	]);
			if equal(s^.point, p) then
			begin
				AddAdj(m.indices[J*3+1]);
				AddAdj(m.indices[J*3+2]);
			end;

			s := P4AVertStatic(m.vertices);
			Inc(s, m.indices[J*3+1]);
			if equal(s^.point, p) then
			begin
				AddAdj(m.indices[J*3	]);
				AddAdj(m.indices[J*3+2]);
			end;

			s := P4AVertStatic(m.vertices);
			Inc(s, m.indices[J*3+2]);
			if equal(s^.point, p) then
			begin
				AddAdj(m.indices[J*3	]);
				AddAdj(m.indices[J*3+1]);
			end;

		end;
	end;
end;

procedure SmoothAO(mdl : T4AModelHierrarhy; iterations : Integer);
var
	I, J, K : Integer;
	adjs : array of array of array[0..255] of PSingle;
	adjCnt : Integer;

	m : T4AModelSimple;
	ao : Single;

	new_ao : array of Single;
	v : P4AVertStatic;
begin
	// find adjacents first
	SetLength(adjs, Length(mdl.meshes));
	for I := 0 to Length(mdl.meshes) - 1 do
	begin
		m := mdl.meshes[I];
		SetLength(adjs[I], Length(m.vertices));
		for J := 0 to Length(adjs[I]) - 1 do
		begin
			v := P4AVertStatic(m.vertices);
			Inc(v, J);

			adjCnt := 0;
			FindAdjacents(v^.point, adjs[I, J], adjCnt, mdl);
			adjs[I,J,adjCnt] := nil;
		end;
	end;

	while iterations > 0 do
	begin
		for I := 0 to Length(mdl.meshes) - 1 do
		begin
			m := mdl.meshes[I];
	
			SetLength(new_ao, Length(m.vertices));
			for J := 0 to Length(new_ao) - 1 do
			begin
				ao := ambientOcclusion[I,J];
	
				K := 0;
				while adjs[I,J,K] <> nil do
				begin
					ao := ao + adjs[I,J,K]^;
					Inc(K);
				end;
				ao := ao / (K + 1);
	
				new_ao[J] := ao;
			end;
	
			v := P4AVertStatic(m.vertices);
			for J := 0 to Length(m.vertices) - 1 do
			begin
				ambientOcclusion[I,J] := new_ao[J];
				v^.normal := (v^.normal and $00FFFFFFF) or (Trunc(new_ao[J] * 255) shl 24);
				Inc(v);
			end;
		end;
	
		Dec(iterations);
	end;
end;

procedure CalculateAO(mdl : T4AModelHierrarhy; rdir : TRandomDir; samples, smooth : Integer);
var
	I, J, K : Longint;
	m : T4AModelSimple;

	v : P4AVertStatic;
	count : Longword;

	dir, pt, n : TVec3;
	ao : Single;

	lights : array of TVec3;
	
	ph_scene : TPHScene;
	ph_meshes : TList;
	sh : array of Pointer;
const
	scale = 0.001;
	scalen = 0.1;
begin
	ph_scene := PHCreateScene;
	ph_meshes := TList.Create;
	MakePhysics(ph_meshes, mdl, False);
	
	SetLength(sh, ph_meshes.Count);
	for I := 0 to Length(sh)-1 do
		sh[I] := PHShapeTrimesh(ph_meshes[I]);
		
	PHCreateActor(ph_scene, 0, Length(sh), @sh[0], nil);

	SetLength(lights, samples);
	for I := 0 to samples - 1 do
	begin
		rdir(lights[I]);
		Normalize(lights[I]);
	end;

	SetLength(ambientOcclusion, Length(mdl.meshes));
	for I := 0 to Length(mdl.meshes) - 1 do
	begin
		m := mdl.meshes[I];
		v := P4AVertStatic(m.vertices);
		SetLength(ambientOcclusion[I], Length(m.vertices));
		count := Length(m.vertices);
		for J := 0 to count - 1 do
		begin
			UnpackNormal(n, v^.normal);
			ao := 0.0;

			for K := 0 to samples - 1 do
			begin
				dir := lights[K];

				//r := dir.x*n.x + dir.y*n.y + dir.z*n.z;
				//if r < 0 then
				//	Continue;

				pt.x := (dir.x*scale) + (n.x*scalen) + v^.point.x;
				pt.y := (dir.y*scale) + (n.y*scalen) + v^.point.y;
				pt.z := (dir.z*scale) + (n.z*scalen) + v^.point.z;

				if (PHRaycastAnyShape(ph_scene, @pt, @dir, 500.0) = 0) then
				begin
					ao := ao + (1.0/samples);
				end;
			end;
			
			if ao > 1.0 then
				ao := 1.0;

			ambientOcclusion[I, J] := ao;
			v^.normal := (v^.normal and $00FFFFFF) or (Trunc(ao * 255) shl 24);

			Inc(v);
		end;
	end;

//	if smooth > 0 then
//		SmoothAO(mdl, smooth);
	
	for I := 0 to ph_meshes.Count-1 do
		PHFreeTriMesh(ph_meshes[I]);
	PHDestroyScene(ph_scene);
end;

end.