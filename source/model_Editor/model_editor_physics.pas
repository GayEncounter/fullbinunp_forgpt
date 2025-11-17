unit model_editor_physics;

interface
uses PhysX, fouramdl;

procedure LoadPhysics(ph_scene : TPHScene; model : T4AModelHierrarhy);
procedure LoadPhysicsSkinned(ph_scene : TPHScene; model : T4AModelSkinned);
procedure LoadPhysicsSkeleton(ph_scene : TPHScene; model : T4AModelSkeleton);
procedure LoadPhysicsLevel(ph_scene : TPHScene; level : T4ALevel);

implementation
uses sysutils, classes, cform_utils;

// don't forget to change this file if order of models from cform_utils.pas is changed !!

procedure LoadPhysics(ph_scene : TPHScene; model : T4AModelHierrarhy);
var
	pActor : TPHActor;
	pShapeDesc : Pointer;

	I : Longint;
	
	ph : TList;
begin
	ph := TList.Create;
	MakePhysics(ph, model, True); 

	for I := 0 to Length(model.meshes) - 1 do
	begin
		if ph[I] <> nil then
		begin
			pShapeDesc := PHShapeTrimesh(ph[I]);
			if pShapeDesc = nil then
				raise Exception.Create('PHShapeTrimesh failed');
					
			pActor := PHCreateActor(ph_scene, 0, 1, @pShapeDesc, nil);
			if pActor = nil then
				raise Exception.Create('PHCreateActor failed');
	
			PHSetUserdata(pActor, Pointer(model.meshes[I]));
		end;
	end;
	
	ph.Free;
end;

procedure LoadPhysicsSkinned(ph_scene : TPHScene; model : T4AModelSkinned);
var
	pActor : TPHActor;
	pShapeDesc : Pointer;

	I : Longint;
	
	ph : TList;
begin
	ph := TList.Create;
	MakePhysicsSkinned(ph, model, True); 

	for I := 0 to Length(model.meshes) - 1 do
	begin
		if ph[I] <> nil then
		begin
			pShapeDesc := PHShapeTrimesh(ph[I]);
			if pShapeDesc = nil then
				raise Exception.Create('PHShapeTrimesh failed');
					
			pActor := PHCreateActor(ph_scene, 0, 1, @pShapeDesc, nil);
			if pActor = nil then
				raise Exception.Create('PHCreateActor failed');
	
			PHSetUserdata(pActor, Pointer(model.meshes[I]));
		end;
	end;
	
	ph.Free;
end;

procedure LoadPhysicsSkeleton(ph_scene : TPHScene; model : T4AModelSkeleton);
var
	pActor : TPHActor;
	pShapeDesc : Pointer;
	ph : TList;

	I, J, K : Integer;
begin
	ph := TList.Create;
	MakePhysicsSkeleton(ph, model, True); 
	
	K := 0;
	for I := 0 to Length(model.meshes[0]) - 1 do
		for J := 0 to Length(model.meshes[0,I].meshes) - 1 do
		begin
			if ph[K] <> nil then
			begin
				pShapeDesc := PHShapeTrimesh(ph[K]);
				if pShapeDesc = nil then
					raise Exception.Create('PHShapeTrimesh failed');
				
				pActor := PHCreateActor(ph_scene, 0, 1, @pShapeDesc, nil);
				if pActor = nil then
					raise Exception.Create('PHCreateActor failed');
	
				PHSetUserdata(pActor, Pointer(model.meshes[0,I].meshes[J]));
			end;
			
			Inc(K);
		end;
		
	ph.Free;
end;

procedure LoadPhysicsLevel(ph_scene : TPHScene; level : T4ALevel);
var
	pActor : TPHActor;
	pShapeDesc : Pointer;
	ph : TList;
	
	I, J : Longint;
begin
	ph := TList.Create;
	MakeLevelCform(ph, level); 
	
	J := 0;
	for I := 0 to Length(level.visuals)-1 do
		if level.visuals[I] is T4AModelRef then
		begin
			if ph[J] <> nil then
			begin
				pShapeDesc := PHShapeTrimesh(ph[J]);
				if pShapeDesc = nil then
					raise Exception.Create('PHShapeTrimesh failed');
				
				pActor := PHCreateActor(ph_scene, 0, 1, @pShapeDesc, nil);
				if pActor = nil then
					raise Exception.Create('PHCreateActor failed');
	
				PHSetUserdata(pActor, Pointer(level.visuals[I]));
			end;
			
			Inc(J);
		end;
		
	ph.Free;	
end;

end.
