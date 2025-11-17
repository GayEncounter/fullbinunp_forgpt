unit uScene;

interface
uses common, fouramdl, classes, Konfig, PhysX,
	uEntity, uEnvZone, uSceneCovers, NavmapLdr;

type
	TSceneVersion = (
		sceneVerNone,
		sceneVer2033,
		sceneVerLL,
		sceneVerRedux,
		sceneVerArktika1,
		sceneVerExodus
	);

type
	TScene = class
		level_dir : String;
		level : T4ALevel;
		level2 : T4ALevel2;
		maler : ILevelMaler;
		
		egeoms : T4AModelHierrarhy;
		egeoms_maler : TStaticModelMaler;
		
		decals : T4AModelHierrarhy;
		decals_maler : TStaticModelMaler;
		
		konf : TTextKonfig; // level.bin
		konf_entities : TSection;
		
		konf_add : TTextKonfig; // level.add.bin
		konf_add_entities : TSection;
		
		entities : TList;
		env_zones : TList;
		
		cover_manager : TCoverManager;
		
		showShapes : Boolean;
		showFlags : Boolean;
		showEnvZones : Boolean;
		showDecals : Boolean;
		showEGeoms : Boolean;
		showWays : Boolean;
		showNavmesh : Boolean;
		
		ph_scene : TPHScene;
		ph_meshes : array of TPHTriMesh;
		ph_superstatic : TPHActor;
		
		visible : TFPList;
		visible_instanced_static0 : TFPList; // LOD 0
		visible_instanced_static1 : TFPList; // LOD 1
		visible_instanced_static2 : TFPList; // LOD 2
		visible_instanced_dynamic0 : TFPList;
		visible_instanced_dynamic1 : TFPList;
		visible_instanced_dynamic2 : TFPList;
		
		entities_by_ids : array[0..65535] of TEntity;
		
		navmap : TNavigationMap;
		
		constructor Create;
		destructor Destroy; override;
		
		procedure LevelLoad(const dir : String);
		procedure LevelUnload;
		procedure LevelReload;
		
		procedure LoadEntities(k_level, k_level_add : TTextKonfig; progress : Boolean = False);
		procedure UnloadEntities;
		
		procedure RenderPrepare;
		procedure RenderOpaque;
		procedure RenderOpaqueFast;
		procedure RenderBlended;
		procedure RenderDistort;
		
		procedure RenderEnvZones;
		procedure RenderWays;
		
		procedure AddEntity(e : TEntity);
		procedure RemoveEntity(e : TEntity);
	
		function GenerateId : Word;	
		function EntityById(id : Word) : TEntity;
		function EntityByName(const name : String) : TEntity;
		
		procedure MakeAddon(arr : TEntityArray; addon : Boolean);
		procedure SelectAddon;
		
		function GetSelectedList : TList;
		function GetSelected : TEntityArray;
		
		function EnvZoneByName(const name : String) : TEnvZone;
		
		function GetSelectedEZList : TList;
		function GetSelectedEZ : TEnvZoneArray;
		
		function GetSelectedCovers : TCoverArray;
		
		function GetVersion : TSceneVersion;
		
		procedure UpdateAttaches;
		procedure VerifyIDs;
		
		procedure LoadLevelCform(const dir : String);
		procedure UnloadLevelCform;
		
		procedure LoadEnvironment(env_konf : TTextKonfig);
		function  SaveEnvironment : TTextKonfig;
		procedure UnloadEnvironment;
		
		procedure LoadEnvironmentFromFile(const fn : String);
		procedure SaveEnvironmentToFile(const fn : String);		
	end;
	
var
	Scene : TScene;

implementation
uses sysutils, GL, GLU, GLExt, Texture, levelbin, Iup, cform_utils, PHGroups, vmath, egeoms, uLEOptions, uEditorUtils, Engine;

constructor TScene.Create;
begin
	visible := TFPList.Create;
	visible_instanced_static0 := TFPList.Create;
	visible_instanced_static1 := TFPList.Create;
	visible_instanced_static2 := TFPList.Create;
	visible_instanced_dynamic0 := TFPList.Create;
	visible_instanced_dynamic1 := TFPList.Create;
	visible_instanced_dynamic2 := TFPList.Create;
	
	cover_manager := TCoverManager.Create;
end;

destructor TScene.Destroy;
begin
	visible.Free;
	visible_instanced_static0.Free;
	visible_instanced_static1.Free;
	visible_instanced_static2.Free;
	visible_instanced_dynamic0.Free;
	visible_instanced_dynamic1.Free;
	visible_instanced_dynamic2.Free;
	
	cover_manager.Free;
end;

procedure TScene.LevelLoad(const dir : String);
var
	ep : TSection;	
	k_level, k_level_add : TTextKonfig;
begin
	if not FileExists(dir + '\level.bin') then
	begin
		uEditorUtils.ShowError('There is no level.bin file in ''' + dir + '''!');
		Exit;
	end;
	
	level_dir := dir;

	// load level.bin
	k_level := LoadLevelBin(dir + '\level.bin', Engine.version = eVerLLBeta15102012);
	
	ep := k_level.root.GetSect('entities_params', False);
	if (ep <> nil) and (ep.GetInt('version', 0, 'u16') >= ENTITY_VER_ARKTIKA1) then
		texturesCompressed := True // >= Arktika.1
	else
		texturesCompressed := False;
	
	// load level.add.bin
	if FileExists(dir + '\level.add.bin') then
		k_level_add := LoadLevelBin(dir + '\level.add.bin', Engine.version = eVerLLBeta15102012)
	else
		k_level_add := nil;

	// load superstatic geometry
	if FileExists(dir + '\level') and 
		(FileExists(dir + '\level.geom_pc') or FileExists(dir + '\level.geom_xbox'))then
	begin
		level := T4ALevel.Create;
		level.Load(dir);

		maler := TLevelMaler.Create(level);
		
		WriteLn('!!! num_sectors = ', Length(level.sectors));
	end else
	if DirectoryExists(dir + '\static') then
	begin
		level2 := T4ALevel2.Create;
		level2.Load(dir);

		maler := TLevel2Maler.Create(level2);		
	end;
	
	// load decals
	if FileExists(dir + '\level.decals') then
	begin
		decals := LoadLevelEGeoms(dir + '\level.decals');
		if Length(decals.meshes) > 0 then
			decals_maler := TStaticModelMaler.Create(decals);
	end;
	
	// load egeoms
	if FileExists(dir + '\level.egeoms') then
	begin
		egeoms := LoadLevelEGeoms(dir + '\level.egeoms');
		if Length(egeoms.meshes) > 0 then
			egeoms_maler := TStaticModelMaler.Create(egeoms);
	end;

	// load cform
	ph_scene := PHCreateScene;
	if ph_scene = nil then
		WriteLn('PHCreateScene failed');

	try
		LoadLevelCform(dir);
	except
		on E : Exception do
			IupMessage('Error',
			PAnsiChar('Level cform loading failed. Reason:'#10 + E.ClassName + ': ' + E.Message));
	end;

	// load entities
	LoadEntities(k_level, k_level_add, True);
	
	// load level.environemt
	env_zones := TList.Create;
	
	if FileExists(dir + '\level.environment') then
		LoadEnvironmentFromFile(dir + '\level.environment');
		
	// load covers
	if (k_level.root.GetSect('ucovers', False) <> nil) and (k_level.root.GetSect('ulinks', False) <> nil) then
	begin
	
		WriteLn('count pe_dist_coefs = ', Length( (k_level.root.GetParam('pe_dist_coefs', 'fp32_array') as TFloatArrayValue).data ));
		WriteLn('count pe_dist_infos = ', Length( (k_level.root.GetParam('pe_dist_infos', 'u8_array') as TByteArrayValue).data ));
	
		cover_manager.LoadCoversAndLinks2033(k_level.root, ph_scene);
	end else if FileExists(dir + '\nav_map.covers') then
	begin
		cover_manager.LoadCoversAndLinksLL(dir + '\nav_map.covers', ph_scene);
	end;
		
	if FileExists(dir + '\nav_map.human_combat_covers.pc') then
		cover_manager.LoadHumanCombatCovers(dir + '\nav_map.human_combat_covers.pc', ph_scene);
		
	// load navmap
	if FileExists(dir + '\nav_map.pe') then
	begin
		navmap := LoadNavmap2033(dir + '\nav_map.pe');
		if navmap <> nil then
		begin
			WriteLn('navigation model loaded (2033 format), verts ', Length(navmap.verts), ' tris ', Length(navmap.tris) div 3);
			navmap.SaveRAW(dir + '\nav_map.raw');
		end;
	end else if FileExists(dir + '\nav_map.bin') then
	begin
		navmap := LoadNavmapBin(dir + '\nav_map.bin');
		if navmap <> nil then
		begin
			WriteLn('navigation model loaded (LastLight format), verts ', Length(navmap.verts), ' tris ', Length(navmap.tris) div 3);
			navmap.SaveRAW(dir + '\nav_map.raw');
		end;
	end;
	
end;

procedure TScene.LevelUnload;
begin
	level_dir := '';
  
	FreeAndNil(maler);
	FreeAndNil(level);
	FreeAndNil(level2);
	
	FreeAndNil(decals_maler);
	FreeAndNil(decals);
	
	FreeAndNil(egeoms_maler);
	FreeAndNil(egeoms);

	UnloadEntities;
	UnloadEnvironment;
	
	cover_manager.UnloadAll;

	UnloadLevelCform;
	PHDestroyScene(ph_scene);
	ph_scene := nil;
end;

procedure TScene.LevelReload;
var
	k_level, k_level_add : TTextKonfig;
begin
	if level_dir <> '' then
	begin
		// load level.bin
		k_level := LoadLevelBin(level_dir + '\level.bin', Engine.version = eVerLLBeta15102012);
  	
		// load level.add.bin
		if FileExists(level_dir + '\level.add.bin') then
			k_level_add := LoadLevelBin(level_dir + '\level.add.bin', Engine.version = eVerLLBeta15102012)
		else
			k_level_add := nil;
		
		UnloadEntities;
		LoadEntities(k_level, k_level_add);
		
		UnloadEnvironment;
		env_zones := TList.Create;
		if FileExists(level_dir + '\level.environment') then
			LoadEnvironmentFromFile(level_dir + '\level.environment');
	end;
end;

procedure TScene.LoadEntities(k_level, k_level_add : TTextKonfig; progress : Boolean);
var
	I : Longint;

	ent : TSection;
	E : TEntity;
	
	entity_count : Longint;
	dlg : Ihandle;
	
	function DataValid(s : TSection) : Boolean;
	begin
		DataValid := (ent.GetParam('name', 'stringz') <> nil) and
			((ent.GetParam('', 'pose, matrix_43T') <> nil) or (ent.GetParam('', 'pose, matrix') <> nil));
	end;
begin
	konf := k_level;
	konf_entities := konf.root.GetSect('entities');
  
	if Assigned(k_level_add) then
	begin
		konf_add := k_level_add;
		konf_add_entities := konf_add.root.GetSect('entities');
	end;
  
	entity_count := konf_entities.ParamCount;
	if konf_add_entities <> nil then
		entity_count := entity_count + konf_add_entities.ParamCount;
	
	if progress then
	begin
		dlg := IupProgressDlg;
		IupSetAttribute(dlg, 'TITLE', 'Loading entities');
		IupSetInt(dlg, 'TOTALCOUNT', entity_count);
		IupShowXY(dlg, IUP_CENTER, IUP_CENTER);
	end;

	entities := TList.Create;

	for I := 0 to konf_entities.ParamCount - 1 do
	begin
		if konf_entities.GetParam(I) is TSection then
		begin
			ent := TSection(konf_entities.GetParam(I));

			if progress then
				iup.SetStrAttribute(dlg, 'DESCRIPTION', ent.name);

			if DataValid(ent) then
			begin
				E := TEntity.Create(ph_scene, ent);
				entities.Add(E);
				{
				if entities_by_ids[E.ID] <> nil then
					ShowError('Entity ID conflict between ' + E.Name + ' and ' + entities_by_ids[E.ID].Name);
					
				entities_by_ids[E.ID] := E; }	
			end;
			
			if progress then
				IupSetAttribute(dlg, 'INC', nil);
		end;
	end;

	if konf_add_entities <> nil then
	begin
		for I := 0 to konf_add_entities.ParamCount - 1 do
		begin
			if konf_add_entities.GetParam(I) is TSection then
			begin
				ent := TSection(konf_add_entities.GetParam(I));

				if progress then	
					iup.SetStrAttribute(dlg, 'DESCRIPTION', ent.name);
	
				if DataValid(ent) then
				begin
					E := TEntity.Create(ph_scene, ent);
					entities.Add(E);
					{
					if entities_by_ids[E.ID] <> nil then
						ShowError('Entity ID conflict between ' + E.Name + ' and ' + entities_by_ids[E.ID].Name);
					
					entities_by_ids[E.ID] := E; 	}
				end;
				
				if progress then	
					IupSetAttribute(dlg, 'INC', nil);
			end;
		end;
	end;
	
	if progress then
		IupDestroy(dlg);
	
	//UpdateAttaches;  
	VerifyIDS;
end;

procedure TScene.UnloadEntities;
var
	I : Integer;
begin
	if Assigned(entities) then
	begin
		for I := 0 to entities.Count - 1 do
			TEntity(entities[I]).Free;
		FreeAndNil(entities);
	end;

	FreeAndNil(konf);
	konf_entities := nil;
	
	FreeAndNil(konf_add);
	konf_add_entities := nil;
	
	// clear entities_by_ids
	FillChar(entities_by_ids, Sizeof(entities_by_ids), #0);
end;

function EntitySortCb(p1, p2 : Pointer) : Integer;
var
	e1, e2 : TEntity;
begin
	e1 := TEntity(p1);
	e2 := TEntity(p2);
	
	if e1.distance_sqr > e2.distance_sqr then
		EntitySortCb := -1
	else
	if e1.distance_sqr < e2.distance_sqr then
		EntitySortCb := 1
	else
		EntitySortCb := 0;
end;

function InstancedEntitySortCb(p1, p2 : Pointer) : Integer;
var
	e1, e2 : TEntity;
begin
	e1 := TEntity(p1);
	e2 := TEntity(p2);
	
	InstancedEntitySortCb := e1.visualCRC - e2.visualCRC;
end;

procedure TScene.RenderPrepare;
var
	I : Longint;
	E : TEntity;
	
	diff : TVec3;
	dist_sq : Single;
begin
	visible.Clear;
	visible_instanced_static0.Clear;
	visible_instanced_static1.Clear;
	visible_instanced_static2.Clear;
	visible_instanced_dynamic0.Clear;
	visible_instanced_dynamic1.Clear;
	visible_instanced_dynamic2.Clear;

	if Assigned(entities) then
	begin		
		if visible.Capacity < entities.Count then
			visible.Capacity := entities.Count;
			
		for I := 0 to entities.Count-1 do
		begin
			E := TEntity(entities[I]);
			if E.Visible and AABBVisible(frustum, E.bbox) then
			begin
				diff.x := camera_pos.x-E.FMatrix[4,1];
				diff.y := camera_pos.y-E.FMatrix[4,2];
				diff.z := camera_pos.z-E.FMatrix[4,3];
				dist_sq := diff.x*diff.x + diff.y*diff.y + diff.z*diff.z;
			
				if uLEOptions.cull_distance and Assigned(E.param_cull_dist) and (dist_sq > Sqr(E.param_cull_dist.num)) then
					Continue;
			
				E.distance_sqr := dist_sq;
				E.PrepareDraw;
				
				visible.Add(E);
				
				if Assigned(E.model) and (E.model.b_static) then
				begin
					case E.LOD of
						0: visible_instanced_static0.Add(E);
						1: visible_instanced_static1.Add(E);
						2: visible_instanced_static2.Add(E);
					end;
				end else
				if Assigned(E.model) and (E.model.b_dynamic) then
				begin
					case E.LOD of
						0: visible_instanced_dynamic0.Add(E);
						1: visible_instanced_dynamic1.Add(E);
						2: visible_instanced_dynamic2.Add(E);
					end;
				end;
			end;
		end;
		
		visible.Sort(EntitySortCb);
		visible_instanced_static0.Sort(InstancedEntitySortCb);
		visible_instanced_static1.Sort(InstancedEntitySortCb);
		visible_instanced_static2.Sort(InstancedEntitySortCb);
		visible_instanced_dynamic0.Sort(InstancedEntitySortCb);
		visible_instanced_dynamic1.Sort(InstancedEntitySortCb);
		visible_instanced_dynamic2.Sort(InstancedEntitySortCb);
	end;
end;

procedure TScene.RenderOpaque;
var
	I : Longint;
	E : TEntity;
begin
	try
		if Assigned(maler) then
			maler.Draw;
		if showEGeoms and Assigned(egeoms_maler) then
			egeoms_maler.Draw;
		if showDecals and Assigned(decals_maler) then
			decals_maler.Draw;
	except
		on E: Exception do
			IupMessage('Error', PAnsiChar(E.Message));
	end;
		
	for I := visible.Count-1 downto 0 do
	begin
		E := TEntity(visible[I]);
		if showFlags then
			E.DrawFlag;
		E.Draw(False, False);
	end;
	
	cover_manager.DrawCovers;
	cover_manager.DrawLinks;
	cover_manager.DrawHumanCombatCovers;
end;

procedure TScene.RenderOpaqueFast;
var
	I, J, LOD : Longint;
	E : TEntity;
	
	instances : TInstanceData;
	lists : array[0..2] of TFPList;
begin
	try
		if Assigned(maler) then
			maler.Draw;
		if showEGeoms and Assigned(egeoms_maler) then
			egeoms_maler.Draw;
		if showDecals and Assigned(decals_maler) then
			decals_maler.Draw;
	except
		on E: Exception do
			IupMessage('Error', PAnsiChar(E.Message));
	end;
	
	// flags
	if showFlags then
	begin
		for I := visible.Count-1 downto 0 do
		begin
			E := TEntity(visible[I]);
			E.DrawFlag;
		end;
	end;
	
	// softbody ( no shader yet )
	for I := visible.Count-1 downto 0 do
	begin
		E := TEntity(visible[I]);
		
		if Assigned(E.model) and E.model.b_softbody then
			E.Draw(False, False, True);
	end;

	// static
	glEnableVertexAttribArrayARB(0);
	glEnableVertexAttribArrayARB(1);
	if useBump then glEnableVertexAttribArrayARB(2);
	if useBump then glEnableVertexAttribArrayARB(3);
	if useTextures then glEnableVertexAttribArrayARB(4);
	
	glEnable(GL_VERTEX_PROGRAM_ARB);
	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, prog[VP_STATIC]);
	
	lists[0] := visible_instanced_static0;
	lists[1] := visible_instanced_static1;
	lists[2] := visible_instanced_static2;
	
	for LOD := 0 to 2 do 
	begin
		I := 0;
		while I < lists[LOD].Count do
		begin
			E := TEntity(lists[LOD][I]);
			J := I;
			while (J < lists[LOD].Count) and (TEntity(lists[LOD][J]).visualCRC = E.visualCRC) and (J-I < MAX_INSTANCES) do
			begin				
				instances.matrix[J-I] := TEntity(lists[LOD][J]).Matrix;
				instances.selected[J-I] := TEntity(lists[LOD][J]).Selected;
				Inc(J);
			end;
				
			E.maler.DrawInstanced2(E.mtlset, J-I, instances, False, False);
			
			I := J;
		end;
	end;

	// end of static rendering
	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, 0);
	glDisable(GL_VERTEX_PROGRAM_ARB);

	glDisableVertexAttribArrayARB(0);
	glDisableVertexAttribArrayARB(1);
	if useBump then glDisableVertexAttribArrayARB(2);
	if useBump then glDisableVertexAttribArrayARB(3);
	if useTextures then glDisableVertexAttribArrayARB(4);
	
	// dynamic
	glEnableVertexAttribArrayARB(0);
	glEnableVertexAttribArrayARB(1);
	glEnableVertexAttribArrayARB(2);
	glEnableVertexAttribArrayARB(3);
	if useBump then glEnableVertexAttribArrayARB(4);
	if useBump then glEnableVertexAttribArrayARB(5);
	if useTextures then glEnableVertexAttribArrayARB(6);

	glEnable(GL_VERTEX_PROGRAM_ARB);
	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, prog[VP_DYNAMIC]);
	
	
	if TEntity.showAnimation then
	begin
		for I := visible.Count-1 downto 0 do
		begin
			E := TEntity(visible[I]);	
			
			if Assigned(E.model) and E.model.b_dynamic then
				E.Draw(False, False, True);
		end;
	end else
	begin
		lists[0] := visible_instanced_dynamic0;
		lists[1] := visible_instanced_dynamic1;
		lists[2] := visible_instanced_dynamic2;
		
		for LOD := 0 to 2 do 
		begin
			I := 0;
			while I < lists[LOD].Count do
			begin
				E := TEntity(lists[LOD][I]);
				J := I;
				while (J < lists[LOD].Count) and (TEntity(lists[LOD][J]).visualCRC = E.visualCRC) and (J-I < MAX_INSTANCES) do
				begin
					instances.matrix[J-I] := TEntity(lists[LOD][J]).Matrix;
					instances.selected[J-I] := TEntity(lists[LOD][J]).Selected;
					Inc(J);
				end;
					
				E.model.maler.DrawInstanced2(E.mtlset, J-I, instances, False, False);
				
				I := J;
			end;
		end;
	end;
	
	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, 0);
	glDisable(GL_VERTEX_PROGRAM_ARB);
	
	glDisableVertexAttribArrayARB(0);
	glDisableVertexAttribArrayARB(1);
	glDisableVertexAttribArrayARB(2);
	glDisableVertexAttribArrayARB(3);
	if useBump then glDisableVertexAttribArrayARB(4);
	if useBump then glDisableVertexAttribArrayARB(5);
	if useTextures then glDisableVertexAttribArrayARB(6);
	
	cover_manager.DrawCovers;
	cover_manager.DrawLinks;
	cover_manager.DrawHumanCombatCovers;
end;

procedure TScene.RenderBlended;
var
	I : Longint;
begin

	if showEGeoms and Assigned(egeoms_maler) then
		egeoms_maler.Draw(-1, False, True, False);
	if showDecals and Assigned(decals_maler) then
		decals_maler.Draw(-1, False, True, False);

	for I := 0 to visible.Count-1 do
		TEntity(visible[I]).Draw(True, False);
		
	if showShapes then
		for I := 0 to entities.Count-1 do
			if TEntity(entities[I]).Visible then
				TEntity(entities[I]).DrawShapes;
				
	if showNavmesh and (navmap <> nil) then
		navmap.Draw;

end;

procedure TScene.RenderDistort;
var
	I : Longint;
begin	

	for I := 0 to visible.Count-1 do
		TEntity(visible[I]).Draw(False, True);

end;

procedure TScene.RenderEnvZones;
const
	c : array[0..3] of TVec3 = (
		(X: 0.0; Y: 0.0; Z: 0.8),
		(X: 0.8; Y: 0.0; Z: 0.0),
		(X: 0.8; Y: 0.8; Z: 0.0),
		(X: 0.0; Y: 0.8; Z: 0.8)
	);
var
	I, J, clr : Longint;
	alpha : Single;
	zone : TEnvZone;
begin
	if env_zones = nil then
		Exit;
		
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	
	for I := 0 to env_zones.Count-1 do
	begin
		zone := TEnvZone(env_zones[I]);
	
		clr := I mod Length(c);
		if zone.Selected then
			alpha := 0.8
		else
			alpha := 0.5;
			
		glColor4f(c[clr].x, c[clr].y, c[clr].z, alpha);
		
		glBegin(GL_TRIANGLES);
		for J := 0 to (Length(zone.param_tris.data) div 3) - 1 do
			glVertex3fv(@zone.param_tris.data[J*3]);
		for J := (Length(zone.param_tris.data) div 3) - 1 downto 0 do
			glVertex3fv(@zone.param_tris.data[J*3]);
		glEnd;
	end;
	
	glColor3f(1.0, 1.0, 1.0);
	glDisable(GL_BLEND);
end;

procedure TScene.RenderWays;
var
	I, J : Longint;
	E : TEntity;
	L : TEntity;
	V1 : TVec3;
	V2 : TVec3;
begin
	if entities = nil then
		Exit;
		
	glColor3f(0.0, 1.0, 0.0);	
	glBegin(GL_LINES);
	
	for I := 0 to entities.Count - 1 do
	begin
		E := TEntity(entities[I]);
		
		if E.isWay then
		begin
			for J := 0 to 3 do
			begin
				L := EntityById(E.way_link[J].num);
				if L <> nil then
				begin
					V1.x := 0.0; V1.y := 0.5; V1.z := 0.0;
					V2.x := 0.0; V2.y := 0.5; V2.z := 0.0;
					
					Transform(V1, E.FMatrix);
					Transform(V2, L.FMatrix);
					
					glVertex3fv(@V1.x);
					glVertex3fv(@V2.x);
				end;
			end;
		end;
	end;
	
	glEnd;
	glColor3f(1.0, 1.0, 1.0);	
end;

procedure TScene.AddEntity(e : TEntity);
var
	parm_count : TIntegerValue;
begin
	entities.Add(e);
	konf_entities.items.Add(e.data);
	
	// update entity count for arktika and exodus
	if self.GetVersion >= sceneVerArktika1 then
	begin
		parm_count := konf_entities.GetParam('count', 'u32') as TIntegerValue;
		parm_count.num := konf_entities.ParamCount-1;
	end;
	
	if entities_by_ids[e.ID] <> nil then
		ShowError('Entity ID conflict between ' + e.Name + ' and ' + entities_by_ids[e.ID].Name);
		
	entities_by_ids[e.ID] := e;
end;

procedure TScene.RemoveEntity(e : TEntity);
var
	parm_count : TIntegerValue;
	
	idx : Longint;
	
	I : Longint;
	W : TEntity;
begin
	idx := konf_entities.items.IndexOf(e.data);
	if idx <> -1 then
	begin
		konf_entities.items.Delete(idx);
		
		// update entity count for arktika and exodus
		if self.GetVersion >= sceneVerArktika1 then
		begin
			parm_count := konf_entities.GetParam('count', 'u32') as TIntegerValue;
			parm_count.num := konf_entities.ParamCount-1;
		end;
	end else
	begin
		idx := konf_add_entities.items.IndexOf(e.data);
		if idx <> -1 then
		begin
			konf_add_entities.items.Delete(idx);
			
			// update entity count for arktika and exodus
			if self.GetVersion >= sceneVerArktika1 then
			begin
				parm_count := konf_add_entities.GetParam('count', 'u32') as TIntegerValue;
				parm_count.num := konf_add_entities.ParamCount-1;
			end;
		end else
			raise Exception.Create('entity doesn''t belong to level.bin nor level.add.bin');
	end;
	
	entities.Remove(e);
	
	for I := 0 to entities.Count - 1 do
	begin
		W := TEntity(entities[I]);
		
		if W.isWay then
		begin
			if W.way_link[0].num = e.ID then
				W.way_link[0].num := 65535;
			if W.way_link[1].num = e.ID then
				W.way_link[1].num := 65535;
			if W.way_link[2].num = e.ID then
				W.way_link[2].num := 65535;
			if W.way_link[3].num = e.ID then
				W.way_link[3].num := 65535;
		end;
	end;
	
	entities_by_ids[e.ID] := nil;
	
	e.data.Free;
	e.Free;
end;

function TScene.GenerateId : Word;
var
	id : Word;
begin
	if Assigned(entities) then
	begin
		if self.GetVersion >= sceneVerArktika1 then
			id := 512
		else
			id := 256;
		
		while (id < 65535) and (EntityById(id) <> nil) do
			Inc(id);

		Result := id;
	end else
		Result := 65535;
end;

function TScene.EntityById(id : Word) : TEntity;
var
	I : Integer;
begin
	// are entities use same id-space as ucovers? 
	// added: no.

	Result := nil;
	
	if id = 65535 then
		Exit;

	if Assigned(entities) then
	begin
		for I := 0 to entities.Count - 1 do
		begin
			if TEntity(entities[I]).ID = id then
			begin
				Result := TEntity(entities[I]);
				Exit;
			end;
		end;
	end;
end;

function TScene.EntityByName(const name : String) : TEntity;
var
	I : Integer;
begin
	Result := nil;

	if Assigned(entities) then
	begin
		for I := 0 to entities.Count - 1 do
		begin
			if TEntity(entities[I]).Name = name then
			begin
				Result := TEntity(entities[I]);
				Exit;
			end;
		end;
	end;
end;

procedure TScene.MakeAddon(arr : TEntityArray; addon : Boolean);
var
	I : Longint;
	data : TSection;
begin
	if Assigned(konf_entities) and Assigned(konf_add_entities) then
	begin
		for I := 0 to Length(arr)-1 do
		begin
			data := arr[I].data;
			if not addon then
			begin
				konf_add_entities.items.Remove(data);
				if konf_entities.items.IndexOf(data) = -1 then
					konf_entities.items.Add(data);
			end else
			begin
				konf_entities.items.Remove(data);
				if konf_add_entities.items.IndexOf(data) = -1 then
					konf_add_entities.items.Add(data);
			end;		
		end;
	end;
end;

procedure TScene.SelectAddon;
var
	I : Longint;
	E : TEntity;
begin
	if Assigned(konf_entities) and Assigned(konf_add_entities) then
	begin
		for I := 0 to entities.Count - 1 do
		begin
			E := TEntity(entities[I]);
			if konf_add_entities.items.IndexOf(E.data) <> -1 then
				E.Selected := True
			else
				E.Selected := False;
		end;
	end;
end;

function TScene.GetSelectedList : TList;
var
	list : TList;
	I : Integer;
begin
	list := TList.Create;
	
	if Assigned(entities) then
		for I := 0 to entities.Count - 1 do
			if TEntity(entities[I]).selected then
				list.Add(entities[I]);
			
	GetSelectedList := list;
end;

function TScene.GetSelected : TEntityArray;
var
	sel : TEntityArray;
	I : Integer;
begin
	SetLength(sel, 0);

	if Assigned(entities) then
		for I := 0 to entities.Count - 1 do
			if TEntity(entities[I]).selected then
			begin
				SetLength(sel, Length(sel)+1);
				sel[Length(sel)-1] := TEntity(entities[I]);
			end;
			
	GetSelected := sel;
end;

function TScene.EnvZoneByName(const name : String) : TEnvZone;
var
	I : Integer;
begin
	Result := nil;

	if Assigned(env_zones) then
	begin
		for I := 0 to env_zones.Count - 1 do
		begin
			if TEnvZone(env_zones[I]).Name = name then
			begin
				Result := TEnvZone(env_zones[I]);
				Exit;
			end;
		end;
	end;
end;

function TScene.GetSelectedEZList : TList;
var
	list : TList;
	I : Integer;
begin
	list := TList.Create;
	
	if Assigned(env_zones) then
		for I := 0 to env_zones.Count - 1 do
			if TEnvZone(env_zones[I]).Selected then
				list.Add(env_zones[I]);
			
	GetSelectedEZList := list;
end;

function TScene.GetSelectedEZ : TEnvZoneArray;
var
	sel : TEnvZoneArray;
	I : Integer;
begin
	SetLength(sel, 0);

	if Assigned(env_zones) then
		for I := 0 to env_zones.Count - 1 do
			if TEnvZone(env_zones[I]).Selected then
			begin
				SetLength(sel, Length(sel)+1);
				sel[Length(sel)-1] := TEnvZone(env_zones[I]);
			end;
			
	GetSelectedEZ := sel;
end;

function TScene.GetSelectedCovers : TCoverArray;
var
	sel : TCoverArray;
	I : Integer;
begin
	SetLength(sel, 0);

	for I := 0 to 65534 do
		if (TCover(cover_manager.covers[I]) <> nil) and TCover(cover_manager.covers[I]).Selected then
		begin
			SetLength(sel, Length(sel)+1);
			sel[Length(sel)-1] := TCover(cover_manager.covers[I]);
		end;
			
	GetSelectedCovers := sel;
end;

function TScene.GetVersion : TSceneVersion;
var
	entities_params : TSection;
	entity_ver : Word;
begin
	if konf <> nil then
	begin
		entities_params := konf.root.GetSect('entities_params', False);
		if entities_params <> nil then
		begin
			entity_ver := entities_params.GetInt('version', 'u16');
			
			if entity_ver >= 49 { ENTITY_VER_EXODUS } then 
				Result := sceneVerExodus
			else if entity_ver >= ENTITY_VER_ARKTIKA1 then
				Result := sceneVerArktika1
			else if entity_ver >= ENTITY_VER_REDUX then
				Result := sceneVerRedux
			else
				Result := sceneVerLL;
		end else
			Result := sceneVer2033;
	end else
		Result := sceneVerNone;
end;

procedure TScene.UpdateAttaches;
var
	I : Longint;
	e, parent : TEntity;
	
	att_bone : String;
	att_mat, base_mat : TMatrix;
begin
	if not Assigned(entities) then
		Exit;
		
	for I := 0 to entities.Count-1 do
	begin
		e := TEntity(entities[I]);
		if e.ParentID <> 65535 then
		begin
			parent := EntityById(e.ParentID);
			if parent <> nil then
			begin
				att_mat := e.AttachOffset;
				att_bone := e.AttachBone;
				
				if parent.GetBoneTransform(att_bone, base_mat) then
				begin
					Mul44(base_mat, att_mat);
					e.Matrix := base_mat;
				end;
			end;
		end;
	end;
end;

procedure TScene.VerifyIDs;
var
	I : Longint;
	e : TEntity;
	
	idc : array of Longint;
begin
	SetLength(idc, 65536);
	for I := 0 to 65535 do
		idc[I] := 0;
		
	for I := 0 to entities.Count - 1 do
	begin
		e := TEntity(entities[I]);
		Inc(idc[e.ID]);
	end;
	
	for I := 0 to 65535 do
		if idc[I] > 1 then
			WriteLn('ID conflict found: ', I);
			
	for I := 0 to entities.Count - 1 do
	begin
		e := TEntity(entities[I]);
		if (e.ParentID <> 65535) and (idc[e.ParentID] < 1) then
			WriteLn('Invalid parent ID: ', e.ParentID);
	end;
	
	WriteLn('verify ids done. objects ', entities.Count );
	if entities.Count > 0 then
		WriteLN('last : ', TEntity(entities[entities.Count-1]).Name);
end;

procedure TScene.LoadLevelCform(const dir : String);
var
	ph : TList;
	shapes : array of Pointer;
	I : Integer;
begin
	ph := TList.Create;
	try
		{if FileExists(dir + '\level.nxcform_pc') then
			LoadPhysics(dir + '\level.nxcform_pc', ph)
		else
		if FileExists(dir + '\level.nxcform_xbox') then
			LoadPhysics(dir + '\level.nxcform_xbox', ph)
		else}
		if level <> nil then
			MakeLevelCform(ph, level)
		else
		if level2 <> nil then
			MakeLevelCform2(ph, level2);

		if ph.Count > 0 then
		begin
			SetLength(shapes, ph.Count);
			SetLength(ph_meshes, ph.Count);
			for I := 0 to ph.Count - 1 do
			begin
				ph_meshes[I] := ph[I];
				shapes[I] := PHShapeTriMesh(ph[I], nil, nil);
			end;
	
			ph_superstatic := PHCreateActor(ph_scene, 0, Length(shapes), @shapes[0], nil);
			if ph_superstatic = nil then
				WriteLn('level cform loading failed, actor not created');
		end;
	finally
		ph.Free;
	end;
end;

procedure TScene.UnloadLevelCform;
var
	I : Longint;
begin
	PHRemoveActor(ph_scene, ph_superstatic);
	ph_superstatic := nil;
	
	for I := 0 to Length(ph_meshes)-1 do
		PHFreeTriMesh(ph_meshes[I]);
	SetLength(ph_meshes, 0);
end;

procedure TScene.LoadEnvironment(env_konf : TTextKonfig);
var
	I : Longint;
	
	sect_environment : TSection;
	version : Longword;
	
	sect_zones : TSection;
	data : TSection;
begin	
	if env_konf <> nil then
	begin
		sect_environment := env_konf.root.GetSect('environment');
		version := sect_environment.GetInt('version', 'u32');
		
		if version > 11 then
		begin
			WriteLn('level.environment: unsupported version ', version, ', won''t load');
			Exit;
		end;
		
		sect_zones := sect_environment.GetSect('zones');
		for I := 1 to sect_zones.ParamCount-1 do
		begin
			data := TSection((sect_zones.GetParam(I) as TSection).Copy);
			data.name := 'env_zone'; // will be renumerated in SaveEnvironment ;)
			env_zones.Add(TEnvZone.Create(ph_scene, data));
		end;
	end;
end;

function TScene.SaveEnvironment : TTextKonfig;
var
	I : Longint;
	version : Longint;
	bbox, bbox2 : TAABB;
	
	env_konf : TTextKonfig;
	sect1, sect2 : TSection;
	
	data : TSection;
	n : String[16];
begin
	case GetVersion of
		sceneVer2033:   version := 8;
		sceneVerLL,
		sceneVerRedux:  version := 11;
		else            version := -1;
	end;
	
	if version <> -1 then
	begin
		env_konf := TTextKonfig.Create;
		sect1 := env_konf.root.AddSect('environment');
		sect1.AddInt('version', version, 'u32');
		
		for I := 0 to env_zones.Count - 1 do
		begin
			if I = 0 then
				TEnvZone(env_zones[I]).GetBBox(bbox)
			else
			begin
				TEnvZone(env_zones[I]).GetBBox(bbox2);
				AABBMerge(bbox, bbox2);
			end
		end;
		
		sect1.AddVec3('aabb_min', bbox.min);
		sect1.AddVec3('aabb_max', bbox.max);
		
		sect1.AddHint('zones', 'array');
		sect2 := sect1.AddSect('zones');
		
		sect2.AddInt('count', env_zones.Count, 'u32');
		for I := 0 to env_zones.Count - 1 do
		begin
			data := TSection(TEnvZone(env_zones[I]).data.Copy);
			
			n := IntToStr(I);
			data.name := 'rec_' + StringOfChar('0',4-Length(n)) + n;
			
			sect2.items.Add(data);
		end;
		
		Result := env_konf;
	end else
		Result := nil;
end;

procedure TScene.UnloadEnvironment;
var
	I : Longint;
begin
	if Assigned(env_zones) then
	begin
		for I := 0 to env_zones.Count - 1 do
			TEnvZone(env_zones[I]).Free;
		FreeAndNil(env_zones);
	end;
end;

procedure TScene.LoadEnvironmentFromFile(const fn : String);
var
	K : TKonfig;
	env_konf : TTextKonfig;
begin
	env_konf := TTextKonfig.Create;
		
	K := TKonfig.Create;
	K.Load(fn);
	K.Decompile(env_konf, GetVersion >= sceneVerLL);
	K.Free;
	
	LoadEnvironment(env_konf);
	
	env_konf.Free;
end;

procedure TScene.SaveEnvironmentToFile(const fn : String);
var
	env_konf : TTextKonfig;
	K : TKonfig;
begin
	env_konf := SaveEnvironment;
	
	if env_konf <> nil then
	begin	
		K := TKonfig.Create;
		K.kind := 3;
		K.Compile(env_konf, GetVersion >= sceneVerLL);
		K.Save(fn);
			
		K.Free;
		env_konf.Free;
	end;
end;

end.
