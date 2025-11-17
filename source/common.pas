unit common;

interface
uses classes, GL, fouramdl, motion, vmath, texturePrefs, glfont, common_resource, common_texture;

const
	MAX_INSTANCES = 2048;

type
	TInstanceData = record
		matrix   : array[0..MAX_INSTANCES-1] of TMatrix;
		selected : array[0..MAX_INSTANCES-1] of Boolean;
	end;

	IMaler = class
		procedure Draw(mtlset : Longint = -1; selected : Boolean = False; blended : Boolean = False; distort : Boolean = False); virtual; abstract;
		procedure Draw2(mtlset : Longint = -1; selected : Boolean = False; blended : Boolean = False; distort : Boolean = False); virtual; abstract;
		procedure DrawInstanced2(mtlset, count : Longint; const instances : TInstanceData; blended, distort : Boolean); virtual; abstract;
	end;
	
	ILevelMaler = class
		procedure Draw; virtual abstract;
	end;

const
	VP_LEVEL          = 1;
	VP_STATIC         = 2;
	VP_DYNAMIC        = 3;

	FP_BUMPED         = 4;
	FP_DETAIL         = 5;
	FP_BUMPED_DETAIL  = 6;

	FP_SELECTED       = 7;
	FP_WALLMARK       = 8;
	FP_SELFLIGHT      = 9;
	
	FP_SCREEN_IMAGE   = 10;

var
	useTextures : Boolean = True;
	useDetail : Boolean = False;//True;
	useBump : Boolean = False;
	useWireframe : Boolean = False;
	useWeather : Boolean = False;
	showAO : Boolean = False;
	
	bkg_color : TVec4 = (X: 0.4; Y: 0.7; Z: 0.8; W: 1.0);
	prog : array[1..16] of GLuint;
	
	camera_pos : TVec3;
	frustum : array[1..6] of TPlane;
	modelview, proj : TMatrix;
	modelview_d, proj_d : array[1..4,1..4] of GLdouble;
	viewport : array[0..3] of GLInt;

type
	TResModel = class(TResource)
		model : T4AModel;
		maler : IMaler;
		maler_lod1 : IMaler;
		maler_lod0 : IMaler;
		ph : TList;
		
		b_static, b_dynamic, b_softbody : Boolean;

		constructor Create(const nm : String; m : T4AModel);
		destructor Destroy; override;
		
		function GetMaterialSet(const name : String) : Longint;
	end;

function GetModel(const name : String) : TResModel;
procedure FreeModel(r : TResModel);

procedure ClearResources;

type
	TMaterial = class
		texture, shader : String;

		tex : TResTexture;
		tex_bump : TResTexture;
		tex_detail : TResTexture;

		alpha : Boolean;
		
		blended : Boolean;
		blend_src, blend_dst : GLenum;
		
		distort : Boolean;
		visible : Boolean;
		double_side : Boolean;
		
		fragment_prog : GLuint;

		constructor Create(const texture_name : String; const shader_name : String; const model_name : String = '');
		destructor Destroy; override;

		procedure Enable;
		procedure Disable;
	end;

	TMaterialArray = array of TMaterial;

type	
	TMaterialSet = class
		FMaterials : TMaterialArray;
		n_opaque, n_blended, n_distort : Longint;
		base : TMaterialSet;
		
		constructor Create(m : TMaterialArray); overload;
		constructor Create(m : TMaterialArray; base : TMaterialSet); overload;
		destructor Destroy; override;
		
		function GetMaterial(idx : Longint) : TMaterial; inline;
		
		property Materials[idx : Longint] : TMaterial read GetMaterial; default;
	end;

type
	TStaticModelMaler = class;
	TSkeletonModelMaler = class;
	TSkinnedModelMaler = class;
	
	TStaticModelMaler = class(IMaler)
		model : T4AModelHierrarhy;
		materials : TMaterialSet;
		buffers : array[1..2] of GLuint;
		
		mtlsets : array of TMaterialSet;
		
		mesh_count : Longint;
		index_count  : array of GLsizei;
		index_offset : array of Pointer;
		base_vertex  : array of GLint;
		
		vao : GLuint;

		constructor Create(m : T4AModelHierrarhy; presets : P4AMAterialSetArray = nil);
		destructor Destroy; override;

		procedure Draw(mtlset : Longint = -1; selected : Boolean = False; blended : Boolean = False; distort : Boolean = False); override;
		procedure Draw2(mtlset : Longint = -1; selected : Boolean = False; blended : Boolean = False; distort : Boolean = False); override;
		procedure DrawInstanced2(mtlset, count : Longint; const instances : TInstanceData; blended, distort : Boolean); override;
	end;

	TSkeletonModelMaler = class(IMaler)
		model : T4AModelSkeleton;
		child : array of TSkinnedModelMaler;
		
		tex_subst : TStringList;
		bone_xform : array[0..255] of TMatrix;
		inv_bind_pose : array of TMatrix;

		constructor Create(m : T4AModelSkeleton; lod : Longint = 2);
		destructor Destroy; override;

		procedure GetTexSubst(var tex : String);
		
		procedure ResetTransform;
		procedure MotionTransform(mot : I4AMotion; t : Single);

		procedure Draw(mtlset : Longint = -1; selected : Boolean = False; blended : Boolean = False; distort : Boolean = False); override;
		procedure Draw2(mtlset : Longint = -1; selected : Boolean = False; blended : Boolean = False; distort : Boolean = False); override;
		procedure DrawInstanced2(mtlset, count : Longint; const instances : TInstanceData; blended, distort : Boolean); override;
	end;
	
	TSkinnedModelMaler = class(IMaler)
		parent : TSkeletonModelMaler;
		model : T4AModelSkinned;
		materials : TMaterialSet;
		buffers : array[1..2] of GLuint;
		
		mtlsets : array of TMaterialSet;

		constructor Create(_parent : TSkeletonModelMaler; m : T4AModelSkinned);
		destructor Destroy; override;

		procedure Draw(mtlset : Longint = -1; selected : Boolean = False; blended : Boolean = False; distort : Boolean = False); override;
		procedure Draw2(mtlset : Longint = -1; selected : Boolean = False; blended : Boolean = False; distort : Boolean = False); override;
		procedure DrawInstanced2(mtlset, count : Longint; const instances : TInstanceData; blended, distort : Boolean); override;
	end;
	
	TSoftbodyModelMaler = class(IMaler)
		model : T4AModelSoftbody;
		material : TMaterial;
		
		center : TVec3;

		constructor Create(m : T4AModelSoftbody);
		destructor Destroy; override;

		procedure Draw(mtlset : Longint = -1; selected : Boolean = False; blended : Boolean = False; distort : Boolean = False); override;
		procedure Draw2(mtlset : Longint = -1; selected : Boolean = False; blended : Boolean = False; distort : Boolean = False); override;
	end;

	TLevelMaler = class(ILevelMaler)
		level : T4ALevel;
		materials : array of TMaterial;
		buffers : array[1..2] of GLuint;
		
		visible : TList;

		constructor Create(l : T4ALevel);
		destructor Destroy; override;

		procedure Draw; override;

		procedure AddModel(m : T4AModel);
		procedure AddHierrarhy(m : T4AModelHierrarhyL);
		procedure AddSimple(m : T4AModelRef);
		
		procedure DrawSimple(m : T4AModelRef);
		procedure DrawSelected(m : T4AModelRef);
	end;

	TLevel2Maler = class(ILevelMaler)
		sublevels : array of TLevelMaler;

		constructor Create(l : T4ALevel2);
		destructor Destroy; override;

		procedure Draw; override;
	end;
	
procedure LoadVP(prog : GLuint; const fn : String);
procedure LoadFP(prog : GLuint; const fn : String);
	
procedure ReloadGLPrograms;
procedure InitializeRender;

procedure RenderDefault;
procedure RenderWireframe;

// utility functions
procedure LoadMeshes(model : T4AModelSkeleton; const path : String);
procedure LoadSkeleton(model : T4AModelSkeleton);
procedure UpdateBBox(model : T4AModelSkeleton);

type
	TFlagColor = (fclWhite, fclYellow, fclRedRound, fclGreenRound, fclBlueRound, fclRedRoundHL, fclGreenRoundHL, fclBlueRoundHL, fclWhiteRound);

procedure DrawFlag(clr : TFlagColor);

implementation
uses sysutils, GLExt, fgl, chunkedFile, Texture, PhysX, cform_utils, Iup, skeleton, Engine, common_flags;

type
	TResModelMap    = TFPGMap<String,TResModel>;
var
	models			: TResModelMap;

constructor TResModel.Create(const nm : String; m : T4AModel);
var
	//model_path : String;
	
	m_static : T4AModelHierrarhy;
	m_skeleton : T4AModelSkeleton;
	m_softbody : T4AModelSoftbody;
begin
	inherited Create(nm);
	model := m;
	if model is T4AModelHierrarhy then
	begin
		m_static := T4AModelHierrarhy(model);
		
		maler := TStaticModelMaler.Create(m_static);
		if m_static.lod1 <> nil then
			maler_lod1 := TStaticModelMaler.Create(m_static.lod1, @m_static.mtlsets);
		if m_static.lod0 <> nil then
			maler_lod0 := TStaticModelMaler.Create(m_static.lod0, @m_static.mtlsets);
		
		//if model.version < MODEL_VER_EXODUS then
		begin
			ph := TList.Create;
	
			{model_path := ResourcesPath + '\meshes\' + nm;
			if FileExists(model_path + '.nxcform_xbox') then
				LoadPhysics(model_path + '.nxcform_xbox', ph)
			else 
			if FileExists(model_path + '.nxcform_pc') then
				LoadPhysics(model_path + '.nxcform_pc', ph)
			else}
				MakePhysics(ph, m_static);
		end;
		
		b_static := True;
	end
	else if model is T4AModelSkeleton then
	begin
		m_skeleton := T4AModelSkeleton(model);
	
		maler := TSkeletonModelMaler.Create(m_skeleton, 2);
		if Length(m_skeleton.meshes[1]) > 0 then
			maler_lod1 := TSkeletonModelMaler.Create(m_skeleton, 1);
		if Length(m_skeleton.meshes[2]) > 0 then
			maler_lod1 := TSkeletonModelMaler.Create(m_skeleton, 0);
		
		ph := TList.Create;
		MakePhysicsSkeleton(ph, m_skeleton);
		
		b_dynamic := True;
	end 
	else if model is T4AModelSoftbody then
	begin
		m_softbody := T4AModelSoftbody(model);
	
		maler := TSoftbodyModelMaler.Create(m_softbody);
		ph := TList.Create;
		MakePhysicsSoftbody(ph, m_softbody, True);
		
		b_softbody := True;
	end;
end;

destructor TResModel.Destroy;
var
	I : Integer;
begin
	maler.Free;
	maler_lod1.Free;
	maler_lod0.Free;
	
	model.Free;

	if Assigned(ph) then
	begin
		for I := 0 to ph.Count - 1 do
			PHFreeTrimesh(ph[I]);
		ph.Free;
	end;

	inherited Destroy;
end;

function TResModel.GetMaterialSet(const name : String) : Longint;
var
	ret : Longint;
	m : T4AModelMtlsets;
begin
	ret := -1;
	if model is T4AModelMtlsets then
	begin
		m := T4AModelMtlsets(model);
		ret := m.GetMtlsetIndex(name);
	end;

	Result := ret;
end;

procedure LoadMeshes(model : T4AModelSkeleton; const path : String);
var
	list : TStringList;
	I, lod : Longint;
	r : TMemoryReader;
	
	mesh_path : String;
	meshes : array of T4AModelSkinned;
begin
	for lod := 0 to 2 do
	begin
		if (Length(model.meshes[lod]) = 0) and (Length(model.mesh_names) > 0) then
		begin
			list := TStringList.Create;
			list.CommaText := model.mesh_names[lod];
	
			SetLength(meshes, list.Count);
			for I := 0 to List.Count - 1 do
			begin
				if list[I][1] = '.' then
					mesh_path := ExtractFilePath(path) + list[I] + '.mesh' // relative path
				else
					mesh_path := ResourcesPath + '\meshes\' + list[I] + '.mesh'; // absolute path	
			
				r := TMemoryReader.CreateFromFile(mesh_path);
				meshes[I] := Load4AModel(r) as T4AModelSkinned;
				r.Free;
			end;
	
			model.meshes[lod] := meshes;
	
			list.Free;
		end;
	end;
end;

procedure LoadSkeleton(model : T4AModelSkeleton);
var
	path : String;
	r : TMemoryReader;
	skeleton : T4ASkeleton;
begin
	if (model.skeleton = nil) and (model.skeleton_name <> '') then
	begin	
		skeleton := T4ASkeleton.Create;
		
		try
			path := ResourcesPath + '\meshes\' + model.skeleton_name + '.skeleton';
			if FileExists(path) then
			begin
				r := TMemoryReader.CreateFromFile(path);
				skeleton.Load2033(r);
				r.Free;
			end else
			if FileExists(path+'.bin') then
			begin
				r := TMemoryReader.CreateFromFile(path+'.bin');
				skeleton.LoadKonfig(r);
			end;
		except
			on E : Exception do
			begin
				WriteLn('skeleton ', path, ' loading failed: ', E.Message);
				FreeAndNil(skeleton);
			end;
		end;
		
		model.skeleton := skeleton;
	end;	
end;

procedure UpdateBBox(model : T4AModelSkeleton);
var
	I : Integer;
begin
	for I := 0 to Length(model.meshes[0]) - 1 do
	begin
		AABBMerge(model.bbox, model.meshes[0,I].bbox);
	end;
end;

function LoadModel(const name : String) : T4AModel;
var
	path : String;
	r, s : TMemoryReader;
	model : T4AModel;
begin
	path := LevelPath + '\meshes\' + name + '.model';
	if not FileExists(path) then
		path := ResourcesPath + '\meshes\' + name + '.model';
		
	if FileExists(path) then
	begin
		WriteLn('loading model '''+name+'''');
		r := TMemoryReader.CreateFromFile(path);
		try
			model := Load4AModel(r);

			if (model is T4AModelSkeleton) then
			begin
				LoadMeshes(T4AModelSkeleton(model), path);
				LoadSkeleton(T4AModelSkeleton(model));
				UpdateBBox(T4AModelSkeleton(model));
			end;
			
			if model is T4AModelSoftbody then
			begin
				if FileExists(ChangeFileExt(path, '.sftmdl_pc')) then
					s := TMemoryReader.CreateFromFile(ChangeFileExt(path, '.sftmdl_pc'))
				else if FileExists(ChangeFileExt(path, '.sftmdl_xbox')) then
					s := TMemoryReader.CreateFromFile(ChangeFileExt(path, '.sftmdl_xbox'))
				else
				begin
					model.Free;
					raise Exception.Create('soft model data not found! ('+path+')');
				end;
									
				try
					T4AModelSoftbody(model).LoadSoftbodyData(s);
				finally
					s.Free;
				end;
			end;
		except
			on E : Exception do
			begin
				WriteLn('error loading model '''+name+''''#10+E.ClassName+': '+E.Message);
				model := nil;
			end;
		end;
		r.Free;
	end else
	begin
		WriteLn('model not found '''+name+'''');
		model := nil;
	end;
	
	Result := model;
end;

function GetModel(const name : String) : TResModel;
var
	idx : Longint;
	model : T4AModel;
	m : TResModel;
begin
	if not models.Find(name, idx) then
	begin
		model := LoadModel(name);
		if model <> nil then
		begin			
			m := TResModel.Create(name, model);
			models[name] := m;
		end else
			m := nil;
	end else
	begin
		m := models.Data[idx];
		Inc(m.refcnt);
	end;

	Result := m;
end;

procedure FreeModel(r : TResModel);
begin
	if r <> nil then
	begin
		Dec(r.refcnt);
		
		if r.refcnt < 0 then
		  raise Exception.Create('FreeModel: dangling pointer to [' + r.name + ']');
	end;
end;

procedure ClearResources;
var 
	I : Longint;
	list : TList;
begin
	list := TList.Create;
  
	for I := 0 to models.Count-1 do
		if models.Data[I].refcnt < 1 then
			list.Add(models.Data[I]);
      
	for I := 0 to list.Count-1 do
	begin
		models.Remove(TResModel(list[I]).name);
		TResModel(list[I]).Free;
	end;
    
	list.Free;  
	
	ClearUnusedTextures;
end;

constructor TMaterial.Create(const texture_name : String; const shader_name : String; const model_name : String);
var
	bump_name : String;
	detail_name : String;
	
	at : Integer;
	sh : String;
	params : String;
begin
	texture := texture_name;
	shader := shader_name;

	tex := GetTexture(texture_name);

	if useBump then
	begin
		if tex.params <> nil then
			bump_name := tex.params^.bump_name
		else if tex.params2 <> nil then
			bump_name := tex.params2^.bump_name;
		//else
		//	bump_name := tex.name + '_bump';

		if bump_name <> '' then
			tex_bump := GetTexture(bump_name);
	end;

	if useDetail then
	begin
		if tex.params <> nil then
			detail_name := tex.params^.det_name
		else if tex.params2 <> nil then
			detail_name := tex.params2^.det_name;

		// how actually detail texture list work?
		// edit: they associated with mask
		at := Pos(',', detail_name);
		if at <> 0 then
			detail_name := Copy(detail_name, 1, at-1);

		if detail_name <> '' then
			tex_detail := GetTexture(detail_name);
	end;

	at := Pos('@', shader);
	if at <> 0 then
	begin
		sh := Copy(shader, 1, at-1);
		params := Copy(shader, at+1);
	end else
	begin
		sh := shader;
		params := '';
	end;

	alpha := 
		// 2033
		(sh = 'tree\crown') or
		(sh = 'tree\tree') or
		(sh = 'grass\default') or
		(sh = 'grass_bush\default') or
		(sh = 'geometry\default_aref') or
		(sh = 'geometry\skin_aref') or
		// ll
		(sh = 'geometry\default_aref_no_tess') or
		(sh = 'geometry\default_env_aref') or
		(sh = 'geometry\default_wet_aref') or
		(sh = 'geometry\default_wet_aref_no_tess') or
		(sh = 'geometry\default_wet_leaks_aref') or
		(sh = 'geometry\fresnel_aref') or
		(sh = 'geometry\anisotropy_aref') or
		(sh = 'geometry\skin_hair') or
		(sh = 'special\wet_leaks2_aref_no_tess') or
		// redux
		(sh = 'grass\grass_vxa_snow') or
		(sh = 'grass\grass_vxa_fake_sss') or
		(sh = 'grass\grass_vxa_fake_sss_snow') or
		(sh = 'grass_bush\default_fake_sss_snow') or
		(sh = 'tree\crown_snow') or
		(sh = 'tree\tree_vxa_snow_layer') or
		(sh = 'tree\tree_vxa_fake_sss') or
		(sh = 'tree\tree_vxa_fake_sss_snow_layer') or
		(sh = 'special\snow_layer_aref') or
		(sh = 'special\snow_layer_aref_notess') or
		// exodus			 
		(sh = 'tree\tree_vxa') or
		(sh = 'grass\grass_vxa') or
		(sh = 'foliage\grass_vxa') or
		(Pos('use_aref=1', params) > 0);
		
	if
		(shader = 'geometry\selflight')
	then
		fragment_prog := prog[FP_SELFLIGHT];
		
	if
		(shader = 'geometry\selflight2h_aref') or
		(shader = 'geometry\selflight_aref')
	then begin
		alpha := True;
		fragment_prog := prog[FP_SELFLIGHT];
	end;

	if
		(shader = 'geometry\blended') or
		(shader = 'geometry\blended_nofade') or
		(shader = 'geometry\blended_env_nofade') or
		(shader = 'geometry\blended_lit') or
		(shader = 'geometry\wallmark_blend') or
		(shader = 'geometry\blended_distort')
	then begin
		blended := True;
		blend_src := GL_SRC_ALPHA;
		blend_dst := GL_ONE_MINUS_SRC_ALPHA;
	end;
		
	if
		(shader = 'geometry\lightplanes')
	then begin
		blended := True;
		blend_src := GL_SRC_ALPHA;
		blend_dst := GL_ONE;
		
		fragment_prog := prog[FP_SELFLIGHT];
	end;
		
	if
		(shader = 'geometry\wallmark')
	then begin
		blended := True;
		blend_src := GL_DST_COLOR;
		blend_dst := GL_SRC_COLOR;
		
		fragment_prog := prog[FP_WALLMARK];
	end;
	
	//	blended := False;
		
	distort :=
		//(shader = 'geometry\distort') or
		(shader = 'geometry\gasmask_glass') or
		(shader = 'geometry\blend_distort') or
		(shader = 'geometry\blend_distort_nf') or
		(shader = 'special\rain_wall');
		
	visible := not (
		(Copy(model_name, 1, 15) = 'collision_part_') or
		(model_name = 'collision') or
		(model_name = 'coll') or
		(model_name = 'kolizja') or
		(model_name = 'ao') or
		(shader = 'special\invisible')
	);
	
	double_side := Pos('double_sided=1', params) > 0;
end;

destructor TMaterial.Destroy;
begin
	FreeTexture(tex);
	FreeTexture(tex_bump);
	FreeTexture(tex_detail);

	inherited Destroy;
end;

procedure TMaterial.Enable;
var
	bump, detail : Boolean;
	w, h : GLint;

	bump_height : Single;

	det_scale : TVec2;
	det_int : Single;
begin
	bump := useBump and (tex_bump <> nil);
	detail := useDetail and (tex_detail <> nil);

	glActiveTexture(GL_TEXTURE0);
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, tex.texture);

	if alpha then
		glEnable(GL_ALPHA_TEST);
		
	if blended then
	begin
		glEnable(GL_BLEND);
		glBlendFunc(blend_src, blend_dst);
	end;

	if detail then
	begin
		if tex.params <> nil then
		begin
			det_scale.x := tex.params^.det_u_scale;
			det_scale.y := tex.params^.det_v_scale;
			det_int := tex.params^.det_int;
		end else
		if tex.params2 <> nil then
		begin
			det_scale.x := tex.params2^.det_scale_u;
			det_scale.y := tex.params2^.det_scale_v;
			// det_int ?
		end;
	end;
	
	if bump then
	begin
		if tex_bump.params <> nil then
			bump_height := tex_bump.params^.bump_height
		else if tex_bump.params2 <> nil then
			bump_height := tex_bump.params2^.bump_height
		else
			bump_height := 0.008
	end;

	if fragment_prog <> 0 then
	begin
		glEnable(GL_FRAGMENT_PROGRAM_ARB);
		glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, fragment_prog);
	end else
	if bump then
	begin
		glActiveTexture(GL_TEXTURE1);
		glEnable(GL_TEXTURE_2D);
		glBindTexture(GL_TEXTURE_2D, tex_bump.texture);

		glEnable(GL_FRAGMENT_PROGRAM_ARB);

		if detail then
			glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog[FP_BUMPED_DETAIL])
		else
			glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog[FP_BUMPED]);

		glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @w);
		glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @h);
		glProgramLocalParameter4fARB(GL_FRAGMENT_PROGRAM_ARB, 1, 1/w, 1/h, bump_height*100, 0);

		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

		if detail then
		begin
			glActiveTexture(GL_TEXTURE2);
			glEnable(GL_TEXTURE_2D);
			glBindTexture(GL_TEXTURE_2D, tex_detail.texture);

			// TODO: detail scale from exodus params
			glProgramLocalParameter4fARB(GL_FRAGMENT_PROGRAM_ARB, 2,
			det_scale.x, det_scale.y, 1, det_int);
		end;
	end else
	begin
		if detail then
		begin
			glEnable(GL_FRAGMENT_PROGRAM_ARB);
			glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog[FP_DETAIL]);

			glActiveTexture(GL_TEXTURE1);
			glEnable(GL_TEXTURE_2D);
			glBindTexture(GL_TEXTURE_2D, tex_detail.texture);

			// TODO: detail scale from exodus params
			glProgramLocalParameter4fARB(GL_FRAGMENT_PROGRAM_ARB, 1,
			det_scale.x, det_scale.y, 1, det_int);
		end;
	end;
end;

procedure TMaterial.Disable;
var
	bump, detail : Boolean;
begin
	bump := useBump and (tex_bump <> nil);
	detail := useDetail and (tex_detail <> nil);

	if fragment_prog <> 0 then
	begin
		glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
		glDisable(GL_FRAGMENT_PROGRAM_ARB);
	end else		
	if bump then
	begin
		if detail then
		begin
			glActiveTexture(GL_TEXTURE2);
			glDisable(GL_TEXTURE_2D);
		end;

		glActiveTexture(GL_TEXTURE1);
		glDisable(GL_TEXTURE_2D);

		glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
		glDisable(GL_FRAGMENT_PROGRAM_ARB);
	end else
	 if detail then
	 begin
		 glActiveTexture(GL_TEXTURE1);
		 glDisable(GL_TEXTURE_2D);

		 glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
		 glDisable(GL_FRAGMENT_PROGRAM_ARB);
	 end;

	glActiveTexture(GL_TEXTURE0);
	glDisable(GL_TEXTURE_2D);

	if alpha then
		glDisable(GL_ALPHA_TEST);
	if blended then
		glDisable(GL_BLEND);
end;

constructor TMaterialSet.Create(m : TMaterialArray);
var
	I : Longint;
begin
	inherited Create;
	FMaterials := m;
	
	for I := 0 to Length(FMaterials)-1 do
	begin
		if FMaterials[I].blended then
			Inc(n_blended)
		else
		if FMaterials[I].distort then
			Inc(n_distort)
		else
			Inc(n_opaque);
	end;
end;

constructor TMaterialSet.Create(m : TMaterialArray; base : TMaterialSet);
var
	I : Longint;
	mat : TMaterial;
begin
	inherited Create;
	FMaterials := m;
	//SetLength(FMaterials, Length(m));
	//for I := 0 to Length(m)-1 do
	//	FMaterials[I] := m[I];
	self.base := base;
	
	for I := 0 to Length(FMaterials)-1 do
	begin
		if FMaterials[I] <> nil then mat := FMaterials[I]
		else mat := base[I];
		
		if mat.blended then
			Inc(n_blended)
		else
		if mat.distort then
			Inc(n_distort)
		else
			Inc(n_opaque);
	end;
end;

destructor TMaterialSet.Destroy;
var
	I : Longint;
begin
	for I := 0 to Length(FMaterials)-1 do
		FMaterials[I].Free;
		
	inherited;
end;

function TMaterialSet.GetMaterial(idx : Longint) : TMaterial;
begin
	if FMaterials[idx] <> nil then
		GetMaterial := FMaterials[idx]
	else
	if base <> nil then
		GetMaterial := base[idx]
	else
		GetMaterial := nil;
end;

procedure RenderDefault;
begin
	if useWireframe then
		glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
	else
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
end;

procedure RenderWireframe;
begin
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
end;

constructor TStaticModelMaler.Create(m : T4AModelHierrarhy; presets : P4AMaterialSetArray);
var
	I, J : Longint;
	s : T4AModelSimple;
	
	mats : TMaterialArray;

	vsize, isize : GLsizei;
	vpos, ipos : GLsizei;
	
	texture, shader : String;
begin
	inherited Create;

	self.model := m;

	vsize := 0;
	isize := 0;
	
	mesh_count := Length(m.meshes);
	SetLength(mats, Length(m.meshes));
	SetLength(index_count,  Length(m.meshes));
	SetLength(index_offset, Length(m.meshes));
	SetLength(base_vertex,  Length(m.meshes));
	
	for I := 0 to Length(m.meshes) - 1 do
	begin
		s := m.meshes[I];

		mats[I] := TMaterial.Create(s.texture, s.shader, s.name);
		index_count[I]  := Length(s.indices);
		index_offset[I] := Pointer(isize);
		base_vertex[I]  := (vsize div Sizeof(T4AVertStatic)); 

		Inc(vsize, Length(s.vertices)*Sizeof(T4AVertStatic));
		Inc(isize, Length(s.indices)*Sizeof(Word));
	end;
	
	materials := TMaterialSet.Create(mats);
	if presets = nil then
		presets := @m.mtlsets;
	
	SetLength(mtlsets, Length(presets^));
	for I := 0 to Length(mtlsets) - 1 do
	begin
		SetLength(mats, Length(m.meshes));
		
		for J := 0 to Length(m.meshes) - 1 do
		begin
			s := m.meshes[J];
			presets^[I].GetOverride(s.name, texture, shader);
			
			if (texture <> '') or (shader <> '') then
			begin
				if texture = '' then texture := s.texture;
				if shader = '' then shader := s.shader;
				
				mats[J] := TMaterial.Create(texture, shader);
			end else
				mats[J] := nil;
		end;
		
		mtlsets[I] := TMaterialSet.Create(mats, materials);
	end;

	glGenBuffers(2, @buffers);
	glBindBuffer(GL_ARRAY_BUFFER, buffers[1]);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffers[2]);

	glBufferData(GL_ARRAY_BUFFER, vsize, nil, GL_STATIC_DRAW);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, isize, nil, GL_STATIC_DRAW);

	vpos := 0;
	ipos := 0;

	for I := 0 to Length(m.meshes) - 1 do
	begin
		s := m.meshes[I];

		vsize := Length(s.vertices)*Sizeof(T4AVertStatic);
		isize := Length(s.indices)*Sizeof(Word);

		glBufferSubData(GL_ARRAY_BUFFER, vpos, vsize, Pointer(s.vertices));
		glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, ipos, isize, Pointer(s.indices));

		Inc(vpos, vsize);
		Inc(ipos, isize);
	end;

	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	
	// create VAO
	glGenVertexArrays(1, @self.vao);
	glBindVertexArray(self.vao);

	glEnableVertexAttribArrayARB(0);
	glEnableVertexAttribArrayARB(1);
	glEnableVertexAttribArrayARB(2);
	glEnableVertexAttribArrayARB(3);
	glEnableVertexAttribArrayARB(4);
	
	glBindBuffer(GL_ARRAY_BUFFER, buffers[1]);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffers[2]);
	
	glVertexAttribPointerARB(0, 3, GL_FLOAT, GL_FALSE, Sizeof(T4AVertStatic), Pointer(0));
	glVertexAttribPointerARB(1, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertStatic), Pointer(12));
	glVertexAttribPointerARB(2, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertStatic), Pointer(16));
	glVertexAttribPointerARB(3, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertStatic), Pointer(20));
	glVertexAttribPointerARB(4, 2, GL_FLOAT, GL_FALSE, Sizeof(T4AVertStatic), Pointer(24));
	
	glBindVertexArray(0);
end;

destructor TStaticModelMaler.Destroy;
var
	I : Integer;
begin
	materials.Free;
	
	for I := 0 to Length(mtlsets) - 1 do
		mtlsets[I].Free;

	glDeleteVertexArrays(1, @self.vao);
	glDeleteBuffers(2, @buffers);

	inherited Destroy;
end;

procedure TStaticModelMaler.Draw(mtlset : Longint; selected : Boolean; blended : Boolean; distort : Boolean);
var
	ms : TMaterialSet;
begin
	if (mtlset < 0) or (mtlset >= Length(mtlsets)) then 
		ms := materials
	else 
		ms := mtlsets[mtlset];
	
	if (not blended) and (ms.n_opaque < 1) then
		Exit;
	if (blended) and (ms.n_blended < 1) then
		Exit;
	if (distort) and (ms.n_distort < 1) then
		Exit;
	
	glEnableVertexAttribArrayARB(0);
	glEnableVertexAttribArrayARB(1);
	if useBump then glEnableVertexAttribArrayARB(2);
	if useBump then glEnableVertexAttribArrayARB(3);
	if useTextures then glEnableVertexAttribArrayARB(4);
	
	glEnable(GL_VERTEX_PROGRAM_ARB);
	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, prog[VP_STATIC]);

	Draw2(mtlset, selected, blended, distort);

	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, 0);
	glDisable(GL_VERTEX_PROGRAM_ARB);
	
	glDisableVertexAttribArrayARB(0);
	glDisableVertexAttribArrayARB(1);
	if useBump then glDisableVertexAttribArrayARB(2);
	if useBump then glDisableVertexAttribArrayARB(3);
	if useTextures then glDisableVertexAttribArrayARB(4);
	
end;

procedure TStaticModelMaler.Draw2(mtlset : Longint; selected : Boolean; blended : Boolean; distort : Boolean);
var
	I : Integer;
	ms : TMaterialSet;
	material : TMaterial;
	
	s : T4AModelSimple;
begin
	if (mtlset < 0) or (mtlset >= Length(mtlsets)) then 
		ms := materials
	else 
		ms := mtlsets[mtlset];

	if (not blended) and (ms.n_opaque < 1) then
		Exit;
	if (blended) and (ms.n_blended < 1) then
		Exit;
	if (distort) and (ms.n_distort < 1) then
		Exit;
	{	
	glBindVertexArray(self.vao);
	}	
	
	glBindBuffer(GL_ARRAY_BUFFER, buffers[1]);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffers[2]);
	
	glVertexAttribPointerARB(0, 3, GL_FLOAT, GL_FALSE, Sizeof(T4AVertStatic), Pointer(0));
	glVertexAttribPointerARB(1, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertStatic), Pointer(12));
	if useBump then glVertexAttribPointerARB(2, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertStatic), Pointer(16));
	if useBump then glVertexAttribPointerARB(3, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertStatic), Pointer(20));
	if useTextures then glVertexAttribPointerARB(4, 2, GL_FLOAT, GL_FALSE, Sizeof(T4AVertStatic), Pointer(24));
	
	
	for I := 0 to self.mesh_count - 1 do
	begin	
		material := ms[I];
		s := self.model.meshes[I];

		if material.visible and (blended = material.blended) and (distort = material.distort) then
		begin
			if useTextures then
				material.Enable;
	
			// 0, 0, 0, normal_scale (for double siding)
			glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 0, 
			0.0, 0.0, 0.0, 1.0);
	
			glDrawElementsBaseVertex(
				GL_TRIANGLES, 
				self.index_count[I], 
				GL_UNSIGNED_SHORT, 
				self.index_offset[I], 
				self.base_vertex[I]
			);
			
			if material.double_side then
			begin
				glCullFace(GL_FRONT);
				glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 0, 
				0.0, 0.0, 0.0, -1.0);
				glDrawElementsBaseVertex(
					GL_TRIANGLES, 
					self.index_count[I], 
					GL_UNSIGNED_SHORT, 
					self.index_offset[I], 
					self.base_vertex[I]
				);
				glCullFace(GL_BACK);
			end;
	
			if useTextures then
				material.Disable;
		end;

		if (blended = material.blended) and (distort = False) then
		if selected or (flSelected in s.editor_flags) then
		begin
			glEnable(GL_FRAGMENT_PROGRAM_ARB);
			glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog[FP_SELECTED]);
			
			if material.double_side then
				glDisable(GL_CULL_FACE);

			RenderWireframe;

			glDrawElementsBaseVertex(
				GL_TRIANGLES, 
				self.index_count[I], 
				GL_UNSIGNED_SHORT, 
				self.index_offset[I], 
				self.base_vertex[I]
			);

			RenderDefault;
			
			if material.double_side then
				glEnable(GL_CULL_FACE);

			glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
			glDisable(GL_FRAGMENT_PROGRAM_ARB);
		end;
	end;
	
	
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	
	{
	glBindVertexArray(0);
	}
end;

procedure TStaticModelMaler.DrawInstanced2(mtlset, count : Longint; const instances : TInstanceData; blended, distort : Boolean);
var
	I, J : Integer;
	
	ms : TMaterialSet;
	material : TMaterial;
	
	s : T4AModelSimple;
begin
	if (mtlset < 0) or (mtlset >= Length(self.mtlsets)) then 
		ms := self.materials
	else 
		ms := self.mtlsets[mtlset];
			
	if (not blended) and (ms.n_opaque < 1) then
		Exit;
	if (blended) and (ms.n_blended < 1) then
		Exit;
	if (distort) and (ms.n_distort < 1) then
		Exit;
		{
	glBindVertexArray(self.vao);
	}			
	
	glBindBuffer(GL_ARRAY_BUFFER, buffers[1]);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffers[2]);
	
	glVertexAttribPointerARB(0, 3, GL_FLOAT, GL_FALSE, Sizeof(T4AVertStatic), Pointer(0));
	glVertexAttribPointerARB(1, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertStatic), Pointer(12));
	if useBump then glVertexAttribPointerARB(2, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertStatic), Pointer(16));
	if useBump then glVertexAttribPointerARB(3, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertStatic), Pointer(20));
	if useTextures then glVertexAttribPointerARB(4, 2, GL_FLOAT, GL_FALSE, Sizeof(T4AVertStatic), Pointer(24));
	
	
	for I := 0 to self.mesh_count - 1 do
	begin
		material := ms[I];
		s := self.model.meshes[I];
		
		if material.visible and (blended = material.blended) and (distort = material.distort) then
		begin
			if useTextures then material.Enable;

			for J := 0 to count - 1 do
			begin
				glPushMatrix;
				glMultMatrixf(@instances.matrix[J]);
		
				// 0, 0, 0, normal_scale (for double siding)
				glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 0, 
				0.0, 0.0, 0.0, 1.0);
		
				glDrawElementsBaseVertex(
					GL_TRIANGLES, 
					self.index_count[I], 
					GL_UNSIGNED_SHORT, 
					self.index_offset[I], 
					self.base_vertex[I]
				);
				
				if material.double_side then
				begin
					glCullFace(GL_FRONT);
					glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 0, 
					0.0, 0.0, 0.0, -1.0);

					glDrawElementsBaseVertex(
						GL_TRIANGLES, 
						self.index_count[I], 
						GL_UNSIGNED_SHORT, 
						self.index_offset[I], 
						self.base_vertex[I]
					);
				
					glCullFace(GL_BACK);
				end;
			
				glPopMatrix;
			end; // for J := 0 to count - 1

			if useTextures then material.Disable;
		end;
		
		// Draw selection
		glEnable(GL_FRAGMENT_PROGRAM_ARB);
		glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog[FP_SELECTED]);
		
		RenderWireframe;
		
		for J := 0 to count - 1 do
		begin
			if (blended = material.blended) and (distort = False) then
			if instances.selected[J] or (flSelected in s.editor_flags) then
			begin
				glPushMatrix;
				glMultMatrixf(@instances.matrix[J]);
				
				if material.double_side then
					glDisable(GL_CULL_FACE);

				glDrawElementsBaseVertex(
					GL_TRIANGLES, 
					self.index_count[I], 
					GL_UNSIGNED_SHORT, 
					self.index_offset[I], 
					self.base_vertex[I]
				);
				
				if material.double_side then
					glEnable(GL_CULL_FACE);
			
				glPopMatrix;
			end;
		end;
		
		RenderDefault;
		
		glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
		glDisable(GL_FRAGMENT_PROGRAM_ARB);
	end;

	
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	
	{
	glBindVertexArray(0);
	}
end;

constructor TSkeletonModelMaler.Create(m: T4AModelSkeleton; lod : Longint);
var
	I, L : Longint;
begin
	inherited Create;

	model := m;
	L := 2-lod;
	
	SetLength(child, Length(model.meshes[L]));
	for I := 0 to Length(model.meshes[L]) - 1 do
	begin
		child[I] := TSkinnedModelMaler.Create(self, model.meshes[L,I]);
	end;
	
	if model.skeleton <> nil then
	begin
		SetLength(inv_bind_pose, Length(model.skeleton.bones));	
		for I := 0 to Length(inv_bind_pose) - 1 do
		begin
			model.skeleton.GetBoneTransform(I, inv_bind_pose[I]);
			Invert43(inv_bind_pose[I]);
		end;
	end; 
	
	ResetTransform;
end;

destructor TSkeletonModelMaler.Destroy;
var
	I : Integer;
begin
	for I := 0 to Length(child) - 1 do
	begin
		child[I].Free;
	end;

	inherited;
end;

procedure TSkeletonModelMaler.GetTexSubst(var tex : String);
begin
	tex := model.GetTextureSubst(tex);
end;

procedure TSkeletonModelMaler.ResetTransform;
var
	I, max : Integer;
begin
	if model.skeleton <> nil then
		max := Length(model.skeleton.bones)
	else
		max := 255;
		
	for I := 0 to max do
		Identity(bone_xform[I]);
end;

procedure TSkeletonModelMaler.MotionTransform(mot : I4AMotion; t : Single);
var
	I : Integer;
	skl : T4ASkeleton;
	
	xform_local : array[0..255] of TMatrix;
	xform : array[0..255] of TMatrix;
	
	procedure GetTransform(id : Longint; out m : TMatrix);
	begin
		if skl.bones[id].parent_id = -1 then
			Identity(m)
		else
			GetTransform(skl.bones[id].parent_id, m);		
			
		Mul44(m, xform_local[id]);
	end;
begin
	skl := model.skeleton;
	if skl = nil then
		Exit;
		
	// get local transform
	for I := 0 to Length(skl.bones) - 1 do
	begin
		if mot.AffectsBone(I) then
			mot.GetTransform(I, t, xform_local[I])
		else
			skl.GetBoneTransformLocal(I, xform_local[I]);
	end;
	
	// transform each matrix by it's parent
	for I := 0 to Length(skl.bones) - 1 do
	begin
		GetTransform(I, xform[I]);
	end;

	// multiply by inv_bind_pose
	for I := 0 to Length(skl.bones) - 1 do
	begin
		Mul44(xform[I], inv_bind_pose[I]);
		Transpose(bone_xform[I], xform[I]);
	end;
end;

procedure TSkeletonModelMaler.Draw(mtlset : Longint; selected : Boolean; blended : Boolean; distort : Boolean);
var
	I : Integer;
begin
	for I := 0 to Length(child) - 1 do
	begin
		child[I].Draw(mtlset, selected, blended, distort);
	end;
end;

procedure TSkeletonModelMaler.Draw2(mtlset : Longint; selected : Boolean; blended : Boolean; distort : Boolean);
var
	I : Integer;
begin
	for I := 0 to Length(child) - 1 do
	begin
		child[I].Draw2(mtlset, selected, blended, distort);
	end;
end;

procedure TSkeletonModelMaler.DrawInstanced2(mtlset, count : Longint; const instances : TInstanceData; blended, distort : Boolean);
var
	I : Integer;
begin
	for I := 0 to Length(child) - 1 do
	begin
		child[I].DrawInstanced2(mtlset, count, instances, blended, distort);
	end;
end;

constructor TSkinnedModelMaler.Create(_parent : TSkeletonModelMaler; m : T4AModelSkinned);
var
	I, J : Integer;
	s : T4AModelSkinnedMesh;
	
	mats : TMaterialArray;

	vsize, isize : GLsizei;
	vpos, ipos : GLsizei;
	
	texture, shader : String;
begin
	inherited Create;

	parent := _parent;

	model := m;

	vsize := 0;
	isize := 0;

	SetLength(mats, Length(m.meshes));
	for I := 0 to Length(m.meshes) - 1 do
	begin
		s := m.meshes[I];

		texture := s.texture;
		if Assigned(parent) then 
			parent.GetTexSubst(texture);
		mats[I] := TMaterial.Create(texture, s.shader, s.name);

		Inc(vsize, Length(s.vertices)*Sizeof(T4AVertSkin));
		Inc(isize, Length(s.indices)*Sizeof(Word));
	end;
	
	materials := TMaterialSet.Create(mats);
	
	if Assigned(parent) then
	begin
		SetLength(mtlsets, Length(parent.model.mtlsets));
		for I := 0 to Length(mtlsets) - 1 do
		begin
			SetLength(mats, Length(m.meshes));
			
			for J := 0 to Length(m.meshes) - 1 do
			begin
				s := m.meshes[J];
				parent.model.GetMaterialOverride(I, s.name, texture, shader);
				
				if (texture <> '') or (shader <> '') then
				begin
					if texture = '' then texture := s.texture;
					if shader = '' then shader := s.shader;
					
					mats[J] := TMaterial.Create(texture, shader);
				end else
					mats[J] := nil;
			end;
			
			mtlsets[I] := TMaterialSet.Create(mats, materials);
		end;
	end;

	glGenBuffers(2, @buffers);
	glBindBuffer(GL_ARRAY_BUFFER, buffers[1]);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffers[2]);

	glBufferData(GL_ARRAY_BUFFER, vsize, nil, GL_STATIC_DRAW);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, isize, nil, GL_STATIC_DRAW);

	vpos := 0;
	ipos := 0;

	for I := 0 to Length(m.meshes) - 1 do
	begin
		s := m.meshes[I];

		vsize := Length(S.vertices)*Sizeof(T4AVertSkin);
		isize := Length(s.indices)*Sizeof(Word);

		glBufferSubData(GL_ARRAY_BUFFER, vpos, vsize, Pointer(s.vertices));
		glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, ipos, isize, Pointer(s.indices));

		Inc(vpos, vsize);
		Inc(ipos, isize);
	end;

	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
end;

destructor TSkinnedModelMaler.Destroy;
var
	I : Integer;
begin
	materials.Free;
	
	for I := 0 to Length(mtlsets) - 1 do
		mtlsets[I].Free;

	glDeleteBuffers(2, @buffers);

	inherited Destroy;
end;

procedure TSkinnedModelMaler.Draw(mtlset : Longint; selected : Boolean; blended : Boolean; distort : Boolean);
var
	ms : TMaterialSet;
begin
	if mtlset = -1 then ms := materials
	else ms := mtlsets[mtlset];
	
	if (not blended) and (ms.n_opaque < 1) then
		Exit;
	if (blended) and (ms.n_blended < 1) then
		Exit;
	if (distort) and (ms.n_distort < 1) then
		Exit;
		
	glEnableVertexAttribArrayARB(0);
	glEnableVertexAttribArrayARB(1);
	glEnableVertexAttribArrayARB(2);
	glEnableVertexAttribArrayARB(3);
	if useBump then     glEnableVertexAttribArrayARB(4);
	if useBump then     glEnableVertexAttribArrayARB(5);
	if useTextures then glEnableVertexAttribArrayARB(6);

	glEnable(GL_VERTEX_PROGRAM_ARB);
	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, prog[VP_DYNAMIC]);

	Draw2(mtlset, selected, blended, distort);

	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, 0);
	glDisable(GL_VERTEX_PROGRAM_ARB);

	glDisableVertexAttribArrayARB(0);
	glDisableVertexAttribArrayARB(1);
	glDisableVertexAttribArrayARB(2);
	glDisableVertexAttribArrayARB(3);
	if useBump then     glDisableVertexAttribArrayARB(4);
	if useBump then     glDisableVertexAttribArrayARB(5);
	if useTextures then glDisableVertexAttribArrayARB(6);
end;

procedure TSkinnedModelMaler.Draw2(mtlset : Longint; selected : Boolean; blended : Boolean; distort : Boolean);
var
	I, J, K : Longint;
	s : T4AModelSkinnedMesh;
	base_vert : GLint; 
	ipos : PByte;
	scale : Single;
	
	ms : TMaterialSet;
	material : TMaterial;
begin
	if (mtlset < 0) or (mtlset >= Length(mtlsets)) then 
		ms := materials
	else
		ms := mtlsets[mtlset];
	
	if (not blended) and (ms.n_opaque < 1) then
		Exit;
	if (blended) and (ms.n_blended < 1) then
		Exit;
	if (distort) and (ms.n_distort < 1) then
		Exit;
		
	glBindBuffer(GL_ARRAY_BUFFER, buffers[1]);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffers[2]);
	
	glVertexAttribPointerARB(0, 3, GL_SHORT, GL_FALSE, Sizeof(T4AVertSkin), Pointer(0));
	glVertexAttribPointerARB(1, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertSkin), Pointer(8));
	glVertexAttribPointerARB(2, 4, GL_UNSIGNED_BYTE, GL_FALSE, Sizeof(T4AVertSkin), Pointer(20));
	glVertexAttribPointerARB(3, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertSkin), Pointer(24));
	if useBump then glVertexAttribPointerARB(4, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertSkin), Pointer(12));
	if useBump then glVertexAttribPointerARB(5, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertSkin), Pointer(16));
	if useTextures then glVertexAttribPointerARB(6, 2, GL_SHORT, GL_FALSE, Sizeof(T4AVertSkin), Pointer(28));

	base_vert := 0;
	ipos := nil; // 0

	if model.version < MODEL_VER_ARKTIKA1 then
	begin
		scale := 1/2720;
		glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 0, scale, scale, scale, 1.0);
	end;

	for I := 0 to Length(model.meshes) - 1 do
	begin
		s := model.meshes[I];
		material := ms[I];
				
		if model.version >= MODEL_VER_ARKTIKA1 then
		begin
			scale := (1/32767) * s.scale;
			glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 0, scale, scale, scale, 1.0); 
		end;
		
		// setup bones xform
		if parent <> nil then
			for J := 0 to Length(s.bone_ids) - 1 do
			begin
				K := s.bone_ids[J];
				glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 1 + J*3, @parent.bone_xform[K][1,1]);
				glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 2 + J*3, @parent.bone_xform[K][2,1]);
				glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 3 + J*3, @parent.bone_xform[K][3,1]);
				//glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 4 + J*4, @parent.bone_xform[K][4,1]);
			end
		else
			for J := 0 to Length(s.bone_ids) - 1 do
			begin
				glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 1 + J*3, 1.0, 0.0, 0.0, 0.0);
				glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 2 + J*3, 0.0, 1.0, 0.0, 0.0);
				glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 3 + J*3, 0.0, 0.0, 1.0, 0.0);
				//glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 4 + J*4, 0.0, 0.0, 0.0, 1.0);
			end;


		if material.visible and (blended = material.blended) and (distort = material.distort) then
		begin
			if useTextures then
				material.Enable;
			
			glDrawElementsBaseVertex(GL_TRIANGLES, Length(s.indices), GL_UNSIGNED_SHORT, ipos, base_vert);
	
			if useTextures then
				material.Disable;
		end;

		if (blended = material.blended) and (distort = False) then
		if selected or (flSelected in s.editor_flags) then
		begin
			glEnable(GL_FRAGMENT_PROGRAM_ARB);
			glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog[FP_SELECTED]);

			RenderWireframe;

			glDrawElementsBaseVertex(GL_TRIANGLES, Length(s.indices), GL_UNSIGNED_SHORT, ipos, base_vert);
		
			RenderDefault;

			glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
			glDisable(GL_FRAGMENT_PROGRAM_ARB);
		end;

		Inc(base_vert, Length(s.vertices));
		Inc(ipos, Length(s.indices)*Sizeof(Word));
	end;

	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
end;

procedure TSkinnedModelMaler.DrawInstanced2(mtlset, count : Longint; const instances : TInstanceData; blended, distort : Boolean);
var
	I : Longint; // - submesh id
	M : Longint; // - instance id
	s : T4AModelSkinnedMesh;
	base_vert : GLint; 
	ipos : PByte;
	scale : Single;
	
	ms : TMaterialSet;
	material : TMaterial;
	
	procedure SetupBonesTransform;
	var
		J : Longint; 
		//K : Longint;
	begin
		//if instances[M].bone_xform <> nil then
		//	for J := 0 to Length(s.bone_ids) - 1 do
		//	begin
		//		K := s.bone_ids[J];
		//		glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 1 + J*3, @instances[M].bone_xform[K][1,1]);
		//		glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 2 + J*3, @instances[M].bone_xform[K][2,1]);
		//		glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 3 + J*3, @instances[M].bone_xform[K][3,1]);
		//	end
		//else
			for J := 0 to Length(s.bone_ids) - 1 do
			begin
				glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 1 + J*3, 1.0, 0.0, 0.0, 0.0);
				glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 2 + J*3, 0.0, 1.0, 0.0, 0.0);
				glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 3 + J*3, 0.0, 0.0, 1.0, 0.0);
			end;
	end;
	
begin
	if (mtlset < 0) or (mtlset >= Length(mtlsets)) then 
		ms := materials
	else
		ms := mtlsets[mtlset];
	
	if (not blended) and (ms.n_opaque < 1) then
		Exit;
	if (blended) and (ms.n_blended < 1) then
		Exit;
	if (distort) and (ms.n_distort < 1) then
		Exit;
		
	glBindBuffer(GL_ARRAY_BUFFER, buffers[1]);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffers[2]);
	
	glVertexAttribPointerARB(0, 3, GL_SHORT, GL_FALSE, Sizeof(T4AVertSkin), Pointer(0));
	glVertexAttribPointerARB(1, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertSkin), Pointer(8));
	glVertexAttribPointerARB(2, 4, GL_UNSIGNED_BYTE, GL_FALSE, Sizeof(T4AVertSkin), Pointer(20));
	glVertexAttribPointerARB(3, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertSkin), Pointer(24));
	if useBump then glVertexAttribPointerARB(4, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertSkin), Pointer(12));
	if useBump then glVertexAttribPointerARB(5, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertSkin), Pointer(16));
	if useTextures then glVertexAttribPointerARB(6, 2, GL_SHORT, GL_FALSE, Sizeof(T4AVertSkin), Pointer(28));

	base_vert := 0;
	ipos := nil; // 0

	if model.version < MODEL_VER_ARKTIKA1 then
	begin
		scale := 1/2720;
		glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 0, scale, scale, scale, 1.0);
	end;

	for I := 0 to Length(model.meshes) - 1 do
	begin
		s := model.meshes[I];
		material := ms[I];
		
		SetupBonesTransform;
				
		if model.version >= MODEL_VER_ARKTIKA1 then
		begin
			scale := (1/32767) * s.scale;
			glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, 0, scale, scale, scale, 1.0); 
		end;

		if material.visible and (blended = material.blended) and (distort = material.distort) then
		begin	
			if useTextures then material.Enable;
			
			for M := 0 to count - 1 do
			begin
				//SetupBonesTransform;
			
				glPushMatrix;
				glMultMatrixf(@instances.matrix[M]);
				
				glDrawElementsBaseVertex(GL_TRIANGLES, Length(s.indices), GL_UNSIGNED_SHORT, ipos, base_vert);
			
				glPopMatrix;
			end;
	
			if useTextures then material.Disable;
		end;
		
		// Draw selection
		for M := 0 to count - 1 do
		begin
			if (blended = material.blended) and (distort = False) then
			if instances.selected[M] or (flSelected in s.editor_flags) then
			begin
				//SetupBonesTransform;
			
				glPushMatrix;
				glMultMatrixf(@instances.matrix[M]);
			
				glEnable(GL_FRAGMENT_PROGRAM_ARB);
				glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog[FP_SELECTED]);
				
				RenderWireframe;

				glDrawElementsBaseVertex(GL_TRIANGLES, Length(s.indices), GL_UNSIGNED_SHORT, ipos, base_vert);
		
				RenderDefault;
				
				glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
				glDisable(GL_FRAGMENT_PROGRAM_ARB);
			
				glPopMatrix;
			end;
		end;

		Inc(base_vert, Length(s.vertices));
		Inc(ipos, Length(s.indices)*Sizeof(Word));
	end;

	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
end;

constructor TSoftbodyModelMaler.Create(m : T4AModelSoftbody);
var
	I : Longint;
begin
	inherited Create;
	
	model := m;
	material := TMaterial.Create(model.texture, model.shader);
	
	center := m.vertices[0].point;
	for I := 1 to Length(m.vertices) - 1 do
	begin
		center.x := center.x + m.vertices[I].point.x;
		center.y := center.y + m.vertices[I].point.y;
		center.z := center.z + m.vertices[I].point.z;
	end;
	
	center.x := center.x / Length(m.vertices);
	center.y := center.y / Length(m.vertices);
	center.z := center.z / Length(m.vertices);
end;

destructor TSoftbodyModelMaler.Destroy;
begin
	material.Free;
	
	inherited;
end;

procedure TSoftbodyModelMaler.Draw(mtlset : Longint; selected : Boolean; blended : Boolean; distort : Boolean);
	
	procedure DoDraw;
	var
		I : Longint;
		v : P4AVertSoft;
	begin
		glBegin(GL_TRIANGLES);
		
		for I := 0 to Length(model.indices) - 1 do
		begin
			v := @model.vertices[model.indices[I]];
			
			glNormal3fv(@v^.normal);
			glTexcoord2f(v^.tc.x / 2048, v^.tc.y / 2048);
			//glVertex3fv(@v^.point);
			
			glVertex3f(v^.point.x - center.x, v^.point.y - center.y, v^.point.z - center.z);
		end;
	
		for I := Length(model.indices) - 1 downto 0 do
		begin
			v := @model.vertices[model.indices[I]];
			
			glNormal3f(-v^.normal.x, -v^.normal.y, -v^.normal.z);
			glTexcoord2f(v^.tc.x / 2048, v^.tc.y / 2048);
			//glVertex3fv(@v^.point);
			
			glVertex3f(v^.point.x - center.x, v^.point.y - center.y, v^.point.z - center.z);
		end;
		
		glEnd;
	end;
begin
	if material.visible and (blended = material.blended) and (distort = material.distort) then
	begin
		if useTextures then
			material.Enable;
		
		DoDraw;
		
		if useTextures then
			material.Disable;
	end;
	
	if selected and (blended = material.blended) and (distort = False) then
	begin
		glEnable(GL_FRAGMENT_PROGRAM_ARB);
		glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog[FP_SELECTED]);
		
		RenderWireframe;
		DoDraw;
		RenderDefault;
		
		glDisable(GL_FRAGMENT_PROGRAM_ARB);
	end;
end;

procedure TSoftbodyModelMaler.Draw2(mtlset : Longint; selected : Boolean; blended : Boolean; distort : Boolean);
begin
	Draw(mtlset, selected, blended, distort);
end;

constructor TLevelMaler.Create(l : T4ALevel);
var
	I : Integer;
begin
	level := l;

	SetLength(materials, Length(level.materials));
	for I := 0 to Length(materials) - 1 do
		materials[I] := TMaterial.Create(level.materials[I].texture, level.materials[I].shader);

	glGenBuffers(2, @buffers[1]);

	glBindBuffer(GL_ARRAY_BUFFER, buffers[1]);
	glBufferData(GL_ARRAY_BUFFER, Length(level.vbuffer)*Sizeof(T4AVertLevel),
	P4AVertLevel(level.vbuffer), GL_STATIC_DRAW);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffers[2]);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(level.ibuffer)*Sizeof(Word),
	PWord(level.ibuffer), GL_STATIC_DRAW);

	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	
	visible := TList.Create;
end;

destructor TLevelMaler.Destroy;
var
	I : Integer;
begin
	for I := 0 to Length(materials) - 1 do
		materials[I].Free;

	glDeleteBuffers(2, @buffers[1]);
	
	visible.Free;
end;

procedure TLevelMaler.Draw;
var
	I : Longint;
	m : T4AModelRef;
begin
	glBindBuffer(GL_ARRAY_BUFFER, buffers[1]);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffers[2]);

	glEnableVertexAttribArrayARB(0);
	glEnableVertexAttribArrayARB(1);
	if useBump then glEnableVertexAttribArrayARB(2);
	if useBump then glEnableVertexAttribArrayARB(3);
	if useTextures then glEnableVertexAttribArrayARB(4);
	
	glVertexAttribPointerARB(0, 3, GL_FLOAT, GL_FALSE, Sizeof(T4AVertLevel), Pointer(0));
	glVertexAttribPointerARB(1, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertLevel), Pointer(12));
	if useBump then glVertexAttribPointerARB(2, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertLevel), Pointer(16));
	if useBump then glVertexAttribPointerARB(3, 4, GL_UNSIGNED_BYTE, GL_TRUE, Sizeof(T4AVertLevel), Pointer(20));
	if useTextures then glVertexAttribPointerARB(4, 2, GL_SHORT, GL_FALSE, Sizeof(T4AVertLevel), Pointer(24));

	glEnable(GL_VERTEX_PROGRAM_ARB);
	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, prog[VP_LEVEL]);

	visible.Clear;

	for I := 0 to Length(level.sectors) - 1 do
		AddModel(level.visuals[level.sectors[I]]);
		
	// draw opaque
	for I := 0 to visible.Count - 1 do
	begin
		m := T4AModelRef(visible[I]);
		if (materials[m.shaderid].blended = False) and not (flDeleted in m.editor_flags) then
			DrawSimple(m);
	end;
	
	// draw blended
	glDepthMask(GL_FALSE);
	
	for I := 0 to visible.Count - 1 do
	begin
		m := T4AModelRef(visible[I]);
		if (materials[m.shaderid].blended = True) and not (flDeleted in m.editor_flags) then
			DrawSimple(m);
	end;
	
	glDepthMask(GL_TRUE);
	
	// draw selected (for Model Editor)
	glEnable(GL_FRAGMENT_PROGRAM_ARB);
	glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog[FP_SELECTED]);
		
	RenderWireframe;	
		
	for I := 0 to visible.Count - 1 do
	begin
		m := T4AModelRef(visible[I]);
		if flSelected in m.editor_flags then
			DrawSelected(m);
	end;
	
	RenderDefault;
	
	glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
	glDisable(GL_FRAGMENT_PROGRAM_ARB);
	//

	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, 0);
	glDisable(GL_VERTEX_PROGRAM_ARB);

	glDisableVertexAttribArrayARB(0);
	glDisableVertexAttribArrayARB(1);
	if useBump then glDisableVertexAttribArrayARB(2);
	if useBump then glDisableVertexAttribArrayARB(3);
	if useTextures then glDisableVertexAttribArrayARB(4);

	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
end;

procedure TLevelMaler.AddModel(m : T4AModel);
begin
	case m.modeltype of
		MODEL_TYPE_NORMAL:		AddSimple(T4AModelRef(m));
		MODEL_TYPE_HIERRARHY:	AddHierrarhy(T4AModelHierrarhyL(m));
	end;
end;

procedure TLevelMaler.AddHierrarhy(m : T4AModelHierrarhyL);
var
	v : T4AModel;
	I : Integer;
begin
	for I := 0 to Length(m.meshes) - 1 do
	begin
		v := level.visuals[m.meshes[I]];
		if AABBVisible(frustum, v.bbox) then
			AddModel(v);
	end;
end;

procedure TLevelMaler.AddSimple(m : T4AModelRef);
begin
	visible.Add(m);
end;

procedure TLevelMaler.DrawSimple(m : T4AModelRef);
begin
	if useTextures then
		materials[m.shaderid].Enable;

	glDrawRangeElementsBaseVertex(GL_TRIANGLES, 0, m.indexcount div 3, m.indexcount, GL_UNSIGNED_SHORT, Pointer(m.indexoffset*Sizeof(Word)), m.vertexoffset);

	if useTextures then
		materials[m.shaderid].Disable;
end;

procedure TLevelMaler.DrawSelected(m : T4AModelRef);
begin
	glDrawRangeElementsBaseVertex(GL_TRIANGLES, 0, m.indexcount div 3, m.indexcount, GL_UNSIGNED_SHORT, Pointer(m.indexoffset*Sizeof(Word)), m.vertexoffset);
end;

constructor TLevel2Maler.Create(l : T4ALevel2);
var
	I : Integer;
begin
	inherited Create;

	SetLength(sublevels, Length(l.sublevels));
	for I := 0 to Length(sublevels) - 1 do
		sublevels[I] := TLevelMaler.Create(l.sublevels[I]);
end;

destructor TLevel2Maler.Destroy;
var
	I : Integer;
begin
	for I := 0 to Length(sublevels) - 1 do
		sublevels[I].Free;

	inherited;
end;

procedure TLevel2Maler.Draw;
var
	I : Integer;
begin
	for I := 0 to Length(sublevels) - 1 do
		sublevels[I].Draw;
end;

var
	program_options : String = '';
	
function LoadProgramString(const fn : String) : String;
var
	f : Text;
	str : String;
	
	line : String;
	line_trim : String;
	I : Longint;
	
	function Cond(const condition : String) : Boolean;
	begin
		if condition[1] = '!' then
			Cond := Pos(Copy(condition, 2), program_options) = 0
		else
			Cond := Pos(condition, program_options) > 0;
	end;
begin
	Assign(f, fn);
	Reset(f);
	
	str := '';
	
	while not EOF(f) do
	begin
		ReadLn(f, line);
		
		line_trim := Trim(line);
		if Copy(line_trim, 1, 3) = 'if(' then
		begin
			I := 4;
			while (line_trim[I] <> ')') and (I <= Length(line_trim)) do
				Inc(I);
				
			if Cond(Copy(line_trim, 4, I-4)) then
				str := str + Copy(line_trim, I+1) + #10
			else
				str := str + #10;
				
		end else
			str := str + line + #10;
			
	end;
	
	LoadProgramString := str;
	
	Close(f);
end;

procedure LoadVP(prog : GLuint; const fn : String);
var
	err : PAnsiChar;
	str : String;
begin
	str := LoadProgramString(fn);

	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, prog);
	glProgramStringARB(GL_VERTEX_PROGRAM_ARB, GL_PROGRAM_FORMAT_ASCII_ARB, Length(str), @str[1]);

	if glGetError() <> GL_NO_ERROR then
	begin
		err := glGetString(GL_PROGRAM_ERROR_STRING_ARB);
		IupMessagef('Error', 'Vertex program ''%s'' compilation failed. Error:'#10'%s',
		PAnsiChar(fn), err);
	end;
end;

procedure LoadFP(prog : GLuint; const fn : String);
var
	err : PAnsiChar;
	str : String;
begin
	str := LoadProgramString(fn);

	glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog);
	glProgramStringARB(GL_FRAGMENT_PROGRAM_ARB, GL_PROGRAM_FORMAT_ASCII_ARB, Length(str), @str[1]);

	if glGetError() <> GL_NO_ERROR then
	begin
		err := glGetString(GL_PROGRAM_ERROR_STRING_ARB);
		IupMessagef('Error', 'Fragment program ''%s'' compilation failed. Error:'#10'%s',
		PAnsiChar(fn), err);
	end;
end;

procedure ReloadGLPrograms;
begin
	program_options := '';
	if showAO then
		program_options := program_options + 'SHOW_AO ';
	if useBump then
		program_options := program_options + 'USE_BUMP ';
	if useTextures then
		program_options := program_options + 'USE_TEXTURES ';
	if useWeather then
		program_options := program_options + 'WEATHER ';

	try
		LoadVP(prog[VP_STATIC],        'editor_data\shaders\vp_static.txt');
		LoadVP(prog[VP_DYNAMIC],       'editor_data\shaders\vp_dynamic.txt');
		LoadFP(prog[FP_SELECTED],      'editor_data\shaders\fp_selected.txt');
		LoadFP(prog[FP_BUMPED],        'editor_data\shaders\fp_bumped.txt');
		LoadVP(prog[VP_LEVEL],         'editor_data\shaders\vp_level.txt');
		LoadFP(prog[FP_DETAIL],        'editor_data\shaders\fp_detail.txt');
		LoadFP(prog[FP_BUMPED_DETAIL], 'editor_data\shaders\fp_bumped_detail.txt');
		LoadFP(prog[FP_WALLMARK],      'editor_data\shaders\fp_wallmark.txt');
		LoadFP(prog[FP_SELFLIGHT],     'editor_data\shaders\fp_selflight.txt');
		LoadFP(prog[FP_SCREEN_IMAGE],  'editor_data\shaders\fp_screen_image.txt');
	except
		on E: EFOpenError do
			IupMessage('Error', PAnsiChar(E.Message));
	end;

	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, 0);
	glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
end;

var
	flags : GLuint;

// must be called after MakeCurrent
procedure InitializeRender;

	procedure ShowInfo;
	var
		maxVertexAttribs,
		maxLocalParams,
		maxEnvParams,
		maxMatrices,
		maxTemporaries,
		maxParams : Longint;
	begin
		WriteLn('OpenGL Vendor, Renderer, Version strings:');
		WriteLn(#9, glGetString(GL_VENDOR));
		WriteLn(#9, glGetString(GL_RENDERER));
		WriteLn(#9, glGetString(GL_VERSION));
		WriteLn;
	
		glGetIntegerv(GL_MAX_VERTEX_ATTRIBS_ARB, @maxVertexAttribs);
		glGetIntegerv(GL_MAX_PROGRAM_MATRICES_ARB, @maxMatrices);
		glGetProgramivARB(GL_VERTEX_PROGRAM_ARB, GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB, @maxLocalParams);
		glGetProgramivARB(GL_VERTEX_PROGRAM_ARB, GL_MAX_PROGRAM_ENV_PARAMETERS_ARB,   @maxEnvParams);
		glGetProgramivARB(GL_VERTEX_PROGRAM_ARB, GL_MAX_PROGRAM_TEMPORARIES_ARB,      @maxTemporaries);
		glGetProgramivARB(GL_VERTEX_PROGRAM_ARB, GL_MAX_PROGRAM_PARAMETERS_ARB,       @maxParams);
		
		WriteLn ( 'Max vertex attributes  : ', maxVertexAttribs );
		WriteLn ( 'Max local parameters   : ', maxLocalParams   );
		WriteLn ( 'Max env. parameters    : ', maxEnvParams     );
		WriteLn ( 'Max program matrices   : ', maxMatrices      );
		WriteLn ( 'Max program temporaries: ', maxTemporaries   );
		WriteLn ( 'Max parameters         : ', maxParams        );
	end;
	
	procedure MakeRoundFlag(r, g, b, _r, _g, _b : Single);
	var
		I : Longint;
	begin
		// fill
		glColor3f(r, g, b);
		glBegin(GL_TRIANGLE_FAN);
		glVertex3f(0.0, 0.5, 0.0);
		for I := 1 to 10 do
			glVertex3f(0.0, Cos(PI*I/10) * 0.125 + 0.375, Sin(PI*I/10) * 0.2);
		glEnd;
		glBegin(GL_TRIANGLE_FAN);
		glVertex3f(0.0, 0.25, 0.0);
		for I := 1 to 10 do
			glVertex3f(0.0, 0.375 - Cos(PI*I/10) * 0.125, Sin(PI*I/10) * 0.2);
		glEnd;
		
		// outline
		glColor3f(_r, _g, _b);
		glBegin(GL_LINES);
		glVertex3f(0.0, 0.0, 0.0);
		glVertex3f(0.0, 0.5, 0.0);
		glEnd;
		glBegin(GL_LINE_LOOP);
		glVertex3f(0.0, 0.5, 0.0);
		for I := 1 to 10 do
			glVertex3f(0.0, Cos(PI*I/10) * 0.125 + 0.375, Sin(PI*I/10) * 0.2);
		glEnd;	
		
		glColor3f(1, 1, 1);
	end;
	
begin
	InitializeGLExtensions;

	glGenProgramsARB(16, @prog);
	ReloadGLPrograms;
	
	flags := glGenLists(Integer(High(TFlagColor)) + 1);
	
	glNewList(flags+Integer(fclWhite), GL_COMPILE);
	glColor3f(1, 1, 1);
	glBegin(GL_LINES);
	glVertex3f(0.0, 0.0, 0.0);
	glVertex3f(0.0, 0.5, 0.0);
	glEnd;
	glBegin(GL_TRIANGLE_STRIP);
	glVertex3f(0.0, 0.5, 0.0);
	glVertex3f(0.0, 0.375, -0.25);
	glVertex3f(0.0, 0.250, 0.0);
	glVertex3f(0.0, 0.5, 0.0);
	glEnd;
	glEndList;
	
	glNewList(flags+Integer(fclYellow), GL_COMPILE);
	glColor3f(1, 1, 0);
	glBegin(GL_LINES);
	glVertex3f(0.0, 0.0, 0.0);
	glVertex3f(0.0, 0.5, 0.0);
	glEnd;
	glBegin(GL_TRIANGLE_STRIP);
	glVertex3f(0.0, 0.5, 0.0);
	glVertex3f(0.0, 0.375, -0.25);
	glVertex3f(0.0, 0.250, 0.0);
	glVertex3f(0.0, 0.5, 0.0);
	glEnd;
	glColor3f(1, 1, 1);
	glEndList;

	glNewList(flags+Integer(fclRedRound), GL_COMPILE);
	MakeRoundFlag(0.6, 0, 0, 1, 0, 0);
	glEndList;
	
	glNewList(flags+Integer(fclGreenRound), GL_COMPILE);
	MakeRoundFlag(0, 0.5, 0, 0, 0.9, 0);
	glEndList;
	
	glNewList(flags+Integer(fclBlueRound), GL_COMPILE);
	MakeRoundFlag(0, 0, 0.6, 0, 0, 1);
	glEndList;
	
	glNewList(flags+Integer(fclRedRoundHL), GL_COMPILE);
	MakeRoundFlag(0.9, 0.3, 0.3, 1, 1, 1);
	glEndList;
	
	glNewList(flags+Integer(fclGreenRoundHL), GL_COMPILE);
	MakeRoundFlag(0.2, 0.8, 0.2, 1, 1, 1);
	glEndList;
	
	glNewList(flags+Integer(fclBlueRoundHL), GL_COMPILE);
	MakeRoundFlag(0.3, 0.3, 0.9, 1, 1, 1);
	glEndList;
	
	glNewList(flags+Integer(fclWhiteRound), GL_COMPILE);
	MakeRoundFlag(1, 1, 1, 1, 1, 1);
	glEndList;
	
	common_flags.InitializeFlags;
	
	ShowInfo;
end;

procedure DrawFlag(clr : TFlagColor);
begin
	case clr of
		fclWhite:  glCallList(flags+Integer(fclWhite));
		fclYellow: glCallList(flags+Integer(fclYellow));
		fclRedRound: glCallList(flags+Integer(fclRedRound));
		fclGreenRound: glCallList(flags+Integer(fclGreenRound));
		fclBlueRound: glCallList(flags+Integer(fclBlueRound));
		fclRedRoundHL: glCallList(flags+Integer(fclRedRoundHL));
		fclGreenRoundHL: glCallList(flags+Integer(fclGreenRoundHL));
		fclBlueRoundHL: glCallList(flags+Integer(fclBlueRoundHL));
		fclWhiteRound: glCallList(flags+Integer(fclWhiteRound));
	end;
end;

initialization
	models			:= TResModelMap.Create;
	models.Sorted   := True;
end.
