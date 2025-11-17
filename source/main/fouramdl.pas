unit fouramdl;

interface
uses vmath, chunkedFile, skeleton;

const
	MODEL_TYPE_NORMAL       = 0;
	MODEL_TYPE_HIERRARHY    = 1;
	MODEL_TYPE_SKELETON     = 2;
	MODEL_TYPE_ANIMATED     = 3;
	MODEL_TYPE_SKINNED      = 4;
	MODEL_TYPE_SKINNED_MESH = 5;
	MODEL_TYPE_SOFTBODY     = 8;
	
	MODEL_VF_STATIC         = 2; // -?
	MODEL_VF_LEVEL          = 3;
	
	MODEL_VER_2033          = 8;
	MODEL_VER_LL            = 21; // also may be 20
	MODEL_VER_REDUX         = 23;
	MODEL_VER_ARKTIKA1      = 34;
	MODEL_VER_EXODUS        = 44;
	MODEL_VER_44            = 44; // Exodus

type
	T4AOBB = record
		rot : TMatrix33;
		offset : TVec3;
		half_size : TVec3;
	end;

	T4AVertStatic = record
		point : TVec3;
		normal, tangent, binormal : Longword; // normal.w is AO
		tc : TVec2;
	end;
	P4AVertStatic = ^T4AVertStatic;

	T4AVertSkin = record
		point : TVec4S16;
		normal, tangent, binormal : Longword;
		bones : array[0..3] of Byte;
		weights : array[0..3] of Byte;
		tc : TVec2S16;
	end;
	P4AVertSkin = ^T4AVertSkin;

	T4AVertLevel = record
		point : TVec3;
		normal, tangent, binormal : Longword;
		tc : TVec2S16;
		lm : TVec2S16; // lightmap tc's
	end;
	P4AVertLevel = ^T4AVertLevel;
	
	T4AVertSoft = record
		point : TVec3;
		unk1, unk2 : Longword;
		normal : TVec3;
		tc : TVec2S16;
	end;
	P4AVertSoft = ^T4AVertSoft;
	
	T4AVertShadow = record
		point : TVec3;
		tc : TVec2S16;
	end;
	P4AVertShadow = ^T4AVertShadow;

	T4AModelFlagsEnum = (flSelected,flNoCollision,flDeleted);
	T4AModelFlags = set of T4AModelFlagsEnum;
	
	// in original 4A engine they're called presets ;)
	T4AMaterialSet = record
		name : String;
		hit_preset : String;
		voice : String;
		flags : Longword;
		materials : array of record
			surface : String;
			new_texture : String;
			new_shader : String;
		end;
		
		procedure GetOverride(const name : String; out texture, shader : String);
	end;
	
	T4AMaterialSetArray = array of T4AMaterialSet;
	P4AMaterialSetArray = ^T4AMaterialSetArray;

	T4AModel = class // base
		version, modeltype : Byte;
		shaderid : Word;
		bbox : TAABB;
		bsphere : TSphere;
		scale : Single;
		
		unkwn_parm_1 : Longword;
		unkwn_parm_2 : Single;
		
		editor_flags : T4AModelFlags;
		
		constructor Create;
		
		procedure Load(reader : TMemoryReader); virtual;
		procedure Save(w : TMemoryWriter); virtual;
		
		procedure LoadFromFile(const fn : String);
		procedure SaveToFile(const fn : String);
	end;
	
	T4AModelMtlsets = class(T4AModel) // model with mtlsets
		mtlsets : T4AMaterialSetArray;
		
		procedure LoadMaterialSets(r : TMemoryReader);
		procedure SaveMaterialSets(w : TMemoryWriter);
		
		function GetMtlsetIndex(const name : String) : Longint;
		procedure GetMaterialOverride(idx : Longint; const surface : String; out texture, shader : String);
	end;

	T4AModelSimple = class(T4AModel)
		texture, shader, material, name : String;
		vertexformat : Longword;
		vertices : array of T4AVertStatic;
		indices : array of Word;
		
		constructor Create;
		
		procedure Load(reader : TMemoryReader); override;
		procedure Save(w : TMemoryWriter); override;
	end;
	
	T4AModelHierrarhy = class(T4AModelMtlsets)
		meshes : array of T4AModelSimple;
		
		lod1, lod0 : T4AModelHierrarhy;
		
		constructor Create;
		destructor Destroy; override;
		
		procedure Load(reader : TMemoryReader); override;
		procedure Save(w : TMemoryWriter); override;
	end;
	
	T4AModelRef = class(T4AModel)
		vertexformat, vertexoffset, vertexcount : Longword;
		shadow_vcount : Longword; // Exodus. Goes right after ordinary vertices in same buffer
		indexoffset, indexcount : Longword;
		shadow_icount : Longword; // Exodus. Goes right after ordinary indices in same buffer
		
		// same as in LEVEL_CHUNK_MATERIALS, Exodus duplicates this information here for some reason
		texture, shader, material, name : String;
		flags : Longword;
		
		procedure Load(reader : TMemoryReader); override;
		procedure Save(w : TMemoryWriter); override;
	end;

	T4AModelHierrarhyL = class(T4AModel)
		meshes : array of Longword;
		
		constructor Create;
		
		procedure Load(reader : TMemoryReader); override;
		procedure Save(w : TMemoryWriter); override;
	end;

	T4AModelSkinnedMesh = class(T4AModel)
		texture, shader, material, name : String;
		aux_params : Word; // really unknown
		bone_ids : array of Byte;
		bone_obb : array of T4AOBB;
		vertices : array of T4AVertSkin;
		indices : array of Word;
		indices2 : array of Word;
		
		constructor Create;
		
		procedure Load(reader : TMemoryReader); override;
		procedure Save(w : TMemoryWriter); override;
		
		function GetPointScale : Single;
	end;

	T4AModelSkinned = class(T4AModel)
		skeleton_crc : Longword;
		meshes : array of T4AModelSkinnedMesh;
		
		constructor Create;
		destructor Destroy; override;
		
		procedure Load(reader : TMemoryReader); override;
		procedure Save(w : TMemoryWriter); override;
	end;

	T4AModelSkeleton = class(T4AModelMtlsets)
		mesh_names : array of String;
		meshes : array[0..2] of array of T4AModelSkinned;
		
		skeleton_name : String;
		texture_subst : String;
		
		skeleton : T4ASkeleton;
		
		constructor Create;
		destructor Destroy; override;
		
		procedure Load(reader : TMemoryReader); override;
		procedure Save(w : TMemoryWriter); override;
		
		function GetTextureSubst(const src : String) : String;
	end;
	
	T4AModelSoftbody = class(T4AModel)
		texture, shader, material : String;
		
		// unknown parameters
		param1 : Boolean;
		param2 : Single;
		param3 : Single;
		param4 : Single;
		param5 : Single;
		param6 : Boolean;
		param7 : Single;
		param8 : Boolean;
		param9 : Single;
		
		indices : array of Word;
		vertices : array of T4AVertSoft;
		
		procedure Load(reader : TMemoryReader); override;
		procedure Save(w : TMemoryWriter); override;
		
		procedure LoadSoftbodyData(r : TMemoryReader);
	end;

function Load4AModel(r : TMemoryReader; level : Boolean = False) : T4AModel;
procedure SetModelVersion(m : T4AModel; ver : Byte);

const
	LEVEL_VER_2033        = 16;
	LEVEL_VER_ARKTIKA1    = 17;
	LEVEL_VER_EXODUS      = 19;
	
const
	LEVEL_LIGHTMAPS_VER_0 = 0; // Arktika.1
	LEVEL_LIGHTMAPS_VER_1 = 1; // Exodus

type
	T4APortal = record
		points : array of TVec3;
		name : String;
	end;

	T4ALevel = class
		filename : String;
		
		version : Word;
		quality : Word; // 1 - draft, 0 - high
		checksum : Longword;
		
		visuals : array of T4AModel;
		vbuffer : array of T4AVertLevel;
		ibuffer : array of Word;
		materials : array of record
			shader, texture, material : String;
			flags : Longword; // lower 16-bits is sector index
			                  // upper 16-bits is flags (0x1 means surface is lightmapped)
		end;
		sectors : array of Longword;
		
		portals : array of T4APortal;
		sound_occlusion_version : Longword;
		
		constructor Create;
		destructor Destroy; override;
		
		procedure Load(const path : String);
		procedure SaveTo(const path : String; forXBox : Boolean = False);
		
		function AddMaterial(s,t,m : String; flags : Longword = 0) : Longint;
		function AddVertexBuffer(const Buffer; count : Longword) : Longword;
		function AddIndexBuffer(const Buffer; count : Longword) : Longword;
		function AddVisual(visual : T4AModel) : Longword;
		function AddSector(id : Longword) : Longword;
		
		procedure LoadGeom(const path : String);
		procedure LoadGeomXBox(const path : String);
		
		procedure SaveGeom(const path : String);
		procedure SaveGeomXBox(const path : String);
	end;

	T4ALevel2 = class
		sublevels : array of T4ALevel;
		
		destructor Destroy; override;
		
		procedure Load(const path : String);
	end;

implementation
uses classes, Sysutils;

const
	MODEL_CHUNK_HEADER        = 1;
	MODEL_CHUNK_TEXTURE       = 2;
	MODEL_CHUNK_VERTICES      = 3;
	MODEL_CHUNK_INDICES       = 4;
	MODEL_CHUNK_CHILD         = 9;
	MODEL_CHUNK_CHILD_L       = 10;
	MODEL_CHUNK_LOD1          = 11;
	MODEL_CHUNK_LOD0          = 12;
	MODEL_CHUNK_GEOMREF       = 21;
	
	MODEL_CHUNK_SKIN_VERTICES = 5;
	MODEL_CHUNK_SKELETON_CRC  = 13;
	MODEL_CHUNK_MESHES        = 15;
	MODEL_CHUNK_MESHES_FN     = 16;
	MODEL_CHUNK_SKELETON_FN   = 20;
	MODEL_CHUNK_SKELETON      = 24;
	MODEL_CHUNK_PHYSX_FN      = 25;
	MODEL_CHUNK_TEXTURE_SUBST = 29;
	
	MODEL_CHUNK_MTL_SETS      = 32;
	
	MODEL_CHUNK_COMMENT       = 36;
	//MODEL_CHUNK_UNKNOWN1    = 37; // always contain 24 zero bytes
	//MODEL_CHUNK_UNKNOWN2    = 39; // always contain 24 zero bytes
	//MODEL_CHUNK_UNKNOWN3    = 40; // 4 zero bytes
	//MODEL_CHUNK_UNKNOWN4    = 41; // zero length
  
const
	LEVEL_CHUNK_VERSION   = 1;
	LEVEL_CHUNK_MATERIALS = 2;
	LEVEL_CHUNK_VISUALS   = 3;
	LEVEL_CHUNK_SECTORS   = 8;
	LEVEL_CHUNK_VBUFFER   = 9;
	LEVEL_CHUNK_IBUFFER   = 10;
	LEVEL_CHUNK_CHECKSUM  = 11;

type
	T4AHeader = record
		version : Byte;
		modeltype : Byte;
		shaderid : Word;
		bbox : TAABB;
		bsphere : TSphere;
		unknown : array[1..5] of Longword;
	end;

procedure T4AMaterialSet.GetOverride(const name : String; out texture, shader : String);
var I : Longint;
begin
	for I := 0 to Length(materials) - 1 do
		if materials[I].surface = name then
		begin
			texture := materials[I].new_texture;
			shader := materials[I].new_shader;
			Exit;
		end;
	
	texture := '';
	shader := '';
end;

constructor T4AModel.Create;
begin
	inherited;
	version := 7;
	modeltype := MODEL_TYPE_NORMAL;
	shaderid := $FFFF;
	scale := 1.0;
end;

procedure T4AModel.Load(reader : TMemoryReader);
var
	r : TMemoryReader;
begin
	r := reader.OpenChunk(MODEL_CHUNK_HEADER);
	try
		version := r.ReadByte;
		modeltype := r.ReadByte;
		shaderid := r.ReadWord;
		r.Read(bbox, Sizeof(bbox));
		r.Read(bsphere, Sizeof(bsphere));
		r.ReadLongword; // checksum or model version
		r.ReadLongword; // flags ???
		r.ReadSingle; // scale x ???
		scale := r.ReadSingle;
	finally
		r.Free;
	end;
end;

procedure T4AModel.Save(w : TMemoryWriter);
begin
	w.OpenChunk(MODEL_CHUNK_HEADER);
	w.WriteByte(version);
	w.WriteByte(modeltype);
	w.WriteWord(shaderid);
	w.Write(bbox, Sizeof(bbox));
	w.Write(bsphere, Sizeof(bsphere));
	w.WriteSingle(1500.0); // DWORD checksum indeed
	w.WriteLongword(0);
	w.WriteLongword(unkwn_parm_1);
	w.WriteLongword(0);
	w.WriteSingle(unkwn_parm_2);
	w.CloseChunk;
end;

procedure T4AModel.LoadFromFile(const fn : String);
var
	r : TMemoryReader;
begin
	r := TMemoryReader.CreateFromFile(fn);
	try
		Load(r);
	finally
		r.Free;
	end;
end;

procedure T4AModel.SaveToFile(const fn : String);
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	try
		Save(w);
		w.SaveTo(fn);
	finally
		w.Free;
	end;
end;

procedure T4AModelMtlsets.LoadMaterialSets(r : TMemoryReader);
var
	I, J : Longint;
begin
	SetLength(mtlsets, r.ReadWord);
	for I := 0 to Length(mtlsets) - 1 do
	begin
		mtlsets[I].name := r.ReadStringZ;
		mtlsets[I].hit_preset := r.ReadStringZ;
		if version >= 21 then
			mtlsets[I].voice := r.ReadStringZ;
		
		// TODO: check Arktika.1
		//if version >= MODEL_VER_REDUX then 
		if version >= 23 then // >= 38 then
			mtlsets[I].flags := r.ReadLongword;
		
		SetLength(mtlsets[I].materials, r.ReadWord);
		for J := 0 to Length(mtlsets[I].materials) - 1 do
		begin
			mtlsets[I].materials[J].surface := r.ReadStringZ;
			mtlsets[I].materials[J].new_texture := r.ReadStringZ;
			mtlsets[I].materials[J].new_shader := r.ReadStringZ;
		end;	
	end;
end;

procedure T4AModelMtlsets.SaveMaterialSets(w : TMemoryWriter);
var
	I, J : Longint;
begin
	w.WriteWord(Length(mtlsets));
	for I := 0 to Length(mtlsets) - 1 do
	begin
		w.WriteStringZ(mtlsets[I].name);
		w.WriteStringZ(mtlsets[I].hit_preset);
		if version >= 21 then
			w.WriteStringZ(mtlsets[I].voice);
			
		if version >= 23 then // >= MODEL_VER_EXODUS then
			w.WriteLongword(mtlsets[I].flags);
		
		w.WriteWord(Length(mtlsets[I].materials));
		for J := 0 to Length(mtlsets[I].materials) - 1 do
			with mtlsets[I].materials[J] do
			begin
				w.WriteStringZ(surface);
				w.WriteStringZ(new_texture);
				w.WriteStringZ(new_shader);
			end;
	end;
end;

function T4AModelMtlsets.GetMtlsetIndex(const name : String) : Longint;
var I : Longint;
begin
	Result := -1;
	
	for I := 0 to Length(mtlsets) - 1 do
		if mtlsets[I].name = name then
		begin
			Result := I;
			Exit;
		end;
end;

procedure T4AModelMtlsets.GetMaterialOverride(idx : Longint; const surface : String; out texture, shader : String);
begin
	if (idx >= 0) and (idx < Length(mtlsets)) then
	begin
		mtlsets[idx].GetOverride(surface, texture, shader);
		Exit;
	end;
	
	texture := '';
	shader := '';
end;

constructor T4AModelHierrarhy.Create;
begin
	inherited;
	modeltype := MODEL_TYPE_HIERRARHY;
end;

destructor T4AModelHierrarhy.Destroy;
var
	I : Integer;
begin
	for I := 0 to Length(meshes) - 1 do
		meshes[I].Free;
	
	lod1.Free;
	lod0.Free;
end;

procedure T4AModelHierrarhy.Load(reader : TMemoryReader);
var
	r, r2 : TMemoryReader;
	I : Longint;
	
	mesh : T4AModelSimple;
begin
	inherited Load(reader);
	
	case modeltype of
		MODEL_TYPE_HIERRARHY : ;
		else raise Exception.Create('Invalid model type! Must be hierrarhy');
	end;
	
	r := reader.OpenChunk(MODEL_CHUNK_CHILD);
	try
		I := 0;
		while r.More do
		begin    	
			mesh := T4AModelSimple.Create;
	
			r2 := r.OpenChunk;
			try
				try
					mesh.Load(r2);
				except on E: Exception do
					FreeAndNil(mesh);
				end;
	
				if mesh <> nil then
				begin
					SetLength(meshes, I+1);
					meshes[I] := mesh;
					Inc(I);
				end;
	
			finally
				r2.Free;
			end;
		end;
	finally
		r.Free;
	end;
	
	r := reader.OpenChunk(MODEL_CHUNK_LOD1);
	if Assigned(r) then
	begin
		try
			lod1 := T4AModelHierrarhy.Create;
			lod1.Load(r);
		finally
			r.Free;
		end;
	end else
		lod1 := nil;
	
	r := reader.OpenChunk(MODEL_CHUNK_LOD0);
	if Assigned(r) then
	begin
		try
			lod0 := T4AModelHierrarhy.Create;
			lod0.Load(r);
		finally
			r.Free;
		end;
	end else
		lod0 := nil;
	
	r := reader.OpenChunk(MODEL_CHUNK_MTL_SETS);
	if Assigned(r) then
	begin
		try
			LoadMaterialSets(r);
		finally
			r.Free;
		end;
	end;
end;

procedure T4AModelHierrarhy.Save(w : TMemoryWriter);
var
	I : Integer;
begin
	w.OpenChunk(MODEL_CHUNK_CHILD);
	for I := 0 to Length(meshes) - 1 do
	begin
		w.OpenChunk(I);
		meshes[I].Save(w);
		w.CloseChunk;
	end;
	w.CloseChunk;
	
	if Assigned(lod1) then
	begin
		w.OpenChunk(MODEL_CHUNK_LOD1);
		lod1.Save(w);
		w.CloseChunk;
	end;
	
	if Assigned(lod0) then
	begin
		w.OpenChunk(MODEL_CHUNK_LOD0);
		lod0.Save(w);
		w.CloseChunk;
	end;
	
	if version >= MODEL_VER_LL then
	begin  
		w.OpenChunk(37);
		for I := 1 to 6 do
			w.WriteLongword(0);
		w.CloseChunk;
	end;
	
	if (version >= 20) and (Length(mtlsets) > 0) then
	begin
		w.OpenChunk(MODEL_CHUNK_MTL_SETS);
		SaveMaterialSets(w);
		w.CloseChunk;
	end;
	
	if version >= MODEL_VER_EXODUS then
	begin
		w.OpenChunk(39);
		for I := 1 to 6 do
			w.WriteLongword(0);
		w.CloseChunk;
	
		w.OpenChunk(40);
		w.WriteLongword(0);
		w.CloseChunk;
	
		w.OpenChunk(41);
		w.CloseChunk;
	end;
	
	inherited;
end;

constructor T4AModelSimple.Create;
begin
	inherited;
	vertexformat := MODEL_VF_STATIC;
end;

procedure T4AModelSimple.Load(reader : TMemoryReader);
var
	r : TMemoryReader;
	count : Longword;
	
	I : Longint;
	uv : TVec2S16;
begin
	inherited Load(reader);
	
	case modeltype of
		MODEL_TYPE_NORMAL : ;
		else raise Exception.Create('Invalid model type! Must be static');
	end;
	
	r := nil;
	try
		r := reader.OpenChunk(MODEL_CHUNK_TEXTURE);
		texture := r.ReadStringZ;
		shader := r.ReadStringZ;
		material := r.ReadStringZ;
		
		if version >= MODEL_VER_LL then
		begin
			name := r.ReadStringZ;
			//r.ReadWord; // flags ?
			//r.ReadWord;
		end;
		FreeAndNil(r);
		
		r := reader.OpenChunk(MODEL_CHUNK_VERTICES);
		vertexformat := r.ReadLongword;
		SetLength(vertices, r.ReadLongword);
		if version >= MODEL_VER_ARKTIKA1 then
			r.ReadWord; // shadow model vertex count
		
		case vertexformat of
			MODEL_VF_STATIC: r.Read(vertices[0], Length(vertices)*Sizeof(T4AVertStatic));
			MODEL_VF_LEVEL: begin // HACK
				for I := 0 to Length(vertices)-1 do
				begin
					r.Read(vertices[I].point, Sizeof(TVec3));
					r.Read(vertices[I].normal, Sizeof(Longword));
					r.Read(vertices[I].tangent, Sizeof(Longword));
					r.Read(vertices[I].binormal, Sizeof(Longword));
					r.Read(uv, Sizeof(uv));
					vertices[I].tc.x := uv.x / 1024.0;
					vertices[I].tc.y := uv.y / 1024.0;
					r.Read(uv, Sizeof(uv)); // LMAP tc's, unused in level.egeoms
				end;
			end;
			else raise Exception.Create('Unknown 4A model vertex format');
		end;
		FreeAndNil(r);

		r := reader.OpenChunk(MODEL_CHUNK_INDICES);
		count := r.ReadLongword;
		if version >= MODEL_VER_ARKTIKA1 then
		begin
			count := count * 3; // in arktika.1 index count was changed to triangle count
			r.ReadWord; // shadow model index count
		end;
		SetLength(indices, count);
		r.Read(indices[0], count*Sizeof(Word));
		FreeAndNil(r);
	finally
		r.Free;
	end;
end;

procedure T4AModelSimple.Save(w : TMemoryWriter);
begin
	inherited;
	
	w.OpenChunk(MODEL_CHUNK_TEXTURE);
	w.WriteStringZ(texture);
	w.WriteStringZ(shader);
	w.WriteStringZ(material);
	if version >= MODEL_VER_LL then
	begin
		w.WriteStringZ(name);
		w.WriteLongword($00040002); // material flags, totally unknown
		if version >= MODEL_VER_EXODUS then
			w.WriteByte(1); // ?
	end;
	w.CloseChunk;
	
	w.OpenChunk(MODEL_CHUNK_VERTICES);
	w.WriteLongword(vertexformat);
	w.WriteLongword(Length(vertices));
	if version >= MODEL_VER_ARKTIKA1 then
		w.WriteWord(0);
	w.Write(vertices[0], Length(vertices)*Sizeof(T4AVertStatic));
	w.CloseChunk;
	
	w.OpenChunk(MODEL_CHUNK_INDICES);
	if version >= MODEL_VER_ARKTIKA1 then
	begin
		w.WriteLongword(Length(indices) div 3);
		w.WriteWord(0);
	end else
	begin
		w.WriteLongword(Length(indices));
	end;
	w.Write(indices[0], Length(indices)*Sizeof(Word));
	w.CloseChunk;
end;

constructor T4AModelHierrarhyL.Create;
begin
	inherited;
	modeltype := MODEL_TYPE_HIERRARHY;
end;

procedure T4AModelHierrarhyL.Load(reader : TMemoryReader);
var
	r : TMemoryReader;
	count : Longword;
begin
	inherited Load(reader);

	case modeltype of
		MODEL_TYPE_HIERRARHY : ;
		else raise Exception.Create('Invalid model type! Must be hierrarhy');
	end;

	r := reader.OpenChunk(MODEL_CHUNK_CHILD_L);
	try
		count := r.ReadLongword;
		SetLength(meshes, count);
		r.Read(meshes[0], count*Sizeof(Longword));
	finally
		r.Free;
	end;
end;

procedure T4AModelHierrarhyL.Save(w : TMemoryWriter);
begin
	inherited;

	w.OpenChunk(MODEL_CHUNK_CHILD_L);
	w.WriteLongword(Length(meshes));
	w.Write(meshes[0], Length(meshes)*4);
	w.CloseChunk;
end;

procedure T4AModelRef.Load(reader : TMemoryReader);
var
	r : TMemoryReader;
begin
	inherited Load(reader);

	r := reader.OpenChunk(MODEL_CHUNK_GEOMREF);	
	try
		vertexformat      := r.ReadLongword;
		if vertexformat <> MODEL_VF_LEVEL then
			raise Exception.Create('Invalid model vertex format! Must be level');
			
		vertexoffset      := r.ReadLongword;
		vertexcount       := r.ReadLongword;
		if version >= MODEL_VER_44 then 
			shadow_vcount := r.ReadLongword;
			
		indexoffset       := r.ReadLongword;
		indexcount        := r.ReadLongword;
		if version >= MODEL_VER_44 then 
			shadow_icount := r.ReadLongword;
	finally
		r.Free;
	end;
	
	if version >= MODEL_VER_44 then // Exodus
	begin
		r := reader.OpenChunk(MODEL_CHUNK_TEXTURE);
		try
			texture  := r.ReadStringZ;
			shader   := r.ReadStringZ;
			material := r.ReadStringZ;
			name     := r.ReadStringZ;
			flags    := r.ReadLongword;
		finally
			r.Free;
		end;
	end;
end;

procedure T4AModelRef.Save(w: TMemoryWriter);
begin
	w.OpenChunk(MODEL_CHUNK_GEOMREF);
	w.WriteLongword(vertexformat);
	w.WriteLongword(vertexoffset);
	w.WriteLongword(vertexcount);
	if version >= MODEL_VER_44 then
		w.WriteLongword(shadow_vcount);
	w.WriteLongword(indexoffset);
	w.WriteLongword(indexcount);
	if version >= MODEL_VER_44 then
		w.WriteLongword(shadow_vcount);
	w.CloseChunk;

	if version >= MODEL_VER_44 then // Exodus
	begin
		w.OpenChunk(MODEL_CHUNK_TEXTURE);
		w.WriteStringZ(texture);
		w.WriteStringZ(shader);
		w.WriteStringZ(material);
		w.WriteStringZ(name);
		w.WriteLongword(flags);
		w.CloseChunk;
	end;
	
	inherited;
end;

constructor T4AModelSkinnedMesh.Create;
begin
	inherited;
	modeltype := MODEL_TYPE_SKINNED_MESH;
end;

procedure T4AModelSkinnedMesh.Load(reader : TMemoryReader);
var
	r : TMemoryReader;
	bone_count : Byte;
	inum1, inum2 : Longword;
begin
	inherited Load(reader);

	if modeltype <> MODEL_TYPE_SKINNED_MESH then
		raise Exception.Create('invalid model type, must be skinned mesh');

	r := nil;
	try
		r := reader.OpenChunk(MODEL_CHUNK_TEXTURE);
		texture := r.ReadStringZ;
		shader := r.ReadStringZ;
		material := r.ReadStringZ;
		if version >= 18 then
		begin
			name := r.ReadStringZ;
			aux_params := r.ReadWord;
		end;
		FreeAndNil(r);
		
		r := reader.OpenChunk(MODEL_CHUNK_SKIN_VERTICES);
		bone_count := r.ReadByte;
		SetLength(bone_ids, bone_count);
		r.Read(bone_ids[0], Length(bone_ids));
		SetLength(bone_obb, bone_count);
		r.Read(bone_obb[0], Length(bone_obb)*Sizeof(T4AOBB));
		SetLength(vertices, r.ReadLongword);
		if version >= 30 then
			r.ReadWord; // shadow vertices count ?
		r.Read(vertices[0], Length(vertices)*Sizeof(T4AVertSkin));
		FreeAndNil(r);
		
		r := reader.OpenChunk(MODEL_CHUNK_INDICES);
		if version >= 18 then
		begin
			inum1 := r.ReadWord;
			inum2 := r.ReadWord;
		
			inum1 := inum1 * 3;
			inum2 := inum2 * 3;
		end else
		begin
			inum1 := r.ReadLongword;
			inum2 := 0;
		end;
		
		//WriteLn('inum1 = ', Length(indices));
		//WriteLn('inum2 = ', Length(indices2));
		
		SetLength(indices, inum1);
		SetLength(indices2, inum2);
		r.Read(indices[0], Length(indices)*Sizeof(Word));
		r.Read(indices2[0], Length(indices2)*Sizeof(Word));
		FreeAndNil(r);	
	finally
		r.Free;
	end;
end;

procedure T4AModelSkinnedMesh.Save(w : TMemoryWriter);
begin
	inherited Save (w);
	
	w.OpenChunk(MODEL_CHUNK_TEXTURE);
	w.WriteStringZ(texture);
	w.WriteStringZ(shader);
	w.WriteStringZ(material);
	if version >= 18 then
	begin
		w.WriteStringZ(name);
		w.WriteWord(aux_params); 
	end;
	w.CloseChunk;
	
	w.OpenChunk(MODEL_CHUNK_SKIN_VERTICES);
	w.WriteByte(Length(bone_ids));
	w.Write(bone_ids[0], Length(bone_ids));
	w.Write(bone_obb[0], Length(bone_obb)*Sizeof(T4AOBB));
	w.WriteLongword(Length(vertices));
	w.Write(vertices[0], Length(vertices)*Sizeof(T4AVertSkin));
	w.CloseChunk;
	
	w.OpenChunk(MODEL_CHUNK_INDICES);
	if version >= 18 then
	begin
		w.WriteWord(Length(indices) div 3);
		w.WriteWord(Length(indices2) div 3);
		w.Write(indices[0], Length(indices)*Sizeof(Word));
		w.Write(indices2[0], Length(indices2)*Sizeof(Word));
	end else
	begin
		w.WriteLongword(Length(indices));
		w.Write(indices[0], Length(indices)*Sizeof(Word));
	end;
	w.CloseChunk;
end;

function T4AModelSkinnedMesh.GetPointScale : Single;
begin
	if version >= MODEL_VER_ARKTIKA1 then
		GetPointScale := (1/32767) * scale
	else
		GetPointScale := 1/2720;
end;

constructor T4AModelSkinned.Create;
begin
	inherited;
	modeltype := MODEL_TYPE_SKINNED;
end;

destructor T4AModelSkinned.Destroy;
var
	I : Integer;
begin
	for I := 0 to Length(meshes) - 1 do
		meshes[I].Free;
end;

procedure T4AModelSkinned.Load(reader : TMemoryReader);
var
	I : Longint;
	r, r2 : TMemoryReader;
	
	mesh : T4AModelSkinnedMesh;
begin
	inherited Load(reader);
	
	if modeltype <> MODEL_TYPE_SKINNED then
		raise Exception.Create('invalid model type, must be skinned');
	
	r := reader.OpenChunk(MODEL_CHUNK_SKELETON_CRC);
	if r <> nil then
	begin
		try
			skeleton_crc := r.ReadLongword;
		finally
			r.Free;
		end;
	end;
	
	r := reader.OpenChunk(MODEL_CHUNK_CHILD);
	r2 := nil;
	
	try
		I := 0;
		while r.More do
		begin
			mesh := T4AModelSkinnedMesh.Create;
	
			r2 := r.OpenChunk;
			try
				try
					mesh.Load(r2);
				except
					on E: Exception do
						FreeAndNil(mesh);
				end;
	
				if mesh <> nil then
				begin
					SetLength(meshes, I+1);
					meshes[I] := mesh;
					Inc(I);
				end;
			finally
				FreeAndNil(r2);
			end;
		end;
	finally
		r.Free;
	end;
end;

procedure T4AModelSkinned.Save(w : TMemoryWriter);
var
	I : Longint;
begin
	Inherited Save(w);

	w.OpenChunk(MODEL_CHUNK_SKELETON_CRC);
	w.WriteLongword(skeleton_crc);
	w.CloseChunk;

	w.OpenChunk(MODEL_CHUNK_CHILD);
	for I := 0 to Length(meshes) - 1 do
	begin
		w.OpenChunk(I);
		meshes[I].Save(w);
		w.CloseChunk;
	end;
	w.CloseChunk;
end;

constructor T4AModelSkeleton.Create;
begin
	Inherited;
	modeltype := MODEL_TYPE_SKELETON;
	texture_subst := '';
end;

destructor T4AModelSkeleton.Destroy;
var
	I : Longint;
begin
	for I := 0 to Length(meshes[0]) - 1 do
		meshes[0,I].Free;
	for I := 0 to Length(meshes[1]) - 1 do
		meshes[1,I].Free;
	for I := 0 to Length(meshes[2]) - 1 do
		meshes[2,I].Free;
	
	if Assigned(skeleton) then
		skeleton.Free;
end;

procedure T4AModelSkeleton.Load(reader : TMemoryReader);
var
	r, r2 : TMemoryReader;
	I : Longint;

	procedure LoadMeshes(src : TMemoryReader; lod : Longint);
	var
		r : TMemoryReader;
		I : Longint;
	begin
		I := 0;
		while src.More do
		begin
			SetLength(meshes[lod], I+1);
			meshes[lod,I] := T4AModelSkinned.Create;
	
			r := src.OpenChunk;
			try
				meshes[lod,I].Load(r);
			finally
				r.Free;
			end;
	
			Inc(I);
		end;
	end;
begin
	inherited Load(reader);

	case modeltype of
		MODEL_TYPE_SKELETON, MODEL_TYPE_ANIMATED: ;
		else
			raise Exception.Create('invalid model type, must be skeleton');
	end;

	r := nil;
	r2 := nil;
	try
		r := reader.OpenChunk(MODEL_CHUNK_MESHES_FN);
		if r <> nil then
		begin
			SetLength(mesh_names, r.ReadLongword);
			if Length(mesh_names) <> 3 then
				WriteLn('!!! Length(mesh_names) <> 3');
				for I := 0 to Length(mesh_names) - 1 do
					mesh_names[I] := r.ReadStringZ;
			FreeAndNil(r);
		end;
		
		r := reader.OpenChunk(MODEL_CHUNK_MESHES);
		if r <> nil then
		begin
			r2 := r.OpenChunk(0); // LOD 2
			if (r2 <> nil) and (r2.size > 0) then
				LoadMeshes(r2, 0);
			FreeAndNil(r2);
			{
			r2 := r.OpenChunk(1); // LOD 1
			if (r2 <> nil) and (r2.size > 0) then
				LoadMeshes(r2, 1);
			FreeAndNil(r2);
			
			r2 := r.OpenChunk(2);
			if (r2 <> nil) and (r2.size > 0) then
				LoadMeshes(r2, 2);
			FreeAndNil(r2);
			} 
			FreeAndNil(r);
		end;
		
		r := reader.OpenChunk(MODEL_CHUNK_TEXTURE_SUBST);
		if r <> nil then
		begin
			texture_subst := r.ReadStringZ;
			FreeAndNil(r);
		end;
		
		r := reader.OpenChunk(MODEL_CHUNK_SKELETON_FN);
		if r <> nil then
		begin
			skeleton_name := r.ReadStringZ;
			FreeAndNil(r);
		end;
		
		if version < MODEL_VER_EXODUS then
		begin
			r := reader.OpenChunk(MODEL_CHUNK_SKELETON);
			if r <> nil then
			begin
				skeleton := T4ASkeleton.Create;
				try
					skeleton.LoadKonfig(r);
				except 
					on E : Exception do
					begin
						WriteLn('skeleton loading failed: ', E.Message);
						FreeAndNil(skeleton);
					end;
				end;
				FreeAndNil(r);
			end;
		end;
		
		r := reader.OpenChunk(MODEL_CHUNK_MTL_SETS);
		if Assigned(r) then
		begin
			LoadMaterialSets(r);
			FreeAndNil(r);
		end;
	finally
		r.Free;
		r2.Free;
	end;
end;

procedure T4AModelSkeleton.Save(w : TMemoryWriter);
begin
	raise Exception.Create('not implemented');
end;

function T4AModelSkeleton.GetTextureSubst(const src : String) : String;
var
	I, L : Longint;
	
	src_start : Longint;
	src_end : Longint;
	dst_start : Longint;
	dst_end : Longint;
begin
	// texture subst is a string in form
	// src1=dst1,src2=dst2,...

	I := 1;
	L := Length(texture_subst);
	
	while I <= L do
	begin
		src_start := I;
		while (I <= L) and (texture_subst[I] <> '=') do Inc(I);
		src_end := I;
		
		Inc(I);
		
		dst_start := I;
		while (I <= L) and (texture_subst[I] <> ',') do Inc(I);
		dst_end := I;
		
		Inc(I);
		
		if Copy(texture_subst, src_start, src_end-src_start) = src then
		begin
			GetTextureSubst := Copy(texture_subst, dst_start, dst_end-dst_start);
			Exit;
		end;
	end;
	
	GetTextureSubst := src;
end;

procedure T4AModelSoftbody.Load(reader : TMemoryReader);
var
	r : TMemoryReader;
begin
	inherited Load(reader);
	
	r := reader.OpenChunk(MODEL_CHUNK_TEXTURE);
	try
		texture := r.ReadStringZ;
		shader := r.ReadStringZ;
		material := r.ReadStringZ;
	finally
		r.Free;
	end;
end;

procedure T4AModelSoftbody.Save(w : TMemoryWriter);
begin
	inherited Save(w);
	
	w.OpenChunk(MODEL_CHUNK_TEXTURE);
	w.WriteStringZ(texture);
	w.WriteStringZ(shader);
	w.WriteStringZ(material);
	w.CloseChunk;

end;

procedure T4AModelSoftbody.LoadSoftbodyData(r : TMemoryReader);
var
	I : Longint;
	isLE : Boolean;
	
	function ReadWord : Word;
	begin
		if isLE then ReadWord := r.ReadWord
		else ReadWord := BEtoN(r.ReadWord)
	end;
	
	function ReadLongword : Longword;
	begin
		if isLE then ReadLongword := r.ReadLongword
		else ReadLongword := BEtoN(r.ReadLongword)
	end;
	
	function ReadSingle : Single;
	begin
		if isLE then ReadSingle := r.ReadSingle
		else ReadSingle := Single(BEtoN(r.ReadLongword))
	end;
begin
	isLE := r.ReadByte <> 0; // TODO endianess
	
	if ReadLongword <> $0001FDF then
		WriteLn('LoadSoftbodyData: signature <> $0001FDF');
		
	ReadLongword; // version or file type
	ReadLongword; // checksum
	
	param1 := r.ReadByte <> 0;
	param2 := ReadSingle;
	param3 := ReadSingle;
	param4 := ReadSingle;
	param5 := ReadSingle;
	param6 := r.ReadByte <> 0;
	param7 := ReadSingle;
	param8 := r.ReadByte <> 0;
	param9 := ReadSingle;
	
	SetLength(indices, ReadLongword);
	SetLength(vertices, ReadLongword);
	
	if ReadLongword <> $24 then
		WriteLn('LoadSoftbodyData: vertex_size <> $24');
	
	for I := 0 to Length(indices)-1 do
		indices[I] := ReadWord;
	
	for I := 0 to Length(vertices)-1 do
	begin
		vertices[I].point.x := ReadSingle;
		vertices[I].point.y := ReadSingle;
		vertices[I].point.z := ReadSingle;
		vertices[I].unk1 := ReadLongword;
		vertices[I].unk2 := ReadLongword;
		vertices[I].normal.x := ReadSingle;
		vertices[I].normal.y := ReadSingle;
		vertices[I].normal.z := ReadSingle;
		vertices[I].tc.x := ReadWord;
		vertices[I].tc.y := ReadWord;
	end;
	
	// дальше идёт NxClothMesh
end;

function Load4AModel(r : TMemoryReader; level : Boolean) : T4AModel;
var
	chunk_header : TMemoryReader;
	header : T4AHeader;
begin
	chunk_header := r.OpenChunk(MODEL_CHUNK_HEADER);
	chunk_header.Read(header, Sizeof(header));
	chunk_header.Free;
	
	case header.modeltype of
		MODEL_TYPE_NORMAL:
			if level then
				Result := T4AModelRef.Create
			else
				Result := T4AModelSimple.Create;
		MODEL_TYPE_HIERRARHY:
			if level then
				Result := T4AModelHierrarhyL.Create
			else
				Result := T4AModelHierrarhy.Create;
		MODEL_TYPE_SKELETON, MODEL_TYPE_ANIMATED:
			Result := T4AModelSkeleton.Create;
		MODEL_TYPE_SKINNED:
			Result := T4AModelSkinned.Create;
		MODEL_TYPE_SKINNED_MESH:
			Result := T4AModelSkinnedMesh.Create;
		MODEL_TYPE_SOFTBODY:
			Result := T4AModelSoftbody.Create;
		else
			raise Exception.Create('Unsupported model type ' + IntToStr(header.modeltype));
	end;
	
	try
		Result.Load(r);
	except
		on E: Exception do
		begin
			FreeAndNil(Result);
			raise;
		end;
	end;
end;

procedure SetModelVersion(m : T4AModel; ver : Byte);
var
	I : Integer;
	h : T4AModelHierrarhy;
	s : T4AModelSkinned;
begin
	m.version := ver;
	if m is T4AModelHierrarhy then
	begin
		h := T4AModelHierrarhy(m);
		for I := 0 to Length(h.meshes) - 1 do
			h.meshes[I].version := ver;
	end;
	if m is T4AModelSkinned then
	begin
		s := T4AModelSkinned(m);
		for I := 0 to Length(s.meshes) - 1 do
			s.meshes[I].version := ver;
	end;
end;

constructor T4ALevel.Create;
begin
	filename := 'level';
	version  := LEVEL_VER_2033;
	quality  := 0;
	checksum := Longword(1500.0);
	sound_occlusion_version := 4 // 2033 - 4, LL & Redux - 5
end;

destructor T4ALevel.Destroy;
var
	I : Longint;
begin
	for I := 0 to Length(visuals) - 1 do
		visuals[I].Free;
	
	inherited;
end;

procedure T4ALevel.Load(const path : String);
var
	I : Longint;
	f, r, v : TMemoryReader;
begin
	f := TMemoryReader.CreateFromFile(path + '\' + filename);
	
	r := f.OpenChunk(LEVEL_CHUNK_VERSION);
	version := r.ReadWord;
	quality := r.ReadWord;
	r.Free;
	
	r := f.OpenChunk(LEVEL_CHUNK_CHECKSUM);
	if r <> nil then
	begin
		checksum := r.ReadLongword;
		r.Free;
	end;
	
	r := f.OpenChunk(LEVEL_CHUNK_VISUALS);
	I := 0;
	while r.More do
	begin
		v := r.OpenChunk;
		
		SetLength(visuals, I+1);
		visuals[I] := Load4AModel(v, True);
		Inc(I);
		
		v.Free;
	end;
	r.Free;
	
	r := f.OpenChunk(LEVEL_CHUNK_MATERIALS);
	SetLength(materials, r.ReadWord);
	for I := 0 to Length(materials) - 1 do
	begin
		materials[I].shader := r.ReadStringZ;
		materials[I].texture := r.ReadStringZ;
		materials[I].material := r.ReadStringZ;
		materials[I].flags := r.ReadLongword;
	end;
	r.Free;
	
	r := f.OpenChunk(LEVEL_CHUNK_SECTORS);
	I := 0;
	while r.More do
	begin
		v := r.OpenChunk(I);
		
		SetLength(sectors, I+1);
		v.ReadLongword;
		v.ReadLongword;
		sectors[I] := v.ReadLongword;
		Inc(I);
		
		v.Free;
	end;
	r.Free;
	
	f.Free;
	
	if FileExists(path + '\' + filename + '.geom_pc') then
		LoadGeom(path)
	else if FileExists(path + '\' + filename + '.geom_xbox') then
		LoadGeomXBox(path)
	else
		raise Exception.Create('Level geometry not found, nor .geom_pc nor .geom_xbox');
end;

procedure T4ALevel.SaveTo(const path : String; forXBox : Boolean);
var
	w : TMemoryWriter;
	I : Longint;
begin
	// save main file
	w := TMemoryWriter.Create;
	w.OpenChunk(LEVEL_CHUNK_VERSION);
	w.WriteWord(version);
	w.WriteWord(quality);
	w.CloseChunk;
	
	w.OpenChunk(LEVEL_CHUNK_VISUALS);
	for I := 0 to Length(visuals) - 1 do
	begin
		w.OpenChunk(I);
		visuals[I].Save(w);
		w.CloseChunk;
	end;
	w.CloseChunk;
	
	w.OpenChunk(LEVEL_CHUNK_MATERIALS);
	w.WriteWord(Length(materials));
	for I := 0 to Length(materials) - 1 do
	begin
		w.WriteStringZ(materials[I].shader);
		w.WriteStringZ(materials[I].texture);
		w.WriteStringZ(materials[I].material);
		w.WriteLongword(materials[I].flags);
	end;
	w.CloseChunk;
	
	w.OpenChunk(LEVEL_CHUNK_CHECKSUM);
	w.WriteLongword(checksum);
	w.CloseChunk;
	
	w.OpenChunk(LEVEL_CHUNK_SECTORS);
	for I := 0 to Length(sectors) - 1 do
	begin
		w.OpenChunk(I);
		
		// is that a chunk? mixing chunks and regular data is not good!
		w.WriteLongword(2);
		w.WriteLongword(4);
		w.WriteLongword(sectors[I]);
		
		if version >= LEVEL_VER_ARKTIKA1 then
		begin
			w.WriteStringZ(filename + '_' + IntToStr(I));
			if version >= LEVEL_VER_EXODUS then
				w.WriteSingle(1.0);
		end;
		
		w.CloseChunk;
	end;
	w.CloseChunk;
	
	w.SaveTo(path + '\' + filename);
	w.Free;
	
	// save geom
	if forXBox then
		SaveGeomXbox(path + '\' + filename + '.geom_xbox')
	else
		SaveGeom(path + '\' + filename + '.geom_pc');
	
	// save portals
	//if Length(portals) > 0 then
	begin
		w := TMemoryWriter.Create;
		
		// version
		w.OpenChunk($0000FFFF);
		if version >= LEVEL_VER_EXODUS then
			w.WriteLongword(3)
		else
			w.WriteLongword(2);
		w.CloseChunk;
		
		// render portals
		for I := 0 to Length(portals) - 1 do
		begin
			w.OpenChunk(I);
			
			w.WriteLongword(Length(portals[I].points));
			w.Write(portals[I].points[0], Sizeof(TVec3)*Length(portals[I].points));
			w.WriteStringZ(portals[I].name);
			w.WriteLongword(0);
			
			if version >= LEVEL_VER_EXODUS then
				w.WriteLongword($80808080); // whats that ? some sort of color ?
			
			w.CloseChunk;
		end;
		
		if version < LEVEL_VER_EXODUS then
		begin
			// sound occlusion (dummy)
			w.OpenChunk($0000FFFC);
			w.WriteLongword(sound_occlusion_version);
			w.WriteLongword(checksum);
			w.WriteLongword(0); // sqrt(size_table1)
			w.WriteLongword(0); // portal count
			w.WriteLongword(0); // ?
			w.CloseChunk;
		end;
		
		w.SaveTo(path + '\level.portals');
		w.Free;
	end;
end;

function T4ALevel.AddMaterial(s, t, m : String; flags : Longword) : Longint;
var
	I : Integer;
begin
	Result := -1;
	for I := 0 to Length(materials) - 1 do
		if (materials[I].shader = s) and
		   (materials[I].texture = t) and
		   (materials[I].material = m) and
		   (materials[I].flags = flags) then
		begin
			Result := I;
			Break;
		end;
	
	if Result = -1 then
	begin
		I := Length(materials);
		SetLength(materials, I+1);
		materials[I].shader := s;
		materials[I].texture := t;
		materials[I].material := m;
		materials[I].flags := flags;
		Result := I;
	end;
end;

function T4ALevel.AddVertexBuffer(const Buffer; count : Longword) : Longword;
var
	I : Longword;
begin
	I := Length(vbuffer);
	SetLength(vbuffer, I+count);
	Move(Buffer, vbuffer[I], count*Sizeof(T4AVertLevel));
	Result := I;
end;

function T4ALevel.AddIndexBuffer(const Buffer; count : Longword) : Longword;
var
	I : Longword;
begin
	I := Length(ibuffer);
	SetLength(ibuffer, I+count);
	Move(Buffer, ibuffer[I], count*Sizeof(Word));
	Result := I;
end;

function T4ALevel.AddVisual(visual : T4AModel) : Longword;
var
	I : Longword;
begin
	I := Length(visuals);
	SetLength(visuals, I+1);
	visuals[I] := visual;
	Result := I;
end;

function T4ALevel.AddSector(id : Longword) : Longword;
var
	I : Longword;
begin
	I := Length(sectors);
	SetLength(sectors, I+1);
	sectors[I] := id;
	Result := I;
end;

procedure T4ALevel.LoadGeom(const path : String);
var
	f, r : TMemoryReader;
	
	version : Word;
	vcount, icount : Longint;
begin
	f := TMemoryReader.CreateFromFile(path + '\' + filename + '.geom_pc');
		
	r := f.OpenChunk(LEVEL_CHUNK_VERSION);
	version := r.ReadWord;
	r.Free;
		
	r := f.OpenChunk(LEVEL_CHUNK_VBUFFER);
	
	if version >= LEVEL_VER_EXODUS then
	begin
		{vcount := }r.ReadLongword;
		{shadow_vcount := }r.ReadLongword;
		
		vcount := (r.size - 8) div Sizeof(T4AVertLevel);
	end else
		vcount := r.size div Sizeof(T4AVertLevel);
	
	SetLength(vbuffer, vcount);
	r.Read(vbuffer[0], vcount*Sizeof(T4AVertLevel));
	
	r.Free;
	
	r := f.OpenChunk(LEVEL_CHUNK_IBUFFER);
	
	if version >= LEVEL_VER_EXODUS then
	begin
		{icount := }r.ReadLongword;
		{shadow_icount := }r.ReadLongword;
		
		icount := (r.size - 8) div Sizeof(Word);
	end else
		icount := r.size div Sizeof(Word);
	
	SetLength(ibuffer, icount);
	r.Read(ibuffer[0], icount*Sizeof(Word));
	
	r.Free;
	
	f.Free;
end;

procedure T4ALevel.LoadGeomXBox(const path : String);
var
	I : Longint;
	f, r : TMemoryReader;
	
	vcount, icount : Longint;
begin
	f := TMemoryReader.CreateFromFile(path + '\' + filename + '.geom_xbox');
	
	r := f.OpenChunk(LEVEL_CHUNK_VBUFFER);
	vcount := r.size div Sizeof(T4AVertLevel);
	
	SetLength(vbuffer, vcount);
	for I := 0 to vcount-1 do
	begin
		vbuffer[I].point.x := Single(BEtoN(r.ReadLongword));
		vbuffer[I].point.y := Single(BEtoN(r.ReadLongword));
		vbuffer[I].point.z := Single(BEtoN(r.ReadLongword));
		vbuffer[I].normal := BEtoN(r.ReadLongword);
		vbuffer[I].tangent := BEtoN(r.ReadLongword);
		vbuffer[I].binormal := BEtoN(r.ReadLongword);
		vbuffer[I].tc.x := BEtoN(r.ReadSmallint);
		vbuffer[I].tc.y := BEtoN(r.ReadSmallint);
		vbuffer[I].lm.x := BEtoN(r.ReadSmallint);
		vbuffer[I].lm.y := BEtoN(r.ReadSmallint);
	end;
	
	r.Free;
	
	r := f.OpenChunk(LEVEL_CHUNK_IBUFFER);
	icount := r.size div Sizeof(Word);
	
	SetLength(ibuffer, icount);
	for I := 0 to icount-1 do
		ibuffer[I] := BEtoN(r.ReadWord);
	
	r.Free;
	
	f.Free;
end;

procedure T4ALevel.SaveGeom(const path : String);
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	w.OpenChunk(LEVEL_CHUNK_VERSION);
	w.WriteWord(16);//version); // Strange but saved Exodus version doesn't work properly, I guess I have to leave it as is for now
	w.WriteWord(quality);
	w.CloseChunk;
	
	w.OpenChunk(LEVEL_CHUNK_CHECKSUM);
	w.WriteLongword(checksum);
	w.CloseChunk;
	
	w.OpenChunk(LEVEL_CHUNK_VBUFFER);
	if version >= LEVEL_VER_EXODUS then
	begin
		//w.WriteLongword(Length(vbuffer));
		//w.WriteLongword(0);
	end;
	w.Write(vbuffer[0], Length(vbuffer)*Sizeof(T4AVertLevel));
	w.CloseChunk;
	
	w.OpenChunk(LEVEL_CHUNK_IBUFFER);
	if version >= LEVEL_VER_EXODUS then
	begin
		//w.WriteLongword(Length(ibuffer));
		//w.WriteLongword(0);
	end;
	w.Write(ibuffer[0], Length(ibuffer)*Sizeof(Word));
	w.CloseChunk;
	
	w.SaveTo(path);
	w.Free;
end;

procedure T4ALevel.SaveGeomXBox(const path : String);
var
	w : TMemoryWriter;
	I : Longint;
begin
	w := TMemoryWriter.Create;
	
	w.OpenChunk(LEVEL_CHUNK_VERSION);
	w.WriteLongword(16);
	w.CloseChunk;
	
	w.OpenChunk(LEVEL_CHUNK_CHECKSUM);
	w.WriteLongword(checksum);
	w.CloseChunk;
	
	w.OpenChunk(LEVEL_CHUNK_VBUFFER);
	for I := 0 to Length(vbuffer)-1 do
	begin
		w.WriteLongword(NtoBE(Longword(vbuffer[I].point.x)));
		w.WriteLongword(NtoBE(Longword(vbuffer[I].point.y)));
		w.WriteLongword(NtoBE(Longword(vbuffer[I].point.z)));
		w.WriteLongword(NtoBE(vbuffer[I].normal));
		w.WriteLongword(NtoBE(vbuffer[I].tangent));
		w.WriteLongword(NtoBE(vbuffer[I].binormal));
		w.WriteWord(NtoBE(vbuffer[I].tc.x));
		w.WriteWord(NtoBE(vbuffer[I].tc.y));
		w.WriteWord(NtoBE(vbuffer[I].lm.x));
		w.WriteWord(NtoBE(vbuffer[I].lm.y));
	end;
	w.CloseChunk;
	
	w.OpenChunk(LEVEL_CHUNK_IBUFFER);
	for I := 0 to Length(ibuffer)-1 do
		w.WriteWord(NtoBE(ibuffer[I]));
	w.CloseChunk;
	
	w.SaveTo(path);
	w.Free;		
end;

destructor T4ALevel2.Destroy;
var
	I : Integer;
begin
	for I := 0 to Length(sublevels) - 1 do
		sublevels[I].Free;

	inherited;
end;

procedure T4ALevel2.Load(const path : String);
var
	list : TStringList;
	sr : TSearchRec;
	I : Integer;
begin
	// actually we should parse static\level.lightmaps here 
	
	list := TStringList.Create;
	if FindFirst(path + '\static\*.', faAnyFile xor faDirectory, sr) = 0 then
	begin
		repeat
			list.Add(sr.Name);
			WriteLn('level part: ', sr.Name);
		until FindNext(sr) <> 0;
	
		FindClose(sr);
	end;
	WriteLn('loading statics');
	SetLength(sublevels, list.Count);
	for I := 0 to Length(sublevels) - 1 do
	begin
		sublevels[I] := T4ALevel.Create;
		sublevels[I].filename := list[I];
		sublevels[I].Load(path + '\static');
	end;
	WriteLn('end statics');
	list.Free;
end;

end.
