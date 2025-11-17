// Ќерешенные проблемы:
// шейпы дл€ версий позднее 2033 надо создавать другие 
// создание матриц дл€ линков и укрытий надо сделать как дл€ объектов (безопасным если Z = (0, 1, 0))
// цена PE линков считаетс€ непон€тно как (вроде как длинна пути * 1000, но не всегда совпадает.  ак в большую так и в меньшею сторону причЄм)
// human combat covers работают непон€тно как
// не грузитс€ l09_dead_city_1, l11_dry, l08_market (что-то там не так идЄт в PathEngine)

unit uSceneCovers;

interface
uses vmath, chunkedFile, Konfig, PhysX, uSelectable;

type
	THumanCombatCoverPose = record
		vec1 : TVec3;
		cell1 : Longint;
		vec2 : TVec3;
		cell2 : Longint;
		flags2 : Word;
		packed_dir : Word;
	end;
{	
	THumanCombatCover = record
		flags : Longword;
		arr : array[0..5] of THumanCombatCoverPose;
	end;
	
	THumanCombatCoverArray = array of THumanCombatCover;
	
	// Last Light
	THumanCombatCover2 = record
		pos : TVec3;
		unk1 : Longword;
		unk2 : Longword;
	end;
		
	THumanCombatCover2Array = array of THumanCombatCover2;
}
	THumanCombatCover = class(ISelectable)
		cover_id : Word; // corresponding UCover ID
		flags : Word;
		arr : array[0..5] of THumanCombatCoverPose;
		e_visible : array[0..5] of Boolean;
		
		// last light extra data
		pos : TVec3;
		unk1 : Longword;
		unk2 : Longword;
		
		ph_scene : TPHScene; // TODO use global variable?
		ph_actors : array [0..11] of TPHActor;
		
		constructor Create(_ph_scene : TPHScene);
		destructor Destroy; override;
		
		procedure Load(r : TMemoryReader);
		procedure LoadLLExtraData(r : TMemoryReader);
		
		procedure Save(w : TMemoryWriter);
		
		procedure UpdatePhysics;
	end;
	
	THumanCombatCoverArray = array of THumanCombatCover;
	
	TCover = class(ISelectable)
		data : TSection;
		
		ph_scene : TPHScene; // TODO use global variable?
		ph_actor : TPHActor;
		
		FMatrix : TMatrix;
		
		_ground_cell : TIntegerValue;
		_conn_reg : TIntegerValue;
		_pe_index : TIntegerValue;
		
		constructor Create(_ph_scene : TPHScene; _data : TSection);
		destructor Destroy; override;
		
		procedure GetBBox(out b : TAABB); override;
		
		function ConnectedToAimap : Boolean;
		
		function Pos : TVec3;
		procedure SetMatrix(const m : TMatrix);
		function GetID : Word;
		
		property ID : Word read GetID;
		property Matrix : TMatrix read FMatrix write SetMatrix;
	end;
	
	TCoversDB = array[0..66534] of TCover;
	TCoverArray = array of TCover;
	
	TCoverLink = class(ISelectable)
		data : TSection;
		
		ph_scene : TPHScene; // TODO use global variable?
		ph_actor : TPHActor;
		
		cover_from : TIntegerValue;
		cover_to : TIntegerValue;
		
		constructor Create(_ph_scene : TPHScene; _data : TSection; const covers : TCoversDB);
		destructor Destroy; override;
		
		procedure UpdatePhysics(const covers : TCoversDB);
	end;

	TCoverLinkArray = array of TCoverLink;

function GetCenter(const covers : TCoverArray) : TVec3; overload;	
	
type
	TCoverManager = class
		covers_version : Longint;
		
		konf_covers : TTextKonfig; // loaded nav_map.covers or nil if 2033
		sect_covers : TSection;
	
		sect_ulinks : TSection;
		sect_ulinks_pe : TSection;
	
		covers : TCoversDB;
		links : TCoverLinkArray;
		links_pe : TCoverLinkArray;
		human_combat_covers : THumanCombatCoverArray;
		
		show_pe_links : Boolean;
		
		procedure LoadCoversAndLinks2033(levelbin_root : TSection; ph_scene : TPHScene);
		procedure LoadCoversAndLinksLL(const fn : String; ph_scene : TPHScene);
		procedure LoadHumanCombatCovers(const fn : String; ph_scene : TPHScene);
		
		procedure Save(const level_dir : String);
		procedure SaveHumanCombatCovers2033(const fn : String);
		
		procedure UnloadPELinks;
		procedure UnloadAll;
		
		procedure AddCover(cover : TCover);
		procedure RemoveCover(cover : TCover);
		procedure UpdateCoverPosition(cover : TCover);
		
		function FindLink(cover_id_from, cover_id_to : Word) : TCoverLink;
		procedure AddLink(link : TCoverLink);
		procedure RemoveLink(link : TCoverLink);
		
		procedure RemovePELink(link : TCoverLink);
		
		procedure UpdateHumanCombatCoverPosition(cover : THumanCombatCover);
		function FindHumanCombatCover(cover_id : Word) : THumanCombatCover;
		
		procedure DrawCovers;
		procedure DrawLinks;
		procedure DrawHumanCombatCovers;
		
		function GenerateCoverId : Word;
		function GenerateCoverPEIndex : Word;
		
		procedure CalculatePELinks(ph_scene : TPHScene);
		
		procedure Stat; // RESEARCH
		procedure StatHumanCombatCovers;
	end;
	

procedure LoadCovers(var covers : TCoversDB; sect : TSection; ph_scene : TPHScene);
procedure LoadCoverLinks(var links : TCoverLinkArray; sect : TSection; ph_scene : TPHScene; const covers : TCoversDB);


implementation
uses sysutils, classes, GL, common, PHGroups, _compressed_normal, framework, navmap_server, uEditorUtils, Engine;

const
	NAVMAP_COVERS_SIGN = $63767273; // ASCII srvc 

const
	LINK_TIP_L = 0.3;
	LINK_TIP_W = 0.04; // halfwidth actually
	
constructor THumanCombatCover.Create(_ph_scene : TPHScene);
var
	I : Longint;
begin
	ph_scene := _ph_scene;
	for I := 0 to 5 do
		e_visible[I] := True;
end;
	
destructor THumanCombatCover.Destroy;
var
	I : Longint;
begin
	for I := 0 to 11 do
		PHRemoveActor(ph_scene, ph_actors[I]);
	inherited;
end;

procedure THumanCombatCover.Load(r : TMemoryReader);
begin
	self.cover_id := r.ReadWord;
	self.flags := r.ReadWord;
	r.Read(self.arr, Sizeof(self.arr));
end;

procedure THumanCombatCover.LoadLLExtraData(r : TMemoryReader);
begin
	r.Read(self.pos, Sizeof(self.pos));
	self.unk1 := r.ReadLongword;
	self.unk2 := r.ReadLongword;
end;

procedure THumanCombatCover.Save(w : TMemoryWriter);
begin
	w.WriteWord(self.cover_id);
	w.WriteWord(self.flags);
	
	// zero-out unused poses ?
	
	w.Write(self.arr, Sizeof(self.arr));
end;

procedure THumanCombatCover.UpdatePhysics;
var
	I, J : Longint;
	desc : array[0..11] of Pointer;
	dim : TVec3;
	offs : TVec3;
	matr : TMatrix;
begin
	dim.x := 0.1;
	dim.y := 0.1;
	dim.z := 0.1;
	
	offs.x := 0;
	offs.y := 0;
	offs.z := 0;
	
	J := 0;
	
	for I := 0 to 5 do
		if (self.flags and (1 shl I)) <> 0 then
		begin
			desc[J] := PHShapeBox(@dim, @offs); 
			Translate(matr, self.arr[I].vec1);
			self.ph_actors[J] := PHCreateActor(ph_scene, 1, 1, @desc[J], @matr);
			PHSetUserdata(ph_actors[J], self);
			PHSetGroup(PHGetShape(ph_actors[J], 0), PH_GROUP_HUMAN_COMBAT_COVER);
			PHSetShapeUserdata(PHGetShape(ph_actors[J], 0), Pointer(self.arr[I].cell1));
			Inc(J);
			
			desc[J] := PHShapeBox(@dim, @offs); 
			Translate(matr, self.arr[I].vec2);
			self.ph_actors[J] := PHCreateActor(ph_scene, 1, 1, @desc[J], @matr);
			PHSetUserdata(ph_actors[J], self);
			PHSetGroup(PHGetShape(ph_actors[J], 0), PH_GROUP_HUMAN_COMBAT_COVER);
			PHSetShapeUserdata(PHGetShape(ph_actors[J], 0), Pointer(self.arr[I].cell2));
			Inc(J);
		end;
	{	
	if J > 0 then
	begin
		self.ph_actor := PHCreateActor(ph_scene, 1, J, @desc[0], nil);
		
		PHSetUserdata(ph_actor, self);
		for I := 0 to J - 1 do
			PHSetGroup(PHGetShape(ph_actor, I), PH_GROUP_HUMAN_COMBAT_COVER);
	end;
	}
end;

constructor TCover.Create(_ph_scene : TPHScene; _data : TSection);
var
	dim : TVec3;
	offset : TVec3;
	desc : Pointer;
	m : TMatrix;
	
	pos : TVec3;
	packed_dir : Word;
	right : TVec3;
	up : TVec3;
	dir : TVec3;
begin
	inherited Create;
	
	data := _data;
	pos := data.GetVec3('position');
	packed_dir := data.GetInt('direction', 'u16');
	
	_ground_cell := data.GetParam('ground_cell', 's32') as TIntegerValue;
	_conn_reg := data.GetParam('conn_reg', 's32') as TIntegerValue;
	_pe_index := data.GetParam('pe_index', 'u16') as TIntegerValue;
	
	// create matrix
	dir := pvDecompress(packed_dir);
	Normalize(dir);
	
	up.x := 0; up.y := 1; up.z := 0;
	Cross(right, up, dir);
	Normalize(right);
	
	Cross(up, dir, right);
	Normalize(up);
	
	m[1,1] := right.x; m[1,2] := up.x; m[1,3] := dir.x; m[1,4] := 0;
	m[2,1] := right.y; m[2,2] := up.y; m[2,3] := dir.y; m[2,4] := 0;
	m[3,1] := right.z; m[3,2] := up.z; m[3,3] := dir.z; m[3,4] := 0;
	m[4,1] := pos.x; m[4,2] := pos.y; m[4,3] := pos.z; m[4,4] := 1;
	FMatrix := m;
	
	// create PhysX actor
	ph_scene := _ph_scene;
	
	dim.x := 0.05; dim.y := 0.25; dim.z := 0.1;
	offset.x := 0; offset.y := 0.25; offset.z := 0.1;
	desc := PHShapeBox(@dim, @offset);
	
	ph_actor := PHCreateActor(ph_scene, 1, 1, @desc, @m);
	PHSetUserdata(ph_actor, self);
	PHSetGroup(PHGetShape(ph_actor, 0), PH_GROUP_UCOVER);
end;

destructor TCover.Destroy;
begin
	PHRemoveActor(ph_scene, ph_actor);
	inherited;
end;

procedure TCover.GetBBox(out b : TAABB);
begin
	b.min.x := -0.25;
	b.min.y := 0;
	b.min.z := -0.25;
	b.max.x := 0.25;
	b.max.y := 0.5;
	b.max.z := 0.25;
	AABBTransform(b, self.FMatrix);
end;

function TCover.ConnectedToAimap : Boolean;
begin
	ConnectedToAimap := True;
	if (_ground_cell <> nil) and (_conn_reg <> nil) and (_pe_index <> nil) then
		if (_ground_cell.num < 0) and (_conn_reg.num < 0) and (_pe_index.num = 65535) then
			ConnectedToAimap := False;
end;

function TCover.Pos : TVec3;
begin
	Result.x := FMatrix[4,1];
	Result.y := FMatrix[4,2];
	Result.z := FMatrix[4,3];
end;

procedure TCover.SetMatrix(const m : TMatrix);
var
	fake_scale : TVec3;
	dir : TVec3;
	
	param_pos : TFloatArrayValue;
	param_dir : TIntegerValue;
begin
	FMatrix := m;
	NormalizeMatrixAndGetScale(FMatrix, fake_scale);
	PHSetGlobalPoseM44(ph_actor, @FMatrix);
	
	param_pos := data.GetParam('position', 'vec3f') as TFloatArrayValue;
	param_dir := data.GetParam('direction', 'u16') as TIntegerValue;
	
	param_pos.data[0] := FMatrix[4,1];
	param_pos.data[1] := FMatrix[4,2];
	param_pos.data[2] := FMatrix[4,3];
	
	dir.x := FMatrix[1,3];
	dir.y := FMatrix[2,3];
	dir.z := FMatrix[3,3];
	param_dir.num := pvCompress(dir);
end;

function TCover.GetId : Word;
begin
	GetId := data.GetInt('cover_id', 'u16');
end;

constructor TCoverLink.Create(_ph_scene : TPHScene; _data : TSection; const covers : TCoversDB);
begin
	inherited Create;
	
	data := _data;
	cover_from := data.GetParam('from', 'cover_link, ucover_link') as TIntegerValue;
	cover_to := data.GetParam('to', 'cover_link, ucover_link') as TIntegerValue;

	ph_scene := _ph_scene;
	
	UpdatePhysics(covers);
end;

destructor TCoverLink.Destroy;
begin
	PHRemoveActor(ph_scene, ph_actor);
	inherited;
end;

procedure TCoverLink.UpdatePhysics(const covers : TCoversDB);
var
	dim : TVec3;
	offset : TVec3;
	desc : Pointer;
	m : TMatrix;
	
	p1, p2 : TVec3;
	dir, right, up : TVec3;
	
	l : Single;
	
	is_PE_link : Boolean;
begin
	PHRemoveActor(ph_scene, ph_actor);
	ph_actor := nil;
	
	dim.x := LINK_TIP_W; dim.y := LINK_TIP_W; dim.z := LINK_TIP_L/2*0.8;
	offset.x := 0; offset.y := 0.0; offset.z := 0.0;
	desc := PHShapeBox(@dim, @offset);
	
	if 
		(cover_from.num < 65535) and (covers[cover_from.num] <> nil) and
		(cover_to.num < 65535) and (covers[cover_to.num] <> nil) then 
	begin
		p1 := covers[cover_from.num].pos;
		p2 := covers[cover_to.num].pos;
		
		p1.y := p1.y + 0.5;
		p2.y := p2.y + 0.5;
		
		dir.x := p1.x - p2.x;
		dir.y := p1.y - p2.y;
		dir.z := p1.z - p2.z;
		NormalizeLen(dir, l);
		
		if l > 0.0 then
		begin
		
			MakeMatrixFromDir(right, up, dir);
		
			m[1,1] := right.x; m[1,2] := up.x; m[1,3] := dir.x; m[1,4] := 0;
			m[2,1] := right.y; m[2,2] := up.y; m[2,3] := dir.y; m[2,4] := 0;
			m[3,1] := right.z; m[3,2] := up.z; m[3,3] := dir.z; m[3,4] := 0;
			m[4,1] := p2.x + dir.x * (LINK_TIP_L/2);
			m[4,2] := p2.y + dir.y * (LINK_TIP_L/2);
			m[4,3] := p2.z + dir.z * (LINK_TIP_L/2);
			m[4,4] := 1.0;
	
			ph_actor := PHCreateActor(ph_scene, 1, 1, @desc, @m);
			PHSetUserdata(ph_actor, self);
			
			is_PE_link := (data.GetParam('move_action', 'stringz') <> nil);
			if is_PE_link then
				PHSetGroup(PHGetShape(ph_actor, 0), PH_GROUP_UCOVER_LINK)
			else
				PHSetGroup(PHGetShape(ph_actor, 0), PH_GROUP_UCOVER_LINK_PE)	
		end;
	end;
end;

function GetCenter(const covers : TCoverArray) : TVec3; overload;
var
	I : Longint;
	Count : Longint;
begin
	Result := covers[0].Pos;
	Count := Length(covers);
	
	for I := 1 to Count - 1 do
	begin
		Result.x := Result.x + covers[I].Pos.x;
		Result.y := Result.y + covers[I].Pos.y;
		Result.z := Result.z + covers[I].Pos.z;
	end;
	
	Result.x := Result.x / Count;
	Result.y := Result.y / Count;
	Result.z := Result.z / Count;
end;

procedure LoadCovers(var covers : TCoversDB; sect : TSection; ph_scene : TPHScene);
var
	I, J : Longint;
	cover_count : Longint;
	cover_id : Longword;
	data : TSection;
begin
	// Clear DB first
	FillChar(covers[0], 65534 * Sizeof(Pointer), #0);
	
	// Load Covers
	cover_count := sect.GetInt('count', 'u32');
	for I := 0 to cover_count - 1 do
	begin
		data := sect.GetParam(I+1) as TSection;
		
		cover_id := data.GetInt('cover_id', 'u16');
		if cover_id >= 65535 then
			raise Exception.Create('Invalid cover ID found: ' + IntToStr(cover_id));
			
		if covers[cover_id] <> nil then
			raise Exception.Create('Conflicting cover ID found: ' + IntToStr(cover_id));
		
		covers[cover_id] := TCover.Create(ph_scene, data);
	end;
	
	WriteLn(cover_count, ' covers loaded');
	
	// RESEARCH
	J := 0;
	for I := 0 to 65534 do
	begin
		if Assigned(covers[I]) and (covers[I].data.GetInt('pe_index', 'u16') <> 65535) then
			Inc(J);
	end;
	WriteLn(J, ' pathengine covers');
	
	// RESEARCH
	J := 0;
	for I := 0 to 65534 do
	begin
		if Assigned(covers[I]) and (covers[I].data.GetInt('cover_type_id', 'u8') = 1) then
			Inc(J);
	end;
	WriteLn(J, ' covers with ''human_combat'' type');
	
	// RESEARCH
	J := 0;
	for I := 0 to 65534 do
	begin
		if Assigned(covers[I]) and (covers[I].data.GetInt('cover_type_id', 'u8') = 1) and (covers[I].data.GetInt('pe_index', 'u16') <> 65535) then
			Inc(J);
	end;
	WriteLn(J, ' covers with ''human_combat'' type and ''pe_index'' <> 65535');
end;

procedure LoadCoverLinks(var links : TCoverLinkArray; sect : TSection; ph_scene : TPHScene; const covers : TCoversDB);
var
	I : Longint;
	link_count : Longint;
	data : TSection;
begin

	link_count := sect.GetInt('count', 'u32');
	SetLength(links, link_count);
	
	for I := 0 to link_count - 1 do
	begin
		data := sect.GetParam(I+1) as TSection;
		
		// TODO check from and to IDs here
		
		links[I] := TCoverLink.Create(ph_scene, data, covers);
	end;
	
	WriteLn(link_count, ' cover links loaded');
end;

procedure TCoverManager.LoadCoversAndLinks2033(levelbin_root : TSection; ph_scene : TPHScene);
begin
	konf_covers := nil;
	sect_covers := levelbin_root.GetSect('ucovers');
	
	sect_ulinks := levelbin_root.GetSect('ulinks');
	sect_ulinks_pe := levelbin_root.GetSect('ulinks_pe');
	
	LoadCovers(self.covers, sect_covers, ph_scene);
	LoadCoverLinks(self.links, sect_ulinks, ph_scene, covers);
	LoadCoverLinks(self.links_pe, sect_ulinks_pe, ph_scene, covers);
end;

procedure TCoverManager.LoadCoversAndLinksLL(const fn : String; ph_scene : TPHScene);
var
	r : TMemoryReader;
	K : TKonfig;
	TK : TTextKonfig;
	framework : TFramework;
	
	script_name : String;
begin
	r := TMemoryReader.CreateFromFile(fn);
	
	if r.ReadLongword <> NAVMAP_COVERS_SIGN then
	begin
		WriteLn('LoadCoversAndLinksLL : invalid nav_map.covers signature');
		Exit;
	end;
	
	covers_version := r.ReadLongword;
	case covers_version of
		8: script_name := 'js\nav_map.covers.js';
		11: script_name := 'js\a1\nav_map.covers.js';
		else begin
			WriteLn('LoadCoversAndLinksLL : unsupported nav_map.covers version');
			r.Free;
			Exit;
		end;
	end;
	
	K := TKonfig.Create;
	K.Load(r);
	
	framework := TFramework.Create;
	TK := framework.DecompileKonfig(K, script_name);
	
	self.konf_covers := TK;
	self.sect_covers := TK.root.GetSect('ucovers');
	
	self.sect_ulinks := TK.root.GetSect('ulinks');
	self.sect_ulinks_pe := TK.root.GetSect('ulinks_pe');
	
	LoadCovers(self.covers, sect_covers, ph_scene);
	LoadCoverLinks(self.links, sect_ulinks, ph_scene, covers);
	LoadCoverLinks(self.links_pe, sect_ulinks_pe, ph_scene, covers);
	
	framework.Free;
	K.Free;
	r.Free;
end;

procedure TCoverManager.LoadHumanCombatCovers(const fn : String; ph_scene : TPHScene);
var
	f : TMemoryReader;
	version : Longint;
	cover_count : Longint;
	
	I : Longint;
begin
	f := TMemoryReader.CreateFromFile(fn);
	try
		// handle empty file
		if f.Size = 0 then
			Exit;
		
		version := f.ReadLongint;
		if (version <> 3) and (version <> 8) then
		begin
			WriteLn('Unsupported human combat covers version ', version);
			Exit;
		end;
		
		cover_count := f.ReadLongint;
		SetLength(human_combat_covers, cover_count);
		
		for I := 0 to cover_count - 1 do
		begin
			human_combat_covers[I] := THumanCombatCover.Create(ph_scene);
			human_combat_covers[I].Load(f);
		end;
		
		// Last Light
		if version = 8 then
		begin
			for I := 0 to cover_count - 1 do
				human_combat_covers[I].LoadLLExtraData(f);
			
			if f.ReadByte <> 1 then
				WriteLn('ERROR LoadHumanCombatCovers : missing 1 at end of file');
		end;
		
		//
		for I := 0 to cover_count - 1 do
			human_combat_covers[I].UpdatePhysics;
		
		WriteLn(cover_count, ' human combat covers loaded');
		
		// some statistics
		StatHumanCombatCovers;
	finally
		f.Free;
	end;
end;

procedure TCoverManager.Save(const level_dir : String);
var
	K : TKonfig;
	w : TMemoryWriter;
begin
	if self.sect_covers <> nil then 
		uEditorUtils.RenumberArray(self.sect_covers, 'rec_', 4);
	if self.sect_ulinks <> nil then 
		uEditorUtils.RenumberArray(self.sect_ulinks, 'rec_', 4);
	//CalculatePELinks;
	if self.sect_ulinks_pe <> nil then 
		uEditorUtils.RenumberArray(self.sect_ulinks_pe, 'rec_', 4);
	
	if self.konf_covers <> nil then
	begin
		K := TKonfig.Create;
		K.kind := konfDiktionary or konfNoSections;
		
		K.Compile(self.konf_covers, True);
		
		w := TMemoryWriter.Create;
		w.WriteLongword(NAVMAP_COVERS_SIGN);
		w.WriteLongword(covers_version);
		K.Save(w);
		
		w.SaveTo(level_dir + '\nav_map.covers');
		w.Free;
		
		K.Free;
	end;
end;

procedure TCoverManager.SaveHumanCombatCovers2033(const fn : String);
var
	w : TMemoryWriter;
	I : Longint;
begin
	w := TMemoryWriter.Create;
	try
		if Length(human_combat_covers) > 0 then
		begin
			w.WriteLongword(3); // version
			w.WriteLongword(Length(human_combat_covers));
			
			for I := 0 to Length(human_combat_covers)-1 do
				human_combat_covers[I].Save(w);
		end;
		
		w.SaveTo(fn);
	finally
		w.Free;
	end;
end;

procedure TCoverManager.UnloadPELinks;
var
	I : Longint;
begin
	for I := 0 to Length(links_pe) - 1 do
		links_pe[I].Free;
	SetLength(links_pe, 0);	
end;

procedure TCoverManager.UnloadAll;
var
	I : Longint;
begin
	for I := 0 to 65534 do
		covers[I].Free;
	FillChar(covers[0], 65534 * Sizeof(Pointer), #0);

	for I := 0 to Length(links) - 1 do
		links[I].Free;
	SetLength(links, 0);
	
	for I := 0 to Length(human_combat_covers) - 1 do
		human_combat_covers[I].Free;
	SetLength(human_combat_covers, 0);
	
	UnloadPELinks;
end;

procedure TCoverManager.AddCover(cover : TCover);
begin
	if self.covers[cover.ID] <> nil then
		raise Exception.Create('Cover ID ''' + IntToStr(cover.ID) + ''' is already occupied!');
		
	sect_covers.items.Add(cover.data);
	(sect_covers.GetParam('count', 'u32') as TIntegerValue).num := sect_covers.ParamCount-1;
	
	self.covers[cover.ID] := cover;
end;

procedure TCoverManager.RemoveCover(cover : TCover);
var
	links_to_delete : TList;
	I : Longint;
	
	cover_data : TSection;
begin
	// remove all the links associated with that cover
	links_to_delete := TList.Create;
	
	for I := 0 to Length(self.links) - 1 do
		if (self.links[I].cover_from.num = cover.ID) or (self.links[I].cover_to.num = cover.ID) then
			links_to_delete.Add(self.links[I]);
			
	for I := 0 to links_to_delete.Count - 1 do
		RemoveLink(TCoverLink(links_to_delete[I]));

	links_to_delete.Free;
	
	// remove PE links associated with that cover
	links_to_delete := TList.Create;
	
	for I := 0 to Length(self.links_pe) - 1 do
		if (self.links_pe[I].cover_from.num = cover.ID) or (self.links_pe[I].cover_to.num = cover.ID) then
			links_to_delete.Add(self.links_pe[I]);
			
	for I := 0 to links_to_delete.Count - 1 do
		RemovePELink(TCoverLink(links_to_delete[I]));

	links_to_delete.Free;
			
	// remove cover itself
	cover_data := cover.data;
			
	self.covers[cover.ID] := nil;
	cover.Free;
	
	self.sect_covers.items.Remove(cover_data);
	cover_data.Free;
	
	// Renumber arrays here ?
	// No, better do it in DeleteSelection
	// Or even at saving stage
end;

function GetShapeTypeForCoverType(cover_type : Longint) : Longint;
var
	cover_types : TCoverTypeArray;
	shape_types : TShapeTypeArray;
	shape_inner_types : TInnerShapeTypeArray;
	
	I, J, K : Longint;
begin
	Result := -1;
	
	cover_types := Engine.GetCoverTypes;
	shape_types := Engine.GetShapeTypes;
	shape_inner_types := Engine.GetInnerShapeTypes;
	
	for I := 0 to Length(cover_types) - 1 do
	begin
		if cover_types[I].id = cover_type then
		begin
			for J := 0 to Length(shape_types) - 1 do
			begin
				if shape_types[J].id = cover_types[I].shape then
				begin
					for K := 0 to Length(shape_inner_types) - 1 do
					begin
						if shape_inner_types[K].id = shape_types[J].inner_type then
						begin
							Result := shape_inner_types[K].id;
							
							Break;
						end;
					end;
					
					Break;
				end;
			end;
			
			Break;
		end;
	end;
end;

procedure TCoverManager.UpdateCoverPosition(cover : TCover);
var
	I : Longint;
	cover_id : Word;
	
	ground_cell : Longint;
	conn_reg : Longint;
	shape_id : Longint;
begin
	cover_id := cover.ID;
	
	if cover.ConnectedToAimap then
	begin
	
		shape_id := GetShapeTypeForCoverType(cover.data.GetInt('cover_type_id', -1, 'u8'));
		
		if shape_id >= 0 then
		begin
		
		// update values for PathEngine
		//shape_id := 0; // what shape should we use ?
		
		navmap_server.GetCellAndConnRegForCover(
			cover.Pos.x,
			cover.Pos.y,
			cover.Pos.z,
			shape_id,
			ground_cell,
			conn_reg
		);
		
		cover._ground_cell.num := ground_cell;
		cover._conn_reg.num := conn_reg;
		
		end else
		begin
		
		cover._ground_cell.num := -1;
		cover._conn_reg.num := -1;
		
		end;
	end;

	// update links
	for I := 0 to Length(links) - 1 do
	begin
		if (links[I].cover_from.num = cover_id) or (links[I].cover_to.num = cover_id) then
			links[I].UpdatePhysics(self.covers);
	end;
end;

function TCoverManager.FindLink(cover_id_from, cover_id_to : Word) : TCoverLink;
var
	I : Longint;
begin
	for I := 0 to Length(self.links) - 1 do
		if (self.links[I].cover_from.num = cover_id_from) and (self.links[I].cover_to.num = cover_id_to) then
		begin
			FindLink := self.links[I];
			Exit;
		end;
		
	FindLink := nil;
end;

procedure TCoverManager.AddLink(link : TCoverLink);
var
	len : Longint;
begin
	len := Length(self.links);
	SetLength(self.links, len+1);
	self.links[len] := link;
	
	self.sect_ulinks.items.Add(link.data);
	(sect_ulinks.GetParam('count', 'u32') as TIntegerValue).num := sect_ulinks.ParamCount-1;
end;

procedure TCoverManager.RemoveLink(link : TCoverLink);
var
	I : Longint;
begin
	for I := 0 to Length(self.links) - 1 do
	begin
		if self.links[I] = link then
		begin
			Delete(self.links, I, 1);
			Exit;
		end;
	end;
end;

procedure TCoverManager.RemovePELink(link : TCoverLink);
var
	I : Longint;
begin
	for I := 0 to Length(self.links_pe) - 1 do
	begin
		if self.links_pe[I] = link then
		begin
			Delete(self.links_pe, I, 1);
			Exit;
		end;
	end;
end;

procedure TCoverManager.UpdateHumanCombatCoverPosition(cover : THumanCombatCover);
var
	I : Longint;
	
	_x : Longint; // unused
	_y : Longint; // unused
begin
	for I := 0 to 5 do
	begin
		if (cover.flags and (1 shl I)) <> 0 then
		begin
			// perhaps we should throw a ray from this point to the ground and get cell for it's colliding point ?
			navmap_server.PositionNear3DPoint(
				cover.arr[I].vec1.x,
				cover.arr[I].vec1.y,
				cover.arr[I].vec1.z,
				100,
				2000,
				_x,
				_y,
				cover.arr[I].cell1
			);
			
			navmap_server.PositionNear3DPoint(
				cover.arr[I].vec2.x,
				cover.arr[I].vec2.y,
				cover.arr[I].vec2.z,
				100,
				100,
				_x,
				_y,
				cover.arr[I].cell2
			);			
		end;
	end;
end;

function TCoverManager.FindHumanCombatCover(cover_id : Word) : THumanCombatCover;
var
	I : Longint;
begin
	for I := 0 to Length(self.human_combat_covers) - 1 do
	begin
		if self.human_combat_covers[I].cover_id = cover_id then
		begin
			Result := self.human_combat_covers[I];
			Exit;
		end;
	end;
	
	Result := nil;
end;

procedure TCoverManager.DrawCovers;
var
	I : Longint;
begin
	for I := 0 to 65534 do
	begin
		if covers[I] <> nil then
		begin
			glPushMatrix;
			glMultMatrixf(@covers[I].FMatrix);
			
			if covers[I].ConnectedToAimap then
			begin
				if (covers[I]._ground_cell.num >= 0) and (covers[I]._conn_reg.num >= 0) and (covers[I]._pe_index.num <> 65535) then
				begin
					// cover on AI-map
					if covers[I].Selected then
						DrawFlag(fclGreenRoundHL)
					else 
						DrawFlag(fclGreenRound) 
				end else
				begin
					// invalid
					if covers[I].Selected then
						DrawFlag(fclRedRoundHL)
					else 
						DrawFlag(fclRedRound) 
				end;
			end else
			begin
				// cover NOT on AI-map
				if covers[I].Selected then
					DrawFlag(fclBlueRoundHL)
				else 
					DrawFlag(fclBlueRound) 
			end;
			
			glPopMatrix;
		end;
	end;
end;

procedure TCoverManager.DrawLinks;
var
	links_arr : TCoverLinkArray;
	
	I : Longint;
	
	cover_from : Word;
	cover_to : Word;
	
	p1 : TVec3;
	p2 : TVec3;
	dir : TVec3;
	right : TVec3;
	up : TVec3;
	
	c1 : TVec3;
	c2 : TVec3;
	c3 : TVec3;
	c4 : TVec3;
	
	l : Single;
begin

	if self.show_pe_links then
		links_arr := self.links_pe
	else
		links_arr := self.links;

	// draw lines
	glBegin(GL_LINES);
	glColor3f(1.0, 0.8, 0.0);
	
	for I := 0 to Length(links_arr) - 1 do
	begin
		cover_from := links_arr[I].cover_from.num;
		cover_to := links_arr[I].cover_to.num;
		
		if (cover_from = 65535) or (covers[cover_from] = nil) or
		   (cover_to = 65535) or (covers[cover_to] = nil) then
			Continue;
		
		p1 := covers[cover_from].pos;
		p2 := covers[cover_to].pos;
		
		p1.y := p1.y + 0.5;
		p2.y := p2.y + 0.5;
		
		if links_arr[I].Selected then
			glColor3f(1,1,1);
		
		glVertex3fv(@p1);
		glVertex3fv(@p2);
		
		if links_arr[I].Selected then
			glColor3f(1.0, 0.8, 0.0);
	end;
	
	glEnd;
	
	// draw arrow tips
	glDisable(GL_CULL_FACE);
	
	glBegin(GL_TRIANGLES);
	glColor3f(1.0, 0.8, 0.0);
	
	for I := 0 to Length(links_arr) - 1 do
	begin
		cover_from := links_arr[I].cover_from.num;
		cover_to := links_arr[I].cover_to.num;
		
		if (cover_from = 65535) or (covers[cover_from] = nil) or
		   (cover_to = 65535) or (covers[cover_to] = nil) then
			Continue;

		p1 := covers[cover_from].pos;
		p2 := covers[cover_to].pos;
		
		p1.y := p1.y + 0.5;
		p2.y := p2.y + 0.5;
		
		dir.x := p1.x - p2.x;
		dir.y := p1.y - p2.y;
		dir.z := p1.z - p2.z;
		NormalizeLen(dir, l);
		
		if l = 0.0 then
			Continue;
		
		MakeMatrixFromDir(right, up, dir);
		
		c1.x := p2.x + (dir.x * LINK_TIP_L + up.x * LINK_TIP_W + right.x * LINK_TIP_W);
		c1.y := p2.y + (dir.y * LINK_TIP_L + up.y * LINK_TIP_W + right.y * LINK_TIP_W);
		c1.z := p2.z + (dir.z * LINK_TIP_L + up.z * LINK_TIP_W + right.z * LINK_TIP_W);
		
		c2.x := p2.x + (dir.x * LINK_TIP_L + up.x * LINK_TIP_W - right.x * LINK_TIP_W);
		c2.y := p2.y + (dir.y * LINK_TIP_L + up.y * LINK_TIP_W - right.y * LINK_TIP_W);
		c2.z := p2.z + (dir.z * LINK_TIP_L + up.z * LINK_TIP_W - right.z * LINK_TIP_W);
		
		c3.x := p2.x + (dir.x * LINK_TIP_L - up.x * LINK_TIP_W + right.x * LINK_TIP_W);
		c3.y := p2.y + (dir.y * LINK_TIP_L - up.y * LINK_TIP_W + right.y * LINK_TIP_W);
		c3.z := p2.z + (dir.z * LINK_TIP_L - up.z * LINK_TIP_W + right.z * LINK_TIP_W);
		
		c4.x := p2.x + (dir.x * LINK_TIP_L - up.x * LINK_TIP_W - right.x * LINK_TIP_W);
		c4.y := p2.y + (dir.y * LINK_TIP_L - up.y * LINK_TIP_W - right.y * LINK_TIP_W);
		c4.z := p2.z + (dir.z * LINK_TIP_L - up.z * LINK_TIP_W - right.z * LINK_TIP_W);
		
		if links_arr[I].Selected then
			glColor3f(1,1,1);
		
		glVertex3fv(@p2);
		glVertex3fv(@c1);
		glVertex3fv(@c4);
		
		glVertex3fv(@p2);
		glVertex3fv(@c2);
		glVertex3fv(@c3);
		
		if links_arr[I].Selected then
			glColor3f(1.0, 0.8, 0.0);
	end;
	
	glEnd;
	
	glEnable(GL_CULL_FACE);
end;

procedure TCoverManager.DrawHumanCombatCovers;
const
	ARROW_L = 0.5;
	ARROW_TIP_L = 0.2;
var
	I, J : Longint;
	v : TVec3;
	
	dir, right, up : TVec3;
begin
	glBegin(GL_LINES);
	
	glColor3f(1, 0, 0);
	
	for I := 0 to Length(human_combat_covers) - 1 do
	begin
		for J := 0 to 5 do
		begin
			if ((human_combat_covers[I].flags and (1 shl J)) <> 0) and human_combat_covers[I].e_visible[J] then
			begin
				v := human_combat_covers[I].arr[J].vec1;
				glVertex3f(v.x - 0.1, v.y, v.z);
				glVertex3f(v.x + 0.1, v.y, v.z);
				glVertex3f(v.x, v.y - 0.1, v.z);
				glVertex3f(v.x, v.y + 0.1, v.z);
				glVertex3f(v.x, v.y, v.z - 0.1);
				glVertex3f(v.x, v.y, v.z + 0.1);
				
				dir := pvDecompress(human_combat_covers[I].arr[J].packed_dir);
				MakeMatrixFromDir(right, up, dir);
				
				glVertex3f(v.x, v.y, v.z);
				glVertex3f(v.x + dir.x * ARROW_L, v.y + dir.y * ARROW_L, v.z + dir.z * ARROW_L);
				
				// TODO draw proper arrow
			end;
		end;
	end;
	
	glColor3f(0, 0, 1);
	
	for I := 0 to Length(human_combat_covers) - 1 do
	begin
		for J := 0 to 5 do
		begin
			//if human_combat_covers[I].arr[J].flags2 <> 0 then
			if ((human_combat_covers[I].flags and (1 shl J)) <> 0) and human_combat_covers[I].e_visible[J] then
			begin
				v := human_combat_covers[I].arr[J].vec2;
				glVertex3f(v.x - 0.1, v.y, v.z);
				glVertex3f(v.x + 0.1, v.y, v.z);
				glVertex3f(v.x, v.y - 0.1, v.z);
				glVertex3f(v.x, v.y + 0.1, v.z);
				glVertex3f(v.x, v.y, v.z - 0.1);
				glVertex3f(v.x, v.y, v.z + 0.1);
			end;
		end;
	end;
	
	// Last Light
	glColor3f(1, 0.4, 0);
	
	for I := 0 to Length(human_combat_covers) - 1 do
	begin
		//if human_combat_covers[I].arr[J].flags2 <> 0 then
		begin
		v := human_combat_covers[I].pos;
		glVertex3f(v.x - 0.1, v.y, v.z);
		glVertex3f(v.x + 0.1, v.y, v.z);
		glVertex3f(v.x, v.y - 0.1, v.z);
		glVertex3f(v.x, v.y + 0.1, v.z);
		glVertex3f(v.x, v.y, v.z - 0.1);
		glVertex3f(v.x, v.y, v.z + 0.1);
		end;
	end;
	
	glEnd;
end;

function TCoverManager.GenerateCoverId : Word;
var
	I : Integer;
begin
	I := 0;
	while (self.covers[I] <> nil) and (I < 65535) do
		Inc(I);
	Result := I;
end;

function TCoverManager.GenerateCoverPEIndex : Word;
var
	I, J : Integer;
	bFound : Boolean;
begin
	I := 0;
	
	while  (I < 65535) do
	begin
		bFound := False;
		
		for J := 0 to 65534 do
			if Assigned(self.covers[J]) and (self.covers[J].data.GetInt('pe_index', 'u16') = I) then
			begin
				bFound := True;
				Break;
			end;
			
		if bFound then
			Inc(I)
		else
			Break;
	end;
	
	Result := I;
end;

type
	TTempLink = record
		from : Word;
		_to : Word;
		len : Single;
	end;
	
function RecStr(const pref : String; num : Longint; digits : Longint) : String;
var
	n : String;
begin
	n := IntToStr(num);
	RecStr := pref + StringOfChar('0', digits-Length(n)) + n;
end;

procedure TCoverManager.CalculatePELinks(ph_scene : TPHScene);
var
	I, J: Longint;
	new_links : array of TTempLink;
	already_checked : array[0..65535] of Boolean;
	
	cover_type_id : Longint;
	
	sect : TSection;
	
//	sl1 : TStringList;
//	sl2 : TStringList;
	
	procedure EnumAllReachableCovers(src_cover : TCover; cur_cover : TCover; dist : Single);
	var
		L : Longint;
		cover_type_id : Longint;
	begin
		for L := 0 to Length(self.links) - 1 do
		begin
			if (self.links[L].cover_from.num = cur_cover.ID) and 
			   (not already_checked[self.links[L].cover_to.num]) then
			begin
				already_checked[self.links[L].cover_to.num] := True;
				
				if (self.links[L].cover_to.num <> 65535) and
				   (self.covers[self.links[L].cover_to.num] <> nil) then
				begin
					if (self.covers[self.links[L].cover_to.num]._pe_index.num <> 65535) and (src_cover._pe_index.num <> 65535) then
					begin
					
						cover_type_id := covers[self.links[L].cover_to.num].data.GetInt('cover_type_id', 'u8');
					
						if (cover_type_id <> 1) and (cover_type_id <> 2) and (cover_type_id <> 3) then
						begin
					
						SetLength(new_links, Length(new_links)+1);
						// make link from src_cover to self.covers[self.links[L].cover_to]
						
						new_links[Length(new_links)-1].from := src_cover.ID;
						new_links[Length(new_links)-1]._to := covers[self.links[L].cover_to.num].ID;
						new_links[Length(new_links)-1].len := dist + Distance(src_cover.Pos, self.covers[self.links[L].cover_to.num].Pos);
						
						end;
					end;
					
					EnumAllReachableCovers(src_cover, self.covers[self.links[L].cover_to.num],
						dist + Distance(src_cover.Pos, self.covers[self.links[L].cover_to.num].Pos));
				end;
			end;
		end;
	end;
	
begin

	SetLength(new_links, 0);
	
	//for I := 0 to 65534 do
	for J := 1 to self.sect_covers.ParamCount - 1 do
	begin
		I := (self.sect_covers.GetParam(J) as TSection).GetInt('cover_id', 'u16');
		
		if (covers[I] <> nil) and (covers[I]._pe_index.num <> 65535) then
		begin
			cover_type_id := covers[I].data.GetInt('cover_type_id', 'u8');
			
			if (cover_type_id <> 1) and (cover_type_id <> 2) and (cover_type_id <> 3) then
			begin
			
			FillChar(already_checked, sizeof(already_checked), #0); // clear checked list
			already_checked[I] := True; // mark current cover as checked, to avoid linking cover to itself in case of loop paths
			
			EnumAllReachableCovers(covers[I], covers[I], 0.0);
			
			end;
		end;
	end;
	
	WriteLn(Length(new_links), ' links generated');
	WriteLn(sect_ulinks_pe.ParamCount-1, ' links existed before');
	
	// save
	UnloadPELinks;
	sect_ulinks_pe.Clear;
	
	sect_ulinks_pe.AddInt('count', Length(new_links), 'u32');
	for I := 0 to Length(new_links) - 1 do
	begin
		sect := sect_ulinks_pe.AddSect(RecStr('rec_', I, 4));
		sect.AddInt('from', new_links[I].from, 'cover_link, ucover_link');
		sect.AddInt('to', new_links[I]._to, 'cover_link, ucover_link');
		sect.AddInt('cost', 100 { временно }, 'u32');
		sect.AddInt('anim_state', 255, 'u8');
		sect.AddInt('mental_state', 255, 'u8');
	end;
	
	LoadCoverLinks(self.links_pe, sect_ulinks_pe, ph_scene, covers);
{
	// dump
	sl1 := TStringList.Create;
	sl2 := TStringList.Create;
	
	for I := 0 to Length(self.links) - 1 do
		sl1.Add(IntToStr(self.links[I].cover_from.num) + ' -> ' + IntToStr(self.links[I].cover_to.num) + ' (' + IntToStr(self.links[I].data.GetInt('cost', -1, 'u32')) + ')');
		
	for I := 0 to Length(new_links) - 1 do
		sl2.Add(IntToStr(new_links[I].from) + ' -> ' + IntToStr(new_links[I]._to) + ' (' + IntToStr(Trunc(new_links[I].len * 1000)) + ')');
		
	//sl1.Sort;
	//sl2.Sort;
		
	sl1.SaveToFile('E:\iup_delphi\links1.txt');
	sl2.SaveToFile('E:\iup_delphi\links2.txt');
	
	sl1.Free;
	sl2.Free;
}
end;

procedure TCoverManager.Stat;
var
	I, J : Longint;

	links_only_anim : Longint;
	links_only_pe : Longint;
	links_anim_pe : Longint;
	
	data1 : TSection;
	data2 : TSection;
	
	bFound : Boolean;
	
begin
	// research stuff !
	links_only_anim := 0;
	links_only_pe := 0;
	links_anim_pe := 0;
	
	for I := 0 to sect_ulinks.GetInt('count', 'u32') - 1 do
	begin
		data1 := sect_ulinks.GetParam(I+1) as TSection;
		
		bFound := False;
		
		for J := 0 to sect_ulinks_pe.GetInt('count', 'u32') - 1 do
		begin
			data2 := sect_ulinks_pe.GetParam(J+1) as TSection;
			
			if data1.GetInt('from', 'cover_link, ucover_link') = data2.GetInt('from', 'cover_link, ucover_link') then
				if data1.GetInt('to', 'cover_link, ucover_link') = data2.GetInt('to', 'cover_link, ucover_link') then
					bFound := True;
		end;
		
		if bFound then
			Inc(links_anim_pe)
		else
			Inc(links_only_anim)
	end;
	
	for I := 0 to sect_ulinks_pe.GetInt('count', 'u32') - 1 do
	begin
		data1 := sect_ulinks_pe.GetParam(I+1) as TSection;
		
		bFound := False;
		
		for J := 0 to sect_ulinks.GetInt('count', 'u32') - 1 do
		begin
			data2 := sect_ulinks.GetParam(J+1) as TSection;
			
			if data1.GetInt('from', 'cover_link, ucover_link') = data2.GetInt('from', 'cover_link, ucover_link') then
				if data1.GetInt('to', 'cover_link, ucover_link') = data2.GetInt('to', 'cover_link, ucover_link') then
					bFound := True;
		end;
		
		if not bFound then
			Inc(links_only_pe)
	end;
	
	WriteLn(links_only_anim, ' only exists in ulinks');
	WriteLn(links_only_pe, ' only exists in ulinks_pe');
	WriteLn(links_anim_pe, ' exists both in ulinks and ulinks_pe');
	
	J := 0;
	for I := 0 to sect_ulinks.GetInt('count', 'u32') - 1 do
	begin
		data1 := sect_ulinks.GetParam(I+1) as TSection;
		if (data1.GetInt('lnk_type', -1, 'u32') and 4) <> 0 then
			Inc(J);
	end;
	
	writeln(J, ' links of type 4');
end;

procedure TCoverManager.StatHumanCombatCovers;
var
	I,J,K : Longint;
begin
	K := 0;
	for I := 0 to Length(human_combat_covers) - 1 do
	begin
		for J := 0 to 5 do
		begin
			if (human_combat_covers[I].flags and (1 shl J)) <> 0 then
				Inc(K);
									
		end;
	end;
	
	WriteLn('.. ', K, ' points total ');
end;

end.