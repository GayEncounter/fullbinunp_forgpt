unit Engine;

interface
uses classes, Konfig, vmath;

type
	TEngineVersion = (
		eVer2033,
		eVerLLBeta15102012,
		eVerLLBeta03122012,
		eVerLL,
		eVerRedux,
		eVerArktika1,
		eVerExodus,
		eVerUnknown
	);

var
	version : TEngineVersion = eVerUnknown;	
	ResourcesPath : String = 'content';
	LevelPath : String;

procedure InitializeEngine;
procedure FinalizeEngine;

function GetSoundParams : TTextKonfig;
function GetCoverManagerParams : TTextKonfig;
function GetCameraTracksList : TStringList;

// AI-related shid
type
	TInnerShapeType = record
		name   : String;
		id     : Longint;
		radius : Single;
	end;
	
	TInnerShapeTypeArray = array of TInnerShapeType;
	
function GetInnerShapeTypes : TInnerShapeTypeArray;

type
	TShapeType = record
		name       : String;
		id         : Longint;
		inner_type : Longint;
	end;
	
	TShapeTypeArray = array of TShapeType;
	
function GetShapeTypes : TShapeTypeArray;

type 
	TCoverType = record
		id            : Longint;
		name          : String;
		inner_type    : Longint;
		aabb_min      : TVec3;
		aabb_max      : TVec3;
		shape         : Byte;
		icon          : Byte;
		preview_model : String;
		g2g           : Boolean; // Last Light
		radius        : Byte;    // Last Light
	end;
	
	TCoverTypeArray = array of TCoverType;
	
function GetCoverTypes : TCoverTypeArray;

implementation
uses sysutils, Konfig_reader, framework, chunkedFile, KonfigLibrary, texturePrefs, Texture, uWeather;

var
	sound_params : TTextKonfig;
	cover_manager_params : TTextKonfig;
	camera_tracks_list : TStringList;

var
	loaded_inner_shape_types : TInnerShapeTypeArray;
	loaded_shape_types : TShapeTypeArray;
	loaded_cover_types : TCoverTypeArray;

function GuessEngineVer : TEngineVersion;
var
	has_scriptsbin : Boolean;
	has_configbin : Boolean;
	has_texturesbin : Boolean;
	has_textures_handles_storage : Boolean;
	has_vr_attach_base : Boolean;
begin
	has_scriptsbin := False;
	has_configbin := False;
	has_texturesbin := False;
	has_textures_handles_storage := False;
	has_vr_attach_base := False;
	
	if FileExists(ResourcesPath + '\scripts.bin') then
	begin
		has_scriptsbin := True;
	end else
	if FileExists(ResourcesPath + '\config.bin') then
	begin
		has_configbin := True;
	end;

	if FileExists(ResourcesPath + '\textures\textures.bin') then
	begin
		has_texturesbin := True;
	end;
	
	if FileExists(ResourcesPath + '\textures_handles_storage.bin') then
	begin
		has_textures_handles_storage := True;
	end;
	
	// не самый лучший спобов отличать арктику.1
	// т.к. этот файл распаковывать необязательно и редактору он нафиг не нужен
	// но я ничего лучше не вижу
	if FileExists(ResourcesPath + '\weaponry\attaches\vr_attach_base.bin') then
	begin
		has_vr_attach_base := True;
	end;
	
	if has_scriptsbin then
	begin
		GuessEngineVer := eVer2033;
	end else
	begin
		if has_texturesbin then
		begin
			GuessEngineVer := eVerLL;
		end else
		begin
			if has_textures_handles_storage then
			begin
				GuessEngineVer := eVerExodus;
			end else
			begin
				if has_vr_attach_base then
					GuessEngineVer := eVerArktika1
				else
					GuessEngineVer := eVerRedux;
			end;
		end;
	end;	
end;

procedure InitializeEngine;
var
	tex_prefs_version : TTBVersion;
begin
	if version = eVerUnknown then
		version := GuessEngineVer;
	
	Write('Engine version is ');
	case version of
		eVer2033:           Write('Metro 2033');
		eVerLL:             Write('Metro Last Light');
		eVerLLBeta15102012: Write('Build 2662');		
		eVerLLBeta03122012: Write('Build 2711');
		eVerRedux:          Write('Metro Redux');
		eVerArktika1:       Write('Arktika.1');
		eVerExodus:         Write('Metro Exodus');
		eVerUnknown:        Write('Unknown');
	end;
	WriteLn;
	
	if version = eVer2033 then
	begin
		if FileExists(ResourcesPath + '\scripts.bin') then
			KonfigLibrary.Load(ResourcesPath + '\scripts.bin')
		else
			WriteLn('content\scripts.bin not found');
	end else
	begin
		if FileExists(ResourcesPath + '\config.bin') then
			KonfigLibrary.Load(ResourcesPath + '\config.bin')
		else
			WriteLn('content\config.bin not found');
	end;

	if FileExists(ResourcesPath + '\textures\textures.bin') then
	begin
		if version in [eVerLLBeta15102012, eVerLLBeta03122012] then
			tex_prefs_version := tbVerLLBeta15102012
		else
			tex_prefs_version := tbVerUnknown;
			
		try
			texture_params := TTexturesBin.CreateAndLoad(ResourcesPath + '\textures\textures.bin', tex_prefs_version);
		except on E: Exception do
			WriteLn('textures.bin loading failed'#10 + E.ClassName + ': ' + E.Message);
		end;
	end;
	
	if FileExists(ResourcesPath + '\scripts\texture_aliases.bin') then
	begin
		try
			texture_aliases := TTextureAliases.CreateAndLoad(ResourcesPath + '\scripts\texture_aliases.bin');
		except on E: Exception do
			WriteLn('texture_aliases.bin loading failed'#10 + E.ClassName + ': ' + E.Message);
		end;
	end;
	
	if FileExists(ResourcesPath + '\textures_handles_storage.bin') then
	begin
		texture_params2 := TTexturesBin2.Create;
		texture_params2.Load(ResourcesPath + '\textures_handles_storage.bin');
	end;
	
	uWeather.Initialize;
end;

procedure FinalizeEngine;
begin
	uWeather.Finalize;

	FreeAndNil(texture_params);
	FreeAndNil(texture_params2);
	FreeAndNil(texture_aliases);
	
	FreeAndNil(sound_params);
	
	FreeAndNil(camera_tracks_list);
end;

function GetSoundParams : TTextKonfig;
var
	K : TKonfig;
	js : TFramework;
begin
	if sound_params = nil then
	begin
		if FileExists(ResourcesPath + '\sounds\sounds.bin') then
		begin
			js := TFramework.Create;
			
			K := TKonfig.Create;
			K.Load(ResourcesPath + '\sounds\sounds.bin');
			
			case version of
				eVer2033:           sound_params := js.DecompileKonfig(K, 'js\2033\sounds.js');
				eVerLLBeta15102012: sound_params := js.DecompileKonfig(K, 'js\ll_beta_15_10_2012\sounds.js');
				eVerLLBeta03122012,
				eVerLL:             sound_params := js.DecompileKonfig(K, 'js\ll\sounds.js');
				eVerRedux:          sound_params := js.DecompileKonfig(K, 'js\redux\sounds.js');
			end;
			
			K.Free;
			js.Free;
		end;
	end;
	
	Result := sound_params;
end;

function GetCoverManagerParams : TTextKonfig;
var
	K : TKonfig;
	js : TFramework;
begin
	if cover_manager_params = nil then
	begin
		K := KonfigLibrary.GetKonfig('cover_manager');
		if K <> nil then
		begin
			js := TFramework.Create;
			
			case version of
				eVer2033:           cover_manager_params := js.DecompileKonfig(K, 'js\2033\cover_manager.js');
				eVerLLBeta15102012,
				eVerLLBeta03122012,
				eVerLL,
				eVerRedux:          cover_manager_params := js.DecompileKonfig(K, 'js\ll\cover_manager.js');
			end;
			
			K.Free;
			js.Free;
		end;
	end;
	
	Result := cover_manager_params;
end;

function GetCameraTracksList : TStringList;
var
	r : TMemoryReader;
	I, count : Longint;
	size : Longword;
	name : String;
begin
	if camera_tracks_list = nil then
	begin
		camera_tracks_list := TStringList.Create;
		
		r := nil;
		
		try
			r := TMemoryReader.CreateFromFile(ResourcesPath + '\anims\anims.bin');
			try
				if r.ReadLongint <> 1 then
					raise Exception.Create('Invalid camera tracks version!');
					
				count := r.ReadLongint;
				
				for I := 0 to count - 1 do
				begin
					r.ReadLongword; { ID }
					size := r.ReadLongword;
					name := r.ReadStringZ;
					
					camera_tracks_list.Add(name);
					
					Inc(r.pos, size - (Length(name)+1)); // skip the rest
				end;
			finally
				r.Free;
			end;
		except 
			on E: Exception do
			begin
				WriteLn('! Error loading camera tracks');
				WriteLn(E.ClassName + ': ' + E.Message);
			end;
		end;
	end;
	
	Result := camera_tracks_list;
end;

function GetInnerShapeTypes : TInnerShapeTypeArray;
var
	K : TKonfig;
	kr : TKonfigReader;
	
	arr : IKonfigReader;
	elem : IKonfigReader;
	
	st : TInnerShapeType;
begin
	if loaded_inner_shape_types = nil then
	begin
		K := KonfigLibrary.GetKonfig('inner_shape_type');
		if K <> nil then
		begin
			kr := TKonfigReader.Create(K, nil, nil);
			
			arr := kr.ReadArray('inner_shape_type');
			while arr.MoreElements do
			begin
				elem := arr.NextElement;
				
				st.name   := elem.ReadString('name');
				st.id     := elem.ReadU32('id');
				st.radius := elem.ReadFP32('radius');
				
				Insert(st, loaded_inner_shape_types, Length(loaded_inner_shape_types));
				
				elem.Free;
			end;
			
			arr.Free;
			
			kr.Free;
			K.Free;
		end;
	end;
	
	Result := loaded_inner_shape_types;
end;

function GetShapeTypes : TShapeTypeArray;
var
	K : TKonfig;
	kr : TKonfigReader;
	
	arr : IKonfigReader;
	elem : IKonfigReader;
	
	st : TShapeType;
begin
	if (loaded_shape_types = nil) and (version < eVerExodus) { not supported } then
	begin
		K := KonfigLibrary.GetKonfig('shape_type');
		if K <> nil then
		begin
			kr := TKonfigReader.Create(K, nil, nil);
			
			arr := kr.ReadArray('shape_type');
			while arr.MoreElements do
			begin
				elem := arr.NextElement;
				
				st.name       := elem.ReadString('name');
				st.id         := elem.ReadU32('id');
				st.inner_type := elem.ReadU32('inner_type');
				
				Insert(st, loaded_shape_types, Length(loaded_shape_types));
				
				elem.Free;
			end;
			
			arr.Free;
			
			kr.Free;
			K.Free;
		end;
	end;
	
	Result := loaded_shape_types;
end;

function GetCoverTypes : TCoverTypeArray;
var
	K : TKonfig;
	kr : TKonfigReader;
	
	arr : IKonfigReader;
	elem : IKonfigReader;
	
	ct : TCoverType;
begin
	if (loaded_cover_types = nil) and (version < eVerArktika1) { not supported } then
	begin
		K := KonfigLibrary.GetKonfig('cover_manager');
		if K <> nil then
		begin
			kr := TKonfigReader.Create(K, nil, nil);
			
			// skip, I'm not interested in that
			arr := kr.ReadArray('cover_inner_type');
			while arr.MoreElements do
			begin
				elem := arr.NextElement;
	
				elem.ReadU32('id');
				elem.ReadString('name');
				
				elem.Free;
			end;
			
			arr := kr.ReadArray('cover_type');
			while arr.MoreElements do
			begin
				elem := arr.NextElement;

				ct.id            := elem.ReadU32('id');
				ct.name          := elem.ReadString('name');
				ct.inner_type    := elem.ReadU32('type');
				ct.aabb_min      := elem.ReadVec3('aabb_min');
				ct.aabb_max      := elem.ReadVec3('aabb_max');
				ct.shape         := elem.ReadU8('shape');
				ct.icon          := elem.ReadU8('icon');
				ct.preview_model := elem.ReadHintStr('model', 'ref_model');
				
				if version >= eVerLLBeta15102012 then
				begin
					ct.g2g       := elem.ReadBool('g2g');
					ct.radius    := elem.ReadU8('radius');
				end;
				
				Insert(ct, loaded_cover_types, Length(loaded_cover_types));
				
				elem.Free;
			end;
			
			arr.Free;
			
			kr.Free;
			K.Free;
		end;
	end;
	
	Result := loaded_cover_types;
end;

end.
