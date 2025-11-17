unit texturePrefs;

interface
uses vmath, Konfig, fgl;

type
	TTextureParams = record
		ttype : Longint;
		animated : Boolean;
		fmt : Longword;
		r_width, r_height : Longword;
		name : String;
		namecrc : Longint;
		bump_name : String;
		bump_height : Single;
		displ_mode : Byte;
		parr_height : Byte;
		det_name : String;
		det_u_scale, det_v_scale : Single;
		det_int : Single;
		mip_enabled, streamable, priority, isdeprecated : Boolean;
		avg_color : Longword;
	end;
	PTextureParams = ^TTextureParams;

	TTextureParams2 = record
		name : String;
		namecrc : Longint;
		_type : Longint;
		texture_type : Byte;
		source_name : String;
		surf_xform : TVec4;
		format : Longint;
		width : Longint;
		height : Longint;
		animated : Boolean;
		draft : Boolean;
		override_avg_color : Boolean;
		avg_color : TVec4;
		shader_name : String;
		gamemtl_name : String;
		priority : Longint;
		streamable : Boolean;
		bump_height : Single;
		displ_type : Byte;
		displ_height : Single;
		parallax_height_mul : Single;
		mipmapped : Boolean;
		reflectivity : Single;
		treat_as_metal : Boolean;
		det_name : String;
		det_scale_u : Single;
		det_scale_v : Single;
		det_intensity : Single;
		aux_params : TVec4;
		aux_params_1 : TVec4;
		bump_name : String;
		aux_names : array[0..7] of String;
	end;
	PTextureParams2 = ^TTextureParams2;

	TTBVersion = (tbVer2033, tbVerLLBeta15102012, tbVerLL, tbVerUnknown);
	TTexAliasesMap = TFPGMap<String,String>;

	TTexturesBin = class
		version : TTBVersion;
		aliases : TTexAliasesMap;
		options : array of TTextureParams;

		constructor Create;
		constructor CreateAndLoad(const fn : String; ver_hint : TTBVersion = tbVerUnknown); overload;
		constructor CreateAndLoad(K : TKonfig; ver_hint : TTBVersion = tbVerUnknown); overload;
		destructor Destroy; override;

		procedure Load(const fn : String; ver_hint : TTBVersion = tbVerUnknown); overload;
		procedure Load(K : TKonfig; ver_hint : TTBVersion = tbVerUnknown); overload;
		procedure Save(K : TTextKonfig);
		function Find(const name : String) : PTextureParams;

		property Items[const name : String] : PTextureParams read Find; default;
		function GetRealName(var n : String) : Boolean;
	end;

	TTexturesBin2 = class
		params : array of TTextureParams2;

		function Load(const fn : String) : Boolean;
		function Find(const name : String) : PTextureParams2;

		property Items[name : String] : PTextureParams2 read Find; default;
	end;
	
	// must die
	TTextureAliases = class
		aliases : TTexAliasesMap;
	
		constructor Create;
		constructor CreateAndLoad(const fn : String); overload;
		constructor CreateAndLoad(K : TKonfig); overload;
		destructor Destroy; override;
	
		procedure Load(K : TKonfig); overload;
		procedure Load(const filename : String); overload;
	
		function GetRealName(var n : String) : Boolean; 
	end;
	

implementation
uses chunkedFile, uCrc, Konfig_reader, sysUtils;

constructor TTexturesBin.Create;
begin
	inherited;
	aliases := TTexAliasesMap.Create;
	aliases.Sorted := True;
end;

constructor TTexturesBin.CreateAndLoad(const fn : String; ver_hint : TTBVersion); overload;
begin
	Create;
	Load(fn, ver_hint);
end;

constructor TTexturesBin.CreateAndLoad(K : TKonfig; ver_hint : TTBVersion); overload;
begin
	Create;
	Load(K, ver_hint);	
end;

destructor TTexturesBin.Destroy;
begin
	aliases.Free;
	inherited;
end;

procedure TTexturesBin.Load(const fn : String; ver_hint : TTBVersion = tbVerUnknown);
var
	r : TMemoryReader;
	k : TKonfig;
begin
	r := TMemoryReader.CreateFromFile(fn);
	k := TKonfig.Create;
	k.Load(r);
	Load(k, ver_hint);
	k.Free;
	r.Free;
end;

procedure TTexturesBin.Load(K : TKonfig; ver_hint : TTBVersion = tbVerUnknown);
var
	r : TKonfigReader;
	aliases, a : IKonfigReader;
	prefs, p : IKonfigReader;
	
	src, dst : String;
	I : Longint;
	
	count : Longword;
begin
	r := TKonfigReader.Create(K, nil, nil);
	try
		aliases := r.TryReadArray('texture_aliases');
		if aliases <> nil then
		begin
			version := tbVerLL;
			
			try
				while aliases.More do
				begin
					a := aliases.ReadSection('', False);
					src := a.ReadString('src');
					dst := a.ReadString('dst');
					a.Free;
					
					self.aliases[src] := dst; 
				end;
			finally
				aliases.Free;
			end;
		end else
		begin
			if ver_hint = tbVerUnknown then
				version := tbVer2033
			else
				version := ver_hint
		end;
	
		prefs := r.ReadArray('texture_params', @count);
		try
			I := 0;
			while prefs.More do
			begin
				SetLength(options, I+1);
		
				p := prefs.ReadSection('', False);
				try	
					with options[I] do
					begin
						ttype           := p.ReadS32('type');
						animated        := p.ReadBool('animated');
						fmt             := p.ReadU32('fmt');
						r_width         := p.ReadU32('r_width');
						r_height        := p.ReadU32('r_height');
						name            := p.ReadName('name');
						namecrc         := GetStringCrc(name);
						bump_name       := p.ReadString('bump_name');
						bump_height     := p.ReadFP32('bump_height');
						if version >= tbVerLLBeta15102012 then
							displ_mode  := p.ReadU8('displ_mode')
						else
							displ_mode  := 0;
						parr_height     := p.ReadU8('parr_height');
						det_name        := p.ReadString('det_name');
						det_u_scale     := p.ReadFP32('det_u_scale');
						det_v_scale     := p.ReadFP32('det_v_scale');
						det_int			:= p.ReadFP32('det_int');
						mip_enabled     := p.ReadBool('mip_enabled');
						streamable      := p.ReadBool('streamable');
						priority		:= p.ReadBool('priority');
						if version = tbVer2033 then
							isdeprecated := p.ReadBool('deprecated')
						else
							isdeprecated := False;
						avg_color       := p.ReadU32('avg_color');
					end;
				finally
					p.Free;
				end;
				
				Inc(I);
			end;
		finally
			prefs.Free;
		end;
	finally
		r.Free;
	end;
	
	if count <> Length(options) then
		WriteLn('texture_prefs count (', count, ') is incorrect, must be ', Length(options));
end;

procedure TTexturesBin.Save(K : TTextKonfig);
var
	I : Integer;
	s1, s2 : TSection;
	n : String;
begin
	if version >= tbVerLL then
	begin
		K.root.AddHint('texture_aliases', 'array');
		s1 := K.root.AddSect('texture_aliases');
		s1.AddInt('count', aliases.Count, 'u32');
		for I := 0 to aliases.Count - 1 do
		begin
			n := IntToStr(I);
			s2 := s1.AddSect('rec_'+StringOfChar('0',4-Length(n))+n);
			s2.AddStr('src', aliases.Keys[I]);
			s2.AddStr('dst', aliases.Data[I]);
		end;
	end;
	
	K.root.AddHint('texture_params', 'array');
	s1 := K.root.AddSect('texture_params');
	s1.AddInt('count', Length(options), 'u32');
	for I := 0 to Length(options) - 1 do
	begin
		s2 := s1.AddSect(options[I].name);
		s2.AddInt('type', options[I].ttype, 's32');
		s2.AddBool('animated', options[I].animated);
		s2.AddInt('fmt', options[I].fmt, 'u32');
		s2.AddInt('r_width', options[I].r_width, 'u32');
		s2.AddInt('r_height', options[I].r_height, 'u32');
		s2.AddHint('name', 'name');
		s2.AddStr('name', options[I].name);
		s2.AddStr('bump_name', options[I].bump_name);
		s2.AddFloat('bump_height', options[I].bump_height);
		if version >= tbVerLLBeta15102012 then
			s2.AddInt('displ_mode', options[I].displ_mode, 'u8');
		s2.AddInt('parr_height', options[I].parr_height, 'u8');
		s2.AddStr('det_name', options[I].det_name);
		s2.AddFloat('det_u_scale', options[I].det_u_scale);
		s2.AddFloat('det_v_scale', options[I].det_v_scale);
		s2.AddFloat('det_int', options[I].det_int);
		s2.AddBool('mip_enabled', options[I].mip_enabled);
		s2.AddBool('streamable', options[I].streamable);
		s2.AddBool('priority', options[I].priority);
		if version = tbVer2033 then
			s2.AddBool('deprecated', options[I].isdeprecated);
		s2.AddInt('avg_color', options[I].avg_color, 'u32');
	end;
end;

function TTexturesBin.Find(const name : String) : PTextureParams;
var
	I : Integer;
	crc : Longint;
begin
	crc := GetStringCrc(name);
	Result := nil;
	
	for I := 0 to Length(options) - 1 do
		if options[I].namecrc = crc then
			if options[I].name = name then
			begin
				Result := @options[I];
				Exit;
			end;
end;

function TTexturesBin.GetRealName(var n : String) : Boolean;
var
	idx : Integer;
begin
	if aliases.Find(n, idx) then
	begin
		n := aliases.Data[idx];
		Result := True;
	end else
		Result := False;
end;

function TTexturesBin2.Load(const fn: String) : Boolean;
var
	r, r2 : TMemoryReader;
	k : TKonfig;
	//tk : TTextKonfig;
	e : TKonfigReader;
	
	hdr : Longint; // signature AVER or count of configs
	
	I,J : Longint;
begin
	r := TMemoryReader.CreateFromFile(fn);
	
	hdr := r.ReadLongint;
	if hdr = $52455641 then
	begin
		r.ReadWord;
		SetLength(params, r.ReadLongint);
	end else
		SetLength(params, hdr);
	
	for I := 0 to Length(params) - 1 do
	begin
		r2 := r.OpenChunk(I, True);
		
		params[I].name := r2.ReadStringZ;
		params[I].namecrc := GetStringCrc(params[I].name);
		
		k := TKonfig.Create;
		k.Load(r2);
		
		r2.Free;
{
		tk := TTextKonfig.Create;
		k.Decompile(tk);
		k.Free;
		
		params[I].source_name := (tk.root.GetParam('source_name', 'stringz') as TStringValue).str;
		params[I].det_name := (tk.root.GetParam('det_name', 'stringz') as TStringValue).str;
		params[I].det_scale_u := (tk.root.GetParam('det_scale_u', 'fp32') as TSingleValue).num;
		params[I].det_scale_v := (tk.root.GetParam('det_scale_v', 'fp32') as TSingleValue).num;
		params[I].bump_name := (tk.root.GetParam('bump_name', 'stringz') as TStringValue).str;
		
		tk.Free;
}
		e := TKonfigReader.Create(k, nil, nil);
		
		with params[I] do
		begin
			_type := e.ReadU32('type');
			texture_type := e.ReadU8('texture_type');
			source_name := e.ReadString('source_name');
			surf_xform := e.ReadVec4('surf_xform');
			format := e.ReadU32('format');
			width := e.ReadU32('width');
			height := e.ReadU32('height');
			animated := e.ReadBool('animated');
			draft := e.ReadBool('draft');
			override_avg_color := e.ReadBool('override_avg_color');
			avg_color := e.ReadVec4('avg_color', 'color, vec4f');
			shader_name := e.ReadHintStr('shader_name', 'choose');
			gamemtl_name := e.ReadHintStr('gamemtl_name', 'choose');
			priority := e.ReadU32('priority');
			streamable := e.ReadBool('streamable');
			bump_height := e.ReadFP32('bump_height');
			displ_type := e.ReadU8('displ_type');
			displ_height := e.ReadFP32('displ_height');
			parallax_height_mul := e.ReadFP32('parallax_height_mul');
			mipmapped := e.ReadBool('mipmapped');
			reflectivity := e.ReadFP32('reflectivity');
			treat_as_metal := e.ReadBool('treat_as_metal');
			det_name := e.ReadHintStr('det_name', 'choose_array, str_shared');
			det_scale_u := e.ReadFP32('det_scale_u');
			det_scale_v := e.ReadFP32('det_scale_v');
			det_intensity := e.ReadFP32('det_intensity');
			aux_params := e.ReadVec4('aux_params', 'color, vec4f');
			aux_params_1 := e.ReadVec4('aux_params_1', 'color, vec4f');
			if texture_type = 3 then
				e.ReadFP32Array('sph_coefs');
			if texture_type = 12 then
				e.ReadU8Array('lum');
				
			//WriteLn('_type = ', _type);
			//WriteLn('texture_type = ', texture_type);
			
			bump_name := e.ReadHintStr('bump_name', 'choose');
			for J := 0 to 7 do
				aux_names[J] := e.ReadHintStr('aux'+IntToStr(J)+'_name', 'choose');
		end;
		
		e.Free;
		k.Free;
	end;

	r.Free;
	Load := True;
end;

function TTexturesBin2.Find(const name : String) : PTextureParams2;
var
	I : Integer;
	crc : Longint;
begin
	crc := GetStringCrc(name);
	Result := nil;
	
	for I := 0 to Length(params) - 1 do
		if params[I].namecrc = crc then
			if params[I].name = name then
			begin
				Result := @params[I];
				Exit;
			end;
end;

constructor TTextureAliases.Create;
begin
	inherited Create;
	aliases := TTexAliasesMap.Create;
end;

constructor TTextureAliases.CreateAndLoad(const fn : String); overload;
begin
	Create;
	Load(fn);
end;

constructor TTextureAliases.CreateAndLoad(K : TKonfig); overload;
begin
	Create;
	Load(K);
end;

destructor TTextureAliases.Destroy;
begin
	aliases.Free;
	inherited Destroy;
end;

procedure TTextureAliases.Load(K : TKonfig);
var
	r : TKonfigReader;
	aliases, a : IKonfigReader;
	src, dst : String;
begin
	r := TKonfigReader.Create(K, nil, nil);
	try
		aliases := r.ReadArray('texture_aliases');
		try
			while aliases.More do
			begin
				a := aliases.ReadSection('', False);
				try
					src := a.ReadString('src');
					dst := a.ReadString('dst');
				finally
					a.Free;
				end;
				
				self.aliases[src] := dst; 
			end;
		finally
			aliases.Free;
		end;
	finally
		r.Free;
	end;
end;

procedure TTextureAliases.Load(const filename : String);
var
	r : TMemoryReader;
	k : TKonfig;
begin
	r := TMemoryReader.CreateFromFile(filename);
	k := TKonfig.Create;
	k.Load(r);
	Load(k);
	k.Free;
	r.Free;
end;

function TTextureAliases.GetRealName(var n : String) : Boolean;
var
	idx : Longint;
begin
	idx := aliases.IndexOf(n);
	if idx <> -1 then
	begin
		n := aliases.Data[idx];
		Result := True;
	end else
		Result := False;
end;

end.
