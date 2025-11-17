unit uWeather;

interface
uses classes;

procedure Initialize;
procedure Finalize;

procedure Update;
procedure SetWeather(const weather : String);

function GetWeathersList : TStringList;

procedure RenderSky;

implementation
uses sysutils, vmath, common, common_texture, GL, GLExt, Engine, Konfig, KonfigLibrary, framework;

var
	k_env : TTextKonfig;
	
	// current weather description
	current : TSection;
	
	t_sky      : TResTexture;
	c_sky      : TVec4;
	
	c_sun      : TVec4;
	d_sun      : TVec3;
	
	c_ambient  : TVec4;
	c_hemi     : TVec4;
	//c_fog      : TVec4;
	//fog_params : TVec4;

procedure Initialize;
var
	K : TKonfig;
	js : TFramework;
begin
	K := KonfigLibrary.GetKonfig('environments');
	if K <> nil then
	begin
		js := TFramework.Create;
		
		case Engine.version of
			eVer2033: k_env := js.DecompileKonfig(K, 'js\2033\environments.js');
			eVerLLBeta15102012,
			eVerLLBeta03122012,
			eVerLL:   k_env := js.DecompileKonfig(K, 'js\ll\environments.js');
			else      k_env := nil;
		end;
		
		js.Free;
		
		K.Free;
	end;
end;

procedure Finalize;
begin
	FreeAndNil(k_env);
end;

procedure Update;
var
	d_sun_v : TVec3;
begin
	d_sun_v := d_sun;
	Transform33(d_sun_v, modelview);
	
	// bind parameters to GL programs
	glProgramEnvParameter4fARB (GL_VERTEX_PROGRAM_ARB, 0, 
		c_sun.x * c_sun.w, c_sun.y * c_sun.w, c_sun.z * c_sun.w, 0);
	glProgramEnvParameter4fARB (GL_VERTEX_PROGRAM_ARB, 1, 
		d_sun_v.x, d_sun_v.y, d_sun_v.z, 0);
	glProgramEnvParameter4fARB (GL_VERTEX_PROGRAM_ARB, 2, 
		c_ambient.x * c_ambient.w, c_ambient.y * c_ambient.w, c_ambient.z * c_ambient.w, 0);
	glProgramEnvParameter4fARB (GL_VERTEX_PROGRAM_ARB, 3, 
		c_hemi.x * c_hemi.w, c_hemi.y * c_hemi.w, c_hemi.z * c_hemi.w, 0);
		
	glProgramEnvParameter4fARB (GL_FRAGMENT_PROGRAM_ARB, 0, 
		c_sun.x * c_sun.w, c_sun.y * c_sun.w, c_sun.z * c_sun.w, 0);
	glProgramEnvParameter4fARB (GL_FRAGMENT_PROGRAM_ARB, 1, 
		d_sun_v.x, d_sun_v.y, d_sun_v.z, 0);
	glProgramEnvParameter4fARB (GL_FRAGMENT_PROGRAM_ARB, 2, 
		c_ambient.x * c_ambient.w, c_ambient.y * c_ambient.w, c_ambient.z * c_ambient.w, 0);
	glProgramEnvParameter4fARB (GL_FRAGMENT_PROGRAM_ARB, 3, 
		c_hemi.x * c_hemi.w, c_hemi.y * c_hemi.w, c_hemi.z * c_hemi.w, 0);
end;

procedure SetWeather(const weather : String);
var
	sect_descs : TSection;
	sky_texture : String;
	
	v : TVec2;
	
	sin_phi, cos_phi : Single;
	sin_theta, cos_theta : Single;
	
	K : TKonfig;
	js : Tframework;
begin
	current := nil;
	if t_sky <> nil then FreeTexture(t_sky);
	t_sky := nil;
	
	if weather = '' then
		Exit;
	
	try
		if Engine.version = eVerRedux then
		begin
			K := TKonfig.Create;
			K.Load(ResourcesPath + '\environments\' + weather + '.bin');
			
			js := TFramework.Create;
			k_env := js.DecompileKonfig(K, 'js\redux\environments.js');
			js.Free;
			
			K.Free;
			
			current := k_env.root;
		end else
		begin
			sect_descs := k_env.root.GetSect('descriptions');
			current := sect_descs.GetSect(weather);
		end;
		
		// get sky params
		sky_texture := current.GetStr('sky_texture');
		t_sky := GetTexture(sky_texture);
		c_sky := current.GetVec4('sky_color', 'color, vec4f');
		
		// get sun params
		v := current.GetVec2('sun_dir');
		sin_phi := Sin(v.y * PI/180);
		cos_phi := Cos(v.y * PI/180);
		sin_theta := Sin(v.x * PI/180);
		cos_theta := Cos(v.x * PI/180);
		
		d_sun.x := -sin_phi * cos_theta;
		d_sun.y := sin_theta;
		d_sun.z := cos_phi * cos_theta;
		
		c_sun := current.GetVec4('sun_color', 'color, vec4f');
		
		// get hemi & ambient
		c_ambient := current.GetVec4('ambient', 'color, vec4f');
		c_hemi := current.GetVec4('hemi_color', 'color, vec4f');
	except
		on E: Exception do
			WriteLn('Cannot set weather (', E.ClassName, ': ', E.Message, ')');
	end;
end;

procedure _ListWeathers(list : TStringList; const dir : String);
var
	sr : TSearchRec;
	str : String;
begin
	if FindFirst(dir + '\*', faDirectory, sr) = 0 then
	begin
		repeat
			if not ((sr.Name = '.') or (sr.Name = '..')) then
				_ListWeathers(list, dir + '\' + sr.Name)
		until FindNext(sr) <> 0;
		FindClose(sr);
	end;

	if FindFirst(dir + '\*.bin', faAnyFile xor faDirectory, sr) = 0 then
	begin
		repeat
			// 1. full path
			str := dir + '\' + sr.Name;
			// 2. cut path to environments directory
			str := StringReplace(str, ResourcesPath+'\environments\', '', []);
			// 3. cut extension		
			str := StringReplace(str, ExtractFileExt(str), '', []);
			
			list.Add(str);
		until FindNext(sr) <> 0;
		FindClose(sr);
	end;
end;

function GetWeathersList : TStringList;
var
	sect_descs : TSection;
	I : Longint;
begin
	Result := TStringList.Create;
	
	if Engine.version = eVerRedux then
	begin
		_ListWeathers(Result, ResourcesPath + '\environments');
	end else
	begin
		sect_descs := k_env.root.GetSect('descriptions');
		for I := 0 to sect_descs.ParamCount-1 do
			if sect_descs.GetParam(I) is TSection then
				Result.Add(sect_descs.GetParam(I).name);
	end;
end;

procedure RenderSky;

	const
		SKY_RANGE = 50.0;
	
	procedure Vertex(x, y, z, s, t, r : Single);
	var
	  p : TVec3;
	 begin
	 	p.x := s; p.y := t; p.z := r;
	 	Normalize(p);
	 	glTexCoord3fv(@p);
	 	
	 	glVertex3f(x * SKY_RANGE, y * SKY_RANGE, z * SKY_RANGE);
	 end;
begin
	if current = nil then
		Exit;
		
	glEnable(GL_TEXTURE_CUBE_MAP);
	glBindTexture(GL_TEXTURE_CUBE_MAP, t_sky.texture);
	
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_NEAREST);  
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	
	glDisable(GL_DEPTH_TEST);
	
	glBegin(GL_QUADS);
	
	glColor3f(c_sky.x * c_sky.w, c_sky.y * c_sky.w, c_sky.z * c_sky.w);
	
	// POSITIVE X
	Vertex( 1.0,  1.0,  -1.0,  1,  1,  -1);
	Vertex( 1.0, -0.01, -1.0,  1, -1,  -1);
	Vertex( 1.0, -0.01,  1.0,  1, -1, 1);
	Vertex( 1.0,  1.0,   1.0,  1,  1, 1);
	
	// NEGATIVE X
	Vertex(-1.0,  1.0,  -1.0, -1,  1, -1);
	Vertex(-1.0,  1.0,   1.0, -1,  1,  1);
	Vertex(-1.0, -0.01,  1.0, -1, -1,  1);
	Vertex(-1.0, -0.01, -1.0, -1, -1, -1);
	
	// POSITIVE Y
	Vertex( 1.0,  1.0, -1.0,  1,  1, -1);
	Vertex( 1.0,  1.0,  1.0,  1,  1,  1);
	Vertex(-1.0,  1.0,  1.0, -1,  1,  1);
	Vertex(-1.0,  1.0, -1.0, -1,  1, -1);
	
	// NEGATIVE Y
	Vertex( 1.0, -0.01, -1.0,  1, -1, -1);
	Vertex(-1.0, -0.01, -1.0, -1, -1, -1);
	Vertex(-1.0, -0.01,  1.0, -1, -1,  1);
	Vertex( 1.0, -0.01,  1.0,  1, -1,  1);
	
	// POSITIVE Z
	Vertex(-1.0,  1.0,   1.0, -1,  1,  1);
	Vertex( 1.0,  1.0,   1.0,  1,  1,  1);
	Vertex( 1.0, -0.01,  1.0,  1, -1,  1);
	Vertex(-1.0, -0.01,  1.0, -1, -1,  1);
	
	// NEGATIVE Z
	Vertex( 1.0,  1.0,  -1.0,  1,  1, -1);
	Vertex(-1.0,  1.0,  -1.0, -1,  1, -1);
	Vertex(-1.0, -0.01, -1.0, -1, -1, -1);
	Vertex( 1.0, -0.01, -1.0,  1, -1, -1);
	
	glEnd;
	
	glEnable(GL_DEPTH_TEST);
	glDisable(GL_TEXTURE_CUBE_MAP);
	glColor3f(1,1,1);
end;

end.
