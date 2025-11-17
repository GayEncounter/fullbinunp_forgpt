unit common_texture;

interface
uses common_resource, GL, texturePrefs;

type
	TResTexture = class(TResource)
		texture : GLuint;
		params : PTextureParams;
		params2 : PTextureParams2;

		constructor Create(const nm : String; texture : GLuint);
		destructor Destroy; override;
	end;
	
function GetTexture(const name : String) : TResTexture;
procedure FreeTexture(r : TResTexture);
procedure ClearUnusedTextures;

implementation
uses sysutils, classes, fgl, Texture, Engine;

type
	TResTextureMap  = TFPGMap<String,TResTexture>;
var
	textures		: TResTextureMap;

constructor TResTexture.Create(const nm : String; texture : GLuint);
begin
	inherited Create(nm);
	self.texture := texture;

	if texture_params <> nil then
		params := texture_params[nm];
	if texture_params2 <> nil then
		params2 := texture_params2[nm];
end;

destructor TResTexture.Destroy;
begin
	glDeleteTextures(1, @texture);
	inherited Destroy;
end;

function GetTexture(const name : String) : TResTexture;
var
	idx : Longint;
	r : TResTexture;

	file_name : String;
begin
	if not textures.Find(name, idx) then
	begin
		file_name := GetRealTextureName(name);

		r := TResTexture.Create(name, LoadTexture(ResourcesPath + '\textures\' + file_name));
		textures[name] := r;
	end else
	begin
		r := textures.Data[idx];
		Inc(r.refcnt);
	end;

	Result := r;
end;

procedure FreeTexture(r : TResTexture);
begin
	if r <> nil then
	begin
		Dec(r.refcnt);
		
		if r.refcnt < 0 then
		  raise Exception.Create('FreeTexture: dangling pointer to [' + r.name + ']');
	end;
end;

procedure ClearUnusedTextures;
var 
	I : Longint;
	list : TList;
begin
	list := TList.Create;
  
	for I := 0 to textures.Count-1 do
		if textures.Data[I].refcnt < 1 then
			list.Add(textures.Data[I]);
      
	for I := 0 to list.Count-1 do
	begin
		textures.Remove(TResTexture(list[i]).name);
		TResTexture(list[I]).Free;
	end;
    
	list.Free;  
end;

initialization
	textures		:= TResTextureMap.Create;
	textures.Sorted := True;
end.
