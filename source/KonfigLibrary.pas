unit KonfigLibrary;

interface
uses Konfig;

procedure Load(const path : String);
function GetKonfig(const name : String) : TKonfig;

implementation
uses sysutils, chunkedFile, uCrc;

var
	lib : TMemoryReader;

procedure Load(const path : String);
begin
	if FileExists(path) then
		lib := TMemoryReader.CreateFromFile(path)
end;

function GetKonfig(const name : String) : TKonfig;
var
	full_name : String;
	r : TMemoryReader;
	k : TKonfig;
begin
	if lib <> nil then
	begin
		full_name := 'content\scripts\' + name + '.bin';
		r := lib.OpenChunk(GetStringCrc(full_name));
		if r <> nil then
		begin
			k := TKonfig.Create;
			if k.Load(r) then
				Result := k
			else
				FreeAndNil(k);
				
			r.Free;
		end else
			Result := nil;
	end else
		Result := nil;
end;

finalization
	lib.Free;
end.
