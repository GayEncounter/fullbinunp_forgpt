unit common_resource;

interface

type
	TResource = class
		name : String;
		refcnt : Integer;
		constructor Create(const nm : String);
	end;

implementation

constructor TResource.Create(const nm : String);
begin
	inherited Create;
	name := nm;
	refcnt := 1;
end;

end.
