unit FileApi;

interface

function CopyFile(const existingfilename, newfilename : String; failifexists : Boolean) : Boolean;
function DeleteFile(const filename : String) : Boolean;
function MoveFile(const existingfilename, newfilename : String; replaceifexists : Boolean) : Boolean;

implementation
uses Windows;

function CopyFile(const existingfilename, newfilename : String; failifexists : Boolean) : Boolean;
begin
	Result := Windows.CopyFile(PAnsiChar(existingfilename), PAnsiChar(newfilename), failifexists);
end;

function DeleteFile(const filename : String) : Boolean;
begin
	Result := Windows.DeleteFile(PAnsiChar(filename));
end;

function MoveFile(const existingfilename, newfilename : String; replaceifexists : Boolean) : Boolean;
var
	flags : DWORD;
begin
	if replaceifexists then
		flags := MOVEFILE_REPLACE_EXISTING
	else
		flags := 0;
		
	Windows.MoveFileEx(PAnsiChar(existingfilename), PAnsiChar(newfilename), flags);
end;

end.
