{$POINTERMATH ON}
unit aiTypes;

interface

//added for Delphi interface
type
   PCardinalArray = ^Cardinal;
   PSingleArray = ^Single; 

type aiString = record
//{$IFDEF WIN64}
//   length: QWord; // x64
//{$ELSE}
   length: Cardinal;
//{$ENDIF}
   data: array [0..1023] of char;
end;
type PaiString = ^aiString;

type aiReturn = (
	aiReturn_SUCCESS = $0,
	aiReturn_FAILURE = -$1,
	aiReturn_OUTOFMEMORY = -$3,
	_AI_ENFORCE_ENUM_SIZE = $7fffffff
);

const AI_SUCCESS = aiReturn_SUCCESS;
const AI_FAILURE = aiReturn_FAILURE;
const AI_OUTOFMEMORY = aiReturn_OUTOFMEMORY;




function aiStringToDelphiString(const a: aiString): AnsiString;
procedure aiStringFromDelphiString(var a: aiString; const str: AnsiString);


implementation

function aiStringToDelphiString(const a: aiString): AnsiString;
var
   i: integer;
begin
   result := '';
   if a.length > 0 then
   begin
      SetLength( result, a.length);
      for i := 1 to a.length do
      begin
         result[i] := a.data[i-1];
      end;
   end;
end;


procedure aiStringFromDelphiString(var a: aiString; const str: AnsiString);
begin
	a.length := Length(str);
	Move(str[1], a.data[0], a.length+1);
end;

end.
