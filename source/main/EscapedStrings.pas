unit EscapedStrings;

interface
uses chunkedFile;

procedure UnpackEscapeSequences(var str : String);
procedure PrintEscapedString(w : TMemoryWriter; const str : String; qch : Char);

implementation

procedure UnpackEscapeSequences(var str : String);
var
	I, J : Longint;
	s : String;
begin
	SetLength(s, Length(str));
	I := 1;
	J := 1;
	while I <= Length(str) do
	begin
		if str[I] = '\' then
		begin
			Inc(I);
			if I <= Length(str) then
			begin
				case str[I] of
					'a': s[J] := #7;
					'b': s[J] := #8;
					'f': s[J] := #12;
					'n': s[J] := #10;
					'r': s[J] := #13;
					't': s[J] := #9;
					'v': s[J] := #11;
					else s[J] := str[I];
				end;	
				Inc(I);
				Inc(J);
			end;
		end else
		begin
			s[J] := str[I];
			Inc(I);
			Inc(J);
		end;
	end;
	SetLength(s, J-1);
	str := s;
end;

procedure PrintEscapedString(w : TMemoryWriter; const str : String; qch : Char);
var
	I : Longint;
begin
	for I := 1 to Length(str) do
	begin
		if str[I] in [#7, #8, #12, #10, #13, #9, #11, '\'] then
		begin
			w.WriteString('\');
			case str[I] of
				#7 : w.WriteString('a');
				#8 : w.WriteString('b');
				#12: w.WriteString('f');
				#10: w.WriteString('n');	
				#13: w.WriteString('r');			
				#9 : w.WriteString('t');
				#11: w.WriteString('v');
				'\': w.WriteString('\');
			end;
		end else
		if str[I] = qch then
		begin
			w.WriteString('\');
			w.WriteString(qch);
		end else
			w.WriteString(str[I]);
	end;
end;

end.