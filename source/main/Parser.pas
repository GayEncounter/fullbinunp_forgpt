unit Parser;

interface

type
	TTokenType = (tokDelimiter, tokString, tokIdentifier);
	PTokenType = ^TTokenType;

type
	TDelimiterSet = set of AnsiChar;

type
	TParser = class
		source : String;
		pos : Longint;
		delims : TDelimiterSet;
		allow_escapes : Boolean;

		constructor Create(const src : String; delimiters : TDelimiterSet; escapes : Boolean);

		procedure Error(const msg : String; p : Longint = 0);
		procedure StripComments;
		procedure StripCommentsLUA;
		procedure StripCommentsCPlusPlus;
		function NextToken(var token : String; allowEOF : Boolean = False; ttype : PTokenType = nil) : Boolean;
	end;

implementation
uses sysutils, EscapedStrings;

constructor TParser.Create(const src: string; delimiters : TDelimiterSet; escapes : Boolean);
begin
	inherited Create;
	source := src;
	pos := 1;
	delims := delimiters;
	allow_escapes := escapes;
end;

procedure TParser.Error(const msg : String; p : Longint);
var
	I : Longint;
	line, ch : Integer;
begin
	I := 1;
	line := 1;
	ch := 1;

	if p = 0 then
		p := pos;

	while I < p do
	begin
		if source[I] = #10 then
		begin
			Inc(line);
			ch := 1;
		end else
			Inc(ch);

		Inc(I);
	end;

	raise Exception.Create(IntToStr(line) + ':' + IntToStr(ch) + ': ' + msg);
end;

procedure TParser.StripComments;
var
	I, start : Longint;
	qch : AnsiChar;
begin
	I := 1;
	while I <= Length(source) do
	begin
		if (source[I] = '''') or (source[I] = '"') then
		begin
			qch := source[I];
			Inc(I);
			while (I <= Length(source)) and (source[I] <> qch) do
			begin
				if allow_escapes and (source[I] = '\') then
					Inc(I); // skip escape sequence
				Inc(I);
			end;
			Inc(I);
		end else 
		if source[I] = '{' then
		begin
			start := I;
			
			source[I] := ' ';
			Inc(I);
			
			while (I <= Length(source)) and (source[I] <> '}') do
			begin
				source[I] := ' ';
				Inc(I);
			end;
			
			if I > Length(source) then
				Error('unclosed comment', start);
				
			source[I] := ' ';
			Inc(I);
		end else
			Inc(I);
	end;
end;

procedure TParser.StripCommentsLUA;
var
	I, start : Longint;
		qch : AnsiChar;
begin
	I := 1;
	while I <= Length(source) do
	begin
		if (source[I] = '''') or (source[I] = '"') then
		begin
			qch := source[I];
			Inc(I);
			while (I <= Length(source)) and (source[I] <> qch) do
			begin
				if allow_escapes and (source[I] = '\') then
					Inc(I); // skip escape sequence
				Inc(I);
			end;
			Inc(I);
		end else
		if ((Length(source) - (I-1)) >= 2) and (source[I] = '-') and (source[I+1] = '-') then
		begin
			start := I;
			
			source[I] := ' ';
			source[I+1] := ' ';
			Inc(I, 2);
			
			if ((Length(source) - (I-1)) >= 2) and (source[I] = '[') and (source[I+1] = '[') then
			begin
				source[I] := ' ';
				source[I+1] := ' ';
				Inc(I, 2);
				
				while ((Length(source) - (I-1)) >= 2) and not ((source[I] = ']') and (source[I+1] = ']')) do
				begin
					source[I] := ' ';
					Inc(I);
				end;
					
				if ((Length(source) - (I-1)) < 2) then
					Error('unclosed comment', start);
					
				source[I] := ' ';
				source[I+1] := ' ';
				Inc(I, 2);
			end else
			begin
				while (I <= Length(source)) and (source[I] <> #10) do
				begin
					source[I] := ' ';
					Inc(I);
				end;
			end;
		end else
			Inc(I);
	end;
end;

procedure TParser.StripCommentsCPlusPlus;
var
	I, start : Longint;
		qch : AnsiChar;
begin
	I := 1;
	while I <= Length(source) do
	begin
		if (source[I] = '''') or (source[I] = '"') then
		begin
			qch := source[I];
			Inc(I);
			while (I <= Length(source)) and (source[I] <> qch) do
			begin
				if allow_escapes and (source[I] = '\') then
					Inc(I); // skip escape sequence
				Inc(I);
			end;
			Inc(I);
		end else
		if ((Length(source) - (I-1)) >= 2) and (source[I] = '/') and (source[I+1] = '/') then
		begin
			source[I] := ' ';
			source[I+1] := ' ';
			Inc(I, 2);
			
			while (I <= Length(source)) and (source[I] <> #10) do
			begin
				source[I] := ' ';
				Inc(I);
			end;			
		end else	
		if ((Length(source) - (I-1)) >= 2) and (source[I] = '/') and (source[I+1] = '*') then
		begin
			start := I;
			
			source[I] := ' ';
			source[I+1] := ' ';
			Inc(I, 2);
				
			while ((Length(source) - (I-1)) >= 2) and not ((source[I] = '*') and (source[I+1] = '/')) do
			begin
				source[I] := ' ';
				Inc(I);
			end;
					
			if ((Length(source) - (I-1)) < 2) then
				Error('unclosed comment', start);
					
			source[I] := ' ';
			source[I+1] := ' ';
			Inc(I, 2);
		end else
			Inc(I);
	end;
end;

function TParser.NextToken(var token : String; allowEOF : Boolean; ttype : PTokenType = nil) : Boolean;
	function IsDelim : Boolean;
	begin
		IsDelim := source[pos] in delims;
	end;
	
	function IsSpace : Boolean;
	begin
		IsSpace := source[pos] in [' ', #10, #13, #9];
	end;
var
	start : Longint;
begin
	while (pos <= Length(source)) and IsSpace do
		Inc(pos);

	if pos <= Length(source) then
	begin
		if IsDelim then
		begin
			token := source[pos];
			Inc(pos);
			
			if ttype <> nil then
				ttype^ := tokDelimiter;
		end else
		if (source[pos] = '''') or (source[pos] = '"') then
		begin
			start := pos;
			Inc(pos);
			while (source[pos] <> source[start]) and (pos <= Length(source)) do
			begin
				if allow_escapes and (source[pos] = '\') then
				begin
					if (Length(source) - pos) < 1 then
						Error('unclosed string', start);
					Inc(pos);
				end;		
				Inc(pos);
			end;
			
			if pos > Length(source) then
				Error('unclosed string', start);

			token := Copy(source, start+1, pos-(start+1));
			if allow_escapes then UnpackEscapeSequences(token);
			Inc(pos); // skip '
			
			if ttype <> nil then
				ttype^ := tokString;
		end else
		begin
			start := pos;
			while (not IsDelim) and (not IsSpace) and (pos <= Length(source)) do
				Inc(pos);

			token := Copy(source, start, pos-start);
			if ttype <> nil then
				ttype^ := tokIdentifier;
		end;

		Result := True;
	end else
	begin
		Result := False;
		if not allowEOF then
			Error('unexpected end of file');
	end;
end;
	
end.