unit navmap_server;

interface
uses vmath;

procedure Run(lastlight : Boolean);
procedure Stop;

function LoadNavmapRaw(buffer : Pointer; buffer_length : Longint) : Boolean;

procedure GetCellAndConnRegForCover(x, y, z : Single; shape_id : Longint; out cell : Longint; out conn_reg : Longint);
procedure PositionFor3DPoint(x, y, z : Single; out _x, _y, _cell : Longint);
procedure PositionNear3DPoint(x, y, z : Single; horz_tolerance, vert_tolerance : Longint; out _x, _y, _cell : Longint);
function Get3dPos(x, y, cell : Longint; out _result : TVec3) : Boolean;

implementation
uses sysutils, strutils, Windows;

type
	TMsgParser = record
		msg : String;
		pos : Longint;
		
		procedure SetMsg(const msg : String);
		function NextWord : String;
	end;
	
procedure TMsgParser.SetMsg(const msg : String);
begin
	self.msg := msg;
	self.pos := 1;
end;

function TMsgParser.NextWord : String;
const
	delims : set of Char = [' ', #9];
var
	wstart : Longint;
	wlen : Longint;
begin
	while (pos <= Length(msg)) and (msg[pos] in delims) do
		Inc(pos);
		
	wstart := pos;
	wlen := 0;
	
	while (pos <= Length(msg)) and not (msg[pos] in delims) do
	begin
		Inc(pos);
		Inc(wlen);
	end;
	
	NextWord := Copy(msg, wstart, wlen);
end;

var
	// pipes for I/O redirection
	hStdinRead   : HANDLE;
	hStdinWrite  : HANDLE;
	hStdoutRead  : HANDLE;
	hStdoutWrite : HANDLE;
	
	// server process
	hProcess     : HANDLE;
	
procedure RetrieveResponseOrMessage(out response : String; var bResponseRecieved : Boolean);
var
	str : String[255];
	nread : DWORD;
	sym : Char;
begin
	ReadFile(hStdoutRead, sym, 1, nread, nil);

	if sym = '!' then
	begin
		// message
		ReadFile(hStdoutRead, sym, 1, nread, nil);
		while (sym = ' ') or (sym = #9) do
			ReadFile(hStdoutRead, sym, 1, nread, nil);
			
		Write('[NAVMAP_SERVER MSG] ');
		Write(sym);

		while sym <> #10 do
		begin
			ReadFile(hStdoutRead, sym, 1, nread, nil);
			Write(sym);
		end;
	end else
	begin
		// response
		SetLength(str, 0);
		
		while sym <> #10 do
		begin
			SetLength(str, Length(str)+1);
			str[Length(str)] := sym;
			ReadFile(hStdoutRead, sym, 1, nread, nil);
		end;
		
		response := str;
		bResponseRecieved := True;
	end;
end;
	
procedure RetrieveResponseAndMessages(out response : String);
var
	bResponseRecieved : Boolean;
begin
	bResponseRecieved := False;
	
	while not bResponseRecieved do
	begin
		RetrieveResponseOrMessage(response, bResponseRecieved);
	end;
	
	// echo response
	WriteLn('[NAVMAP_SERVER RESPONSE] ', response);
	
end;

procedure DoACommand(const cmd : String; out response : String);
var
	nwritten : DWORD;
	lf : Char;
begin
	lf := #10;
	
	// send command to the server
	WriteFile(hStdinWrite, cmd[1], Length(cmd), nwritten, nil);
	WriteFile(hStdinWrite, lf, 1, nwritten, nil);
	
	// echo command
	WriteLn('[NAVMAP_SERVER CMD] ', cmd);
	
	// retrieve response and messages
	RetrieveResponseAndMessages(response);
end;

procedure Run(lastlight : Boolean);
var
	ret : BOOL;
	sa : SECURITY_ATTRIBUTES;
	si : STARTUPINFO;
	pi : PROCESS_INFORMATION;
	
	response : String;
begin
	// create pipes first
	sa.nLength              := Sizeof(sa);
	sa.lpSecurityDescriptor := nil;
	sa.bInheritHandle       := TRUE;
	
	ret := CreatePipe(
		hStdinRead,
		hStdinWrite,
		@sa,
		4096
	);
	
	ret := CreatePipe(
		hStdoutRead,
		hStdoutWrite,
		@sa,
		4096
	);
	
	FillChar(si, Sizeof(si), #0);
	si.cb         := Sizeof(si);
	si.dwFlags    := STARTF_USESTDHANDLES;
	si.hStdInput  := hStdinRead;
	si.hStdOutput := hStdoutWrite;
	si.hStdError  := INVALID_HANDLE_VALUE; // or should it be zero ? I don't get it
	
	ret := CreateProcess(
		'navmap_server.exe', // program name
		nil,                 // command line
		nil,                 // process security attributes
		nil,                 // thread security attributes
		TRUE,                // handle inheritance
		CREATE_NO_WINDOW,    // creation flags
		nil,                 // environment vars
		nil,                 // current directory,
		@si,
		@pi
	);
	
	if not ret then
	begin
		WriteLn('! Can''t run navmap_server (', GetLastError, ')');
		Exit
	end;
	
	// remember handles we needed
	hProcess := pi.hProcess;	
	// we don't need this
	CloseHandle(pi.hThread);
	
	// check for initialization
	RetrieveResponseAndMessages(response);
	if response <> 'PE_INITIALIZED' then
		Writeln('response <> PE_INITIALIZED ', Length(response), ' ', Byte(response[Length(response)]));
	
	// send command
	DoACommand('CREATE_SHAPES_2033', response);
	WriteLn('response = ', response);
end;

procedure Stop;
var
	response : String;
begin
	if hProcess <> 0 then
	begin
		DoACommand('STOP', response);
		
		if WaitForSingleObject(hProcess, 3000) <> 0 then
		begin
			WriteLn('Error stopping navmap server');
			Exit;
		end;
		
		CloseHandle(hProcess);
		CloseHandle(hStdinRead);
		CloseHandle(hStdinWrite);
		CloseHandle(hStdoutRead);
		CloseHandle(hStdoutWrite);
		
		hProcess     := 0;
		hStdinRead   := 0;
		hStdinWrite  := 0;
		hStdoutRead  := 0;
		hStdoutWrite := 0;
	end else
		WriteLn('! navmap_server.Stop: server is not running');
end;

function LoadNavmapRaw(buffer : Pointer; buffer_length : Longint) : Boolean;
var
	response : String;
	parm1 : String[64];
	parm2 : String[64];
	parser : TMsgParser;
	
	ptr : Longword;
	nwritten : SIZE_T;
begin
	if hProcess = 0 then
	begin
		WriteLn('! LoadNavmapRaw: server is not running');
		Exit;
	end;
	
	DoACommand('MESH_DATA raw ' + IntToStr(buffer_length), response);
	
	parser.SetMsg(response);
	
	if parser.NextWord = 'BUFFER_READY' then
	begin
		// parse response		
		parm1 := parser.NextWord;
		parm2 := parser.NextWord;
		
		// convert to binary
		ptr := StrToInt('$'+parm1);
		// parm2 is size (unused)
		
		// write
		WriteProcessMemory(hProcess, Pointer(ptr), buffer, buffer_length, nwritten);
		
		// inform server that data is ready
		DoACommand('MESH_DATA_READY', response);
		
		if response = 'MESH_READY' then
			LoadNavmapRaw := True
		else
			LoadNavmapRaw := False;
	end else
		LoadNavmapRaw := False;
end;

procedure GetCellAndConnRegForCover(x, y, z : Single; shape_id : Longint; out cell : Longint; out conn_reg : Longint);
var
	fs : TFormatSettings;
	
	cmd : String;
	response : String;
	parser : TMsgParser;
	
	parm1 : String[64];
	parm2 : String[64];
	parm3 : String[64];
	parm4 : String[64];
begin
	if hProcess = 0 then
	begin
		WriteLn('! GetCellAndConnRegForCover: server is not running');
		Exit;
	end;

	fs := DefaultFormatSettings;
	fs.DecimalSeparator := '.';
	
	cmd := 'COVER ' + 
		IntToStr(shape_id) + ' ' + 
		FloatToStr(x, fs) + ' ' + 
		FloatToStr(y, fs) + ' ' + 
		FloatToStr(z, fs);
	
	DoACommand(cmd, response);
	
	parser.SetMsg(response);
	
	if parser.NextWord = 'OK' then
	begin
		parm1 := parser.NextWord; // X coord, unused
		parm2 := parser.NextWord; // Y coord, unused
		parm3 := parser.NextWord; // ground cell
		parm4 := parser.NextWord; // connected region
		
		cell := StrToInt(parm3);
		conn_reg := StrToInt(parm4);
	end else
	begin
		cell := -1;
		conn_reg := -1;
	end;
end;

procedure PositionFor3DPoint(x, y, z : Single; out _x, _y, _cell : Longint);
var
	fs : TFormatSettings;
	
	cmd : String;
	response : String;
	parser : TMsgParser;
	
	parm1 : String[64];
	parm2 : String[64];
	parm3 : String[64];
begin
	if hProcess = 0 then
	begin
		WriteLn('! PositionFor3DPoint: server is not running');
		Exit;
	end;
	
	fs := DefaultFormatSettings;
	fs.DecimalSeparator := '.';
	
	cmd := 'POS_FOR_3DP ' + 
		FloatToStr(x, fs) + ' ' + 
		FloatToStr(y, fs) + ' ' + 
		FloatToStr(z, fs);
	
	DoACommand(cmd, response);
	parser.SetMsg(response);
	
	if parser.NextWord = 'OK' then
	begin
		parm1 := parser.NextWord; // X coord
		parm2 := parser.NextWord; // Y coord
		parm3 := parser.NextWord; // ground cell
		
		_x := StrToInt(parm1);
		_y := StrToInt(parm2);
		_cell := StrToInt(parm3);
	end else
	begin
		_x := 0;
		_y := 0;
		_cell := -1;
	end;
end;

procedure PositionNear3DPoint(x, y, z : Single; horz_tolerance, vert_tolerance : Longint; out _x, _y, _cell : Longint);
var
	fs : TFormatSettings;
	
	cmd : String;
	response : String;
	parser : TMsgParser;
	
	parm1 : String[64];
	parm2 : String[64];
	parm3 : String[64];
begin
	if hProcess = 0 then
	begin
		WriteLn('! PositionNear3DPoint: server is not running');
		Exit;
	end;
	
	fs := DefaultFormatSettings;
	fs.DecimalSeparator := '.';
	
	cmd := 'POS_NEAR_3DP ' + 
		FloatToStr(x, fs) + ' ' + 
		FloatToStr(y, fs) + ' ' + 
		FloatToStr(z, fs) + ' ' +
		IntToStr(horz_tolerance) + ' ' +
		IntToStr(vert_tolerance);
	
	DoACommand(cmd, response);
	parser.SetMsg(response);
	
	if parser.NextWord = 'OK' then
	begin
		parm1 := parser.NextWord; // X coord
		parm2 := parser.NextWord; // Y coord
		parm3 := parser.NextWord; // ground cell
		
		_x := StrToInt(parm1);
		_y := StrToInt(parm2);
		_cell := StrToInt(parm3);
	end else
	begin
		_x := 0;
		_y := 0;
		_cell := -1;
	end;
end;

function Get3dPos(x, y, cell : Longint; out _result : TVec3) : Boolean;
var
	fs : TFormatSettings;
	
	cmd : String;
	response : String;
	parser : TMsgParser;
	
	parm1 : String[64];
	parm2 : String[64];
	parm3 : String[64];
begin
	if hProcess = 0 then
	begin
		WriteLn('! Get3dPos: server is not running');
		Get3DPos := False;
		Exit;
	end;
	
	fs := DefaultFormatSettings;
	fs.DecimalSeparator := '.';
	
	cmd := '3D_POS ' + 
		IntToStr(x) + ' ' + 
		IntToStr(y) + ' ' + 
		IntToStr(cell);
	
	DoACommand(cmd, response);
	parser.SetMsg(response);
	
	if parser.NextWord = 'OK' then
	begin
		parm1 := parser.NextWord; // X
		parm2 := parser.NextWord; // Y
		parm3 := parser.NextWord; // Z
		
		_result.x := StrToFloat(parm1, fs);
		_result.y := StrToFloat(parm2, fs);
		_result.z := StrToFloat(parm3, fs);
		
		Get3DPos := True;
	end else
	begin
		Get3DPos := False;
	end;
end;


end.