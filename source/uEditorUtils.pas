unit uEditorUtils;

interface
uses vmath, Iup, classes, Konfig;

function MainDialog : Ihandle;

procedure Redisplay;
procedure ShowError(const message : String);

procedure WriteMatrix(const matrix : TMatrix);
procedure WriteVec3(const v : TVec3);

type TCondFunc = function(const s : String) : Boolean;
function GenerateName(src : String; cond : TCondFunc) : String;

function SelectColor(var clr : TVec4; alpha : Boolean = False) : Boolean;

type TStringArray = array of String;
function SplitString(const str : String; delim : Char = ','; do_trim : Boolean = True) : TStringArray;

function AddTreeNode(tree : Ihandle; const name : String; ref : Longint = 0) : longint;
procedure FillTreeFromList(tree : Ihandle; ref : Longint; sl : TStringList); // way faster than AddTreeNode
function PathOfTreeNode(tree : Ihandle; id : Longint) : String;
function KindOfTreeNode(tree : Ihandle; id : Longint) : String;

procedure RenumberArray(arr : TSection; const pref : String; digits : Longint);

implementation
uses sysutils;

function MainDialog : Ihandle;
begin
	Result := IupGetHandle('MAINDIALOG')
end;

procedure Redisplay;
var
	gl : Ihandle;
begin
	gl := IupGetDialogChild(MainDialog, 'GL_CANVAS');
	IupRedraw(gl, 0);
end;

procedure ShowError(const message : String);
begin
	IupMessageError(MainDialog, PAnsiChar(Message));
end;

procedure WriteMatrix(const matrix : TMatrix);
begin
	WriteLn(matrix[1,1], matrix[1,2], matrix[1,3], matrix[1,4]);		 
	WriteLn(matrix[2,1], matrix[2,2], matrix[2,3], matrix[2,4]);
	WriteLn(matrix[3,1], matrix[3,2], matrix[3,3], matrix[3,4]);
	WriteLn(matrix[4,1], matrix[4,2], matrix[4,3], matrix[4,4]);
end;

procedure WriteVec3(const v : TVec3);
begin
	WriteLn('x = ', v.x, ' y = ', v.y, ' z = ', v.z);
end;

function GenerateName(src : String; cond : TCondFunc) : String;
var
	I : Longint;
	tail : String;
	cut : Boolean;
	num, name : String;
	
	function IsDigit(c : Char) : Boolean;
	begin
		IsDigit := (c >= '0') and (c <= '9');
	end;
begin
	if cond(src) then
	begin
		GenerateName := src
	end else
	begin
		// cut "_NNNN" tail if exist
		if Length(src) > 5 then
		begin
			tail := Copy(src, Length(src)-4, 5);
			cut := (tail[1] = '_') and 
							IsDigit(tail[2]) and 
							IsDigit(tail[3]) and 
							IsDigit(tail[4]) and 
							IsDigit(tail[5]);
							
			if cut then
				src := Copy(src, 1, Length(src)-5);
		end;
				
		I := 0;
		repeat
			num := IntToStr(I);
			name := src + '_' + StringOfChar('0',4-Length(num)) + num;
			Inc(I);
		until cond(name);
		
		GenerateName := name;
	end;
end;

function SelectColor(var clr : TVec4; alpha : Boolean) : Boolean;
var
	str : String;
	I, E : Integer;
	dlg : Ihandle;
begin
	dlg := IupColorDlg;

	str := IntToStr(Trunc(clr.x * 255)) + ' ' + IntToStr(Trunc(clr.y * 255)) + ' ' + IntToStr(Trunc(clr.z * 255));
	IupSetStrAttribute(dlg, 'VALUE', PAnsiChar(str));

	if alpha then
	begin
		str := IntToStr(Trunc(clr.w * 255));
		IupSetStrAttribute(dlg, 'ALPHA', PAnsiChar(str));
		IupSetAttribute(dlg, 'SHOWALPHA', 'YES');
	end;

	IupPopup(dlg, IUP_CURRENT, IUP_CURRENT);

	if IupGetInt(dlg, 'STATUS') = 1 then
	begin
		str := IupGetAttribute(dlg, 'VALUE');
		I := 1; E := 1;
		while str[E] <> ' ' do Inc(E);
		clr.x := StrToInt(Copy(str, I, E-I)) / 255;
		I := E+1; E := E+1;
		while str[E] <> ' ' do Inc(E);
		clr.y := StrToInt(Copy(str, I, E-I)) / 255;
		I := E+1; E := E+1;
		while (E <= Length(str)) and (str[E] <> ' ') do Inc(E); 
		clr.z := StrToInt(Copy(str, I, E-I)) / 255;

		if alpha then
		begin
			clr.w := IupGetInt(dlg, 'ALPHA') / 255;
		end;

		Result := True;
	end else
		Result := False;

	IupDestroy(dlg);
end;

function SplitString(const str : String; delim : Char; do_trim : Boolean) : TStringArray;
var
	ret : TStringArray;
	S, E, L : Longint;
begin		
	SetLength(ret, 0);
	
	S := 1;
	E := 1;
	while E <= Length(str) do
	begin	
		if str[E] = delim then
		begin
			L := Length(ret);
			SetLength(ret, L+1);
			ret[L] := Copy(str, S, E-S);
			if do_trim then ret[L] := Trim(ret[L]);
			S := E+1;
		end;
		
		Inc(E);
	end;
	
	if E-S > 0 then
	begin
		L := Length(ret);
		SetLength(ret, L+1);
		ret[L] := Copy(str, S, E-S);
		if do_trim then ret[L] := Trim(ret[L]);
		S := E+1;
	end;
	
	SplitString := ret;
end;

function AddTreeNode(tree : Ihandle; const name : String; ref : Longint = 0) : longint;
var
	folder, rest : String;
	s : Longint;
	next : PAnsiChar;
begin
	s := Pos('\', name);
	if s <> 0 then
	begin
		folder := Copy(name, 1, s-1);
		rest := Copy(name, s+1);
		
		if IupGetIntId(tree, 'CHILDCOUNT', ref) > 0 then
		begin
			ref := ref+1;
			next := IupGetAttributeId(tree, 'FIRST', ref);
			
			while IupGetAttributeId(tree, 'TITLE', ref) <> folder do
			begin
				next := IupGetAttributeId(tree, 'NEXT', ref);
				if next <> nil then
					ref := StrToInt(next)
				else
					Break;
			end;
			
			if next <> nil then // нашли нужную папку
			begin
				AddTreeNode(tree, rest, ref);
			end else // нужно создать новую
			begin
				IupSetStrAttributeId(tree, 'INSERTBRANCH', ref, PAnsiChar(folder));
				Result := AddTreeNode(tree, rest, IupGetInt(tree, 'LASTADDNODE'));
			end;
		end else
		begin
			// нужно создать новую папку
			IupSetStrAttributeId(tree, 'ADDBRANCH', ref, PAnsiChar(folder));
			Result := AddTreeNode(tree, rest, IupGetInt(tree, 'LASTADDNODE'));
		end;
	end else
	begin
		// SLOW !!!
		if IupGetIntId(tree, 'CHILDCOUNT', ref) > 0 then
		begin
			ref := IupGetIntId(tree, 'LAST', ref+1);
			IupSetStrAttributeId(tree, 'INSERTLEAF', ref, PAnsiChar(name));
			Result := IupGetInt(tree, 'LASTADDNODE');
		end else
		begin
			IupSetStrAttributeId(tree, 'ADDLEAF', ref, PAnsiChar(name));
			Result := IupGetInt(tree, 'LASTADDNODE');
		end;
	end;
end;

type
	TTreeRec = record
		name : String;
		dirs : array of TTreeRec;
		files : array of String;
	end;

procedure AddFile(var tr : TTreeRec; const f : String);
var
	I, L, s : Longint;
	dir : String;
begin
	s := Pos('\', f);
	if s <> 0 then
	begin
		dir := Copy(f, 1, s-1);
		for I := 0 to Length(tr.dirs)-1 do
			if tr.dirs[I].name = dir then
			begin
				AddFile(tr.dirs[I], Copy(f, s+1));
				Exit;
			end;
			
		L := Length(tr.dirs);
		SetLength(tr.dirs, L+1);
		tr.dirs[L].name := dir;
		SetLength(tr.dirs[L].dirs, 0);
		SetLength(tr.dirs[L].files, 0);
		AddFile(tr.dirs[L], Copy(f, s+1));
	end else
	begin
		L := Length(tr.files);
		SetLength(tr.files, L+1);
		tr.files[L] := f;
	end;		
end;

procedure FillTree(tree : Ihandle; ref : Longint; const tr : TTreeRec);
var
	I : Longint;
begin
	for I := Length(tr.files)-1 downto 0 do
	begin
		IupSetStrAttributeId(tree, 'ADDLEAF', ref, PAnsiChar(tr.files[I]));
	end;
	
	for I := Length(tr.dirs)-1 downto 0 do
	begin
		IupSetStrAttributeId(tree, 'ADDBRANCH', ref, PAnsiChar(tr.dirs[I].name));
		FillTree(tree, IupGetInt(tree, 'LASTADDNODE'), tr.dirs[I]);
	end;
end;

procedure FillTreeFromList(tree : Ihandle; ref : Longint; sl : TStringList);
var
	I : Longint;
	tr : TTreeRec;
begin
	tr.name := '';
	SetLength(tr.dirs, 0);
	SetLength(tr.files, 0);
	
	for I := 0 to sl.Count-1 do
		AddFile(tr, sl[I]);
		
	FillTree(tree, ref, tr);
end;

function PathOfTreeNode(tree : Ihandle; id : Longint) : String;
var
	path : String;
begin
	path := iup.GetAttribute(tree, 'TITLE'+IntToStr(id));
	
	while iup.GetInt(tree, 'PARENT'+IntToStr(id)) <> 0 do
	begin
		id := iup.GetInt(tree, 'PARENT'+IntToStr(id));
		Insert('\', path, 1);
		Insert(iup.GetAttribute(tree, 'TITLE'+IntToStr(id)), path, 1);
	end;
	
	Result := path;
end;

function KindOfTreeNode(tree : Ihandle; id : Longint) : String;
begin
	Result := iup.GetAttribute(tree, 'KIND'+IntToStr(id));
end;

procedure RenumberArray(arr : TSection; const pref : String; digits : Longint);
var
	I, J : Longint;
	
	elem : TSection;
	count : TIntegerValue;
	
	n : String;
begin
	J := 0;
	
	for I := 0 to arr.items.Count-1 do
	begin
		if TSimpleValue(arr.items[I]) is TSection then
		begin
			elem := TSection(arr.items[I]);
			
			n := IntToStr(J);
			elem.name := pref + StringOfChar('0', digits-Length(n)) + n;
			
			Inc(J);
		end;
	end;
	
	count := arr.GetParam('count', 'u32') as TIntegerValue;
	count.num := J;
end;

end.
