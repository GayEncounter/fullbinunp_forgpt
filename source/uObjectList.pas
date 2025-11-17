unit uObjectList;

interface

procedure Create;
procedure Show;

implementation
uses SysUtils, Iup, uScene, uEntity;

var
	tree_objects : Ihandle;
	dlg : Ihandle;

procedure Create;
begin
	tree_objects := IupTree;
	
	dlg := IupDialog(tree_objects);
	IupSetAttribute(dlg, 'TITLE', 'Object List');
end;

function AllChildsFor(id : Word) : TEntityArray;
var
	I : Longint;
	E : TEntity;
	C : TEntityArray;
begin
	SetLength(C, 0);
	for I := 0 to Scene.entities.Count - 1 do
	begin
		E := TEntity(Scene.entities[I]);
		if E.ParentID = id then
		begin
			SetLength(C, Length(C)+1);
			C[Length(C)-1] := E;
		end;
	end;
	Result := C;
end;

procedure AddEntity(tree : Ihandle; ref : Longint; E : TEntity);
var
	I, J : Longint;
	C : TEntityArray;
begin
	C := AllChildsFor(E.ID);
	if Length(C) > 0 then
	begin
		iup.SetStrAttribute(tree, 'ADDBRANCH'+IntToStr(ref), E.Name);
		J := iup.GetInt(tree, 'LASTADDNODE');
		for I := Length(C) - 1 downto 0 do
			AddEntity(tree, J, C[I]);
	end else
	begin
		iup.SetStrAttribute(tree_objects, 'ADDLEAF'+IntToStr(ref), E.Name);
	end;
end;

procedure Show;
var
	I : Longint;
	E : TEntity;
begin
	IupSetAttribute(tree_objects, 'DELNODE', 'ALL');

	if Assigned(Scene.entities) then
	begin
		for I := Scene.entities.Count - 1 downto 0 do
		begin
			E := TEntity(Scene.entities[I]);	
			if E.ParentID = 65535 then
				AddEntity(tree_objects, -1, E);
		end;
	end;
	
	IupShow(dlg);
end;

end.
