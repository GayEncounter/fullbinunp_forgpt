unit uTemplates;

interface
uses classes, Konfig, Iup, vmath, uScene, uEntity;

procedure LoadTemplates(const fn : String);
procedure SaveTemplates;
procedure UnloadTemplates;

// fill IupTree with templates from konfig
procedure UpdateTemplates(ih : Ihandle);

procedure NewTemplate(const path : String; entities : TList; pivot : TEntity);
procedure DeleteTemplate(t : TSection);

function  IsFolder(t : TSection) : Boolean;

// w/ transform
function CreateEntities(template : TSection; const transform : TMatrix) : TEntityArray; overload;
// no transform
function CreateEntities(template : TSection) : TEntityArray; overload;

implementation
uses sysutils, uEditorUtils;

var
	templates : TTextKonfig;
	templates_fn : String;
	
procedure LoadTemplates(const fn : String);
begin
	UnloadTemplates;
	templates_fn := fn;
	
	templates := TTextKonfig.Create;
	templates.LoadFromFile(templates_fn);
end;

procedure SaveTemplates;
begin
	try
		if Assigned(templates) then
			templates.SaveToFile(templates_fn);
	except
		on E: Exception do
			ShowError('Saving templates failed!'#10 + E.ClassName + ': ' + E.Message);
	end;
end;

procedure UnloadTemplates;
begin
	FreeAndNil(templates);
end;

function _TemplateSortCb(item1, item2 : Pointer) : Integer;
var
	t1, t2 : TSimpleValue;
begin
	t1 := TSimpleValue(item1);
	t2 := TSimpleValue(item2);
	
	Result := AnsiCompareStr(t1.name, t2.name);
end;

procedure _SortTemplates(t : TSection);
var
	I : Longint;
	s : TSection;
begin
	for I := 0 to t.items.Count - 1 do
		if TSimpleValue(t.items[I]) is TSection then
		begin
			s := TSection(t.items[I]);
			if IsFolder(s) then
				_SortTemplates(s);
		end;
			
	t.items.Sort(_TemplateSortCb);
end;

procedure _AddSection(tree : Ihandle; ref : Longint; sect : TSection);
var
	I : Integer;
	v : TSimpleValue;

	nid : Longint;
begin
	for I := sect.items.Count - 1 downto 0 do
	begin
		v := TSimpleValue(sect.items[I]);

		if (v is TSection) and IsFolder(TSection(v)) then
		begin		
			IupSetAttributeId(tree, 'ADDBRANCH', ref, PAnsiChar(v.name));
			nid := IupGetInt(tree, 'LASTADDNODE');
			IupSetAttributeId(tree, 'USERDATA', nid, Pointer(v));
			
			_AddSection(tree, nid, v as TSection);
		end else
		begin
			IupSetStrAttributeId(tree, 'ADDLEAF', ref, PAnsiChar(v.name));
			nid := IupGetInt(tree, 'LASTADDNODE');
			IupSetAttributeId(tree, 'USERDATA', nid, Pointer(v));
			
			if (v is TSection) and TSection(v).GetBool('is_group', False) then
				IupSetAttributeId(tree, 'IMAGE', nid, 'ICON_GROUP')
			else
				IupSetAttributeId(tree, 'IMAGE', nid, 'ICON_OBJECT')
		end;
	end;
end;

procedure UpdateTemplates(ih : Ihandle);
begin
	IupSetAttribute(ih, 'DELNODE', 'ALL');
	
	IupSetAttribute(ih, 'IMAGEBRANCHCOLLAPSED', 'ICON_FOLDER1');
	IupSetAttribute(ih, 'IMAGEBRANCHEXPANDED', 'ICON_FOLDER2');
	
	if Assigned(templates) then
	begin
		_SortTemplates(templates.root);
		_AddSection(ih, -1, templates.root);
	end;
end;

function _GetCenter(list : TList) : TVec3;
var
	c : TVec3;
	bb : TAABB;
	I : Longint;
begin
	if list.Count > 0 then
	begin
		bb := TEntity(list[0]).bbox;
	
		for I := 1 to list.Count - 1 do
			AABBMerge(bb, TEntity(list[I]).bbox);
		
		AABBCenter(c, bb);
	end else
	begin
		c.x := 0;
		c.y := 0;
		c.z := 0;
	end;
		
	_GetCenter := c;
end;

function _CreateTemplate(entities : TList; const pivot : TEntity) : TSection;
var
	I : Integer;
	s, d : TSection;
	
	c : TVec3;
	m, mm : TMatrix;
	inv_pivot : TMatrix;
	
	param_matrix : TFloatArrayValue;
begin
	if entities.Count > 1 then
	begin
		if pivot <> nil then
		begin
			Invert43(inv_pivot, pivot.Matrix);
		end else
		begin
			c := _GetCenter(entities);
			Translate(inv_pivot, -c.x, -c.y, -c.z);
		end;
		
		s := TSection.Create;
		s.AddBool('is_group', True);
		
		for I := 0 to entities.Count - 1 do
		begin
			d := TEntity(entities[I]).data.Copy as TSection;
			
			param_matrix := d.GetParam('', 'pose, matrix') as TFloatArrayValue;
			
			if Length(param_matrix.data) = 16 then
				param_matrix.GetMatrix44(mm)
			else
				param_matrix.GetMatrix43(mm);
				
			m := inv_pivot;
			Mul44(m, mm);
			
			if Length(param_matrix.data) = 16 then
				param_matrix.SetMatrix44(m)
			else
				param_matrix.SetMatrix43(m);
			
			s.items.Add(d);
		end;
	end else
	begin
		s := TEntity(entities[0]).data.Copy as TSection;
		param_matrix := s.GetParam('', 'pose, matrix') as TFloatArrayValue;
			
		Identity(mm);
		if Length(param_matrix.data) = 16 then
			param_matrix.SetMatrix44(mm)
		else
			param_matrix.SetMatrix43(mm);
	end;
	
	Result := s;
end;

procedure NewTemplate(const path : String; entities : TList; pivot : TEntity);
var
	sl : TStringList;
	I : Longint;
	s, ss, newdata : TSection;
	param_name : TStringValue;
begin
	sl := TStringList.Create;
	sl.Delimiter := '\';
	sl.DelimitedText := path;
	
	s := templates.root;
	for I := 0 to sl.Count - 2 do
	begin
		ss := s.GetSect(sl[I], False);
		if (ss = nil) or not IsFolder(ss) then
			ss := s.AddSect(sl[I]);
		
		s := ss;
	end;
	
	newdata := _CreateTemplate(entities, pivot);
	newdata.name := sl[sl.Count-1];
	
	if newdata.GetParam('id', 'u16') <> nil then
	begin
		param_name := newdata.GetParam('name', 'stringz') as TStringValue;
		param_name.str := sl[sl.Count-1];
	end;
	
	s.items.Add(newdata);
	
	sl.Free;
end;

procedure DeleteTemplate(t : TSection);
	procedure _Recurs(s, t : TSection);
	var
		I : Longint;
	begin
		if not IsFolder(s) then
			Exit;
	
		for I := 0 to s.items.Count - 1 do
		begin
			if s.items[I] = t then
			begin
				s.items.Remove(t);
				t.Free;
				Exit;
			end else
			if TObject(s.items[I]) is TSection then
				_Recurs(TSection(s.items[I]), t);
		end;
	end;
begin
	if Assigned(templates) then
		_Recurs(templates.root, t);
end;

function IsFolder(t : TSection) : Boolean;
begin
	IsFolder := (t.GetParam('id', 'u16') = nil) and (t.GetBool('is_group', False) = False);
end;

function _IsNameFree(const name : String) : Boolean;
begin
	Result := Scene.EntityByName(name) = nil;
end;

function _CreateGroup(template : TSection) : TEntityArray;
var
	I, J : Longint;
	
	data : array of TSection;
	
	ids : array of Word;
	new_ids : array of Word;
	
	entities : TEntityArray;
	
	name : String;
	new_data : TSection;
	p_id : TIntegerValue;
	
	procedure _RemapLinks(sect : TSection);
	var
		I, J : Longint;
		p : TIntegerValue;
	begin
		for I := 0 to sect.items.Count - 1 do
		begin
			if TSimpleValue(sect.items[I]).vtype = 'entity_link, uobject_link' then
			begin
				p := TSimpleValue(sect.items[I]) as TIntegerValue;
				for J := 0 to Length(ids) do
				begin
					if p.num = ids[J] then
					begin
						p.num := new_ids[J];
						Break;
					end;
				end;
			end else
			if TSimpleValue(sect.items[I]) is TSection then
				_RemapLinks(TSection(sect.items[I]));
		end;
	end;
begin
	// collect entities from template
	SetLength(data, 0);
	
	for I := 0 to template.items.Count - 1 do
		if TObject(template.items[I]) is TSection then
		begin
			SetLength(data, Length(data)+1);
			data[Length(data)-1] := TSection(template.items[I]);
		end;
		
	// create entities and remember original and new ids
	SetLength(ids, Length(data));
	SetLength(new_ids, Length(data));
	SetLength(entities, Length(data));
	
	for I := 0 to Length(data) - 1 do
	begin
		ids[I] := data[I].GetInt('id', 'u16');
		new_ids[I] := Scene.GenerateId;
		
		if new_ids[I] = 65535 then
		begin
			ShowError('Not enough IDs!');
			for J := 0 to I - 1 do
				Scene.RemoveEntity(entities[J]);	
				
			Result := nil;
			Exit;
		end;
		
		name := GenerateName(data[I].name, _IsNameFree);
		
		new_data := data[I].Copy as TSection;
		p_id := new_data.GetParam('id', 'u16') as TIntegerValue;
		p_id.num := new_ids[I];
		
		entities[I] := TEntity.Create(Scene.ph_scene, new_data);
		Scene.AddEntity(entities[I]);

		entities[I].Name := name;
	end;
	
	// remap ids
	for I := 0 to Length(entities) - 1 do
	begin
		for J := 0 to Length(ids) - 1 do
		begin
			if entities[I].ParentID = ids[J] then
			begin
				entities[I].ParentID := new_ids[J];
				Break;
			end;
		end;
				
		_RemapLinks(entities[I].data);
	end;
	
	Result := entities;
end;

function _CreateSingleEntity(template : TSection) : TEntity;
var
	id : Word;
	name : String;
	
	new_data : TSection;
	p_id : TIntegerValue;
	
	e : TEntity;
begin
	id := Scene.GenerateId;
	if id = 65535 then
	begin
		ShowError('Not enough IDs!');
		Result := nil;
		Exit;
	end;
	name := GenerateName(template.name, _IsNameFree);
	
	new_data := template.Copy as TSection;
	p_id := new_data.GetParam('id', 'u16') as TIntegerValue;
	p_id.num := id;
	
	e := TEntity.Create(Scene.ph_scene, new_data);
	Scene.AddEntity(e);

	e.Name := name;
	
	Result := e;
end;

function CreateEntities(template : TSection; const transform : TMatrix) : TEntityArray;
var
	I : Longint;
	g : TEntityArray;
	e : TEntity;
	matrix : TMatrix;
begin
	if template.GetBool('is_group', False) then
	begin
		g := _CreateGroup(template);
		for I := 0 to Length(g)-1 do
		begin
			matrix := transform;
			Mul44(matrix, g[I].Matrix); // mul by offset
			g[I].Matrix := matrix;
		end;
	end else
	begin
		e := _CreateSingleEntity(template);
		e.Matrix := transform;
		
		SetLength(g, 1);
		g[0] := e;
	end;
	
	Result := g;
end;

function CreateEntities(template : TSection) : TEntityArray;
var
	g : TEntityArray;
	e : TEntity;
begin
	if template.GetBool('is_group', False) then
	begin
		g := _CreateGroup(template);
	end else
	begin
		e := _CreateSingleEntity(template);
		SetLength(g, 1);
		g[0] := e;	
	end;
	
	Result := g;
end;

end.
