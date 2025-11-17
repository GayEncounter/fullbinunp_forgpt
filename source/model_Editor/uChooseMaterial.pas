unit uChooseMaterial;

interface

function ChooseShader(var shader : String) : Boolean;
function ChooseMaterial(var material : String) : Boolean;

implementation
uses classes, Iup, Konfig, KonfigLibrary, framework;

function ChooseShader(var shader : String) : Boolean;
var
	I, op, ret : Longint;
	
	K : TKonfig;
	tk : TTextKonfig;
	js : TFramework;
	
	arr : TSection;
	elem : TSection;
	
	shaders : TStringList;
begin
	Result := False;
	
	K := KonfigLibrary.GetKonfig('render_subst_all');
	if K <> nil then
	begin
		js := TFramework.Create;
		tk := js.DecompileKonfig(K, 'js\render_subst_all.js');
		js.Free;
		K.Free;
		
		shaders := TStringList.Create;
		
		arr := tk.root.GetSect('render_subst_all');
		for I := 0 to arr.items.Count-1 do
		begin
			if TObject(arr.items[I]) is TSection then
			begin
				elem := TSection(arr.items[I]);
				shaders.Add(elem.GetStr('name'));
			end;
		end;
		
		tk.Free;
		
		shaders.Sort;
		
		op := 1;
		for I := 0 to shaders.Count-1 do
			if shaders[I] = shader then op := I+1;
			
		ret := iup.ListDialog('Select shader', shaders, op, 25, 40);
		if ret <> -1 then
		begin
			shader := shaders[ret];
			Result := True;
		end;
		
		shaders.Free;
	end;
end;

function ChooseMaterial(var material : String) : Boolean;
var
	I, op, ret : Longint;
	
	K : TKonfig;
	tk : TTextKonfig;
	js : TFramework;
	
	arr : TSection;
	elem : TSection;
	
	materials : TStringList;
begin
	Result := False;
	
	K := KonfigLibrary.GetKonfig('game_materials');
	if K <> nil then
	begin
		js := TFramework.Create;
		tk := js.DecompileKonfig(K, 'js\game_materials.js');
		js.Free;
		K.Free;
		
		materials := TStringList.Create;
		
		arr := tk.root.GetSect('materials');
		for I := 0 to arr.items.Count-1 do
		begin
			if TObject(arr.items[I]) is TSection then
			begin
				elem := TSection(arr.items[I]);
				materials.Add(elem.GetStr('name'));
			end;
		end;
		
		tk.Free;
		
		materials.Sort;
		
		op := 1;
		for I := 0 to materials.Count-1 do
			if materials[I] = material then op := I+1;
			
		ret := iup.ListDialog('Select material', materials, op, 25, 40);
		if ret <> -1 then
		begin
			material := materials[ret];
			Result := True;
		end;
		
		materials.Free;
	end;
end;

end.