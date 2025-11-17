unit uFrameMtlsets;

interface
uses Iup;

function  CreateMtlsetsFrame : Ihandle;
procedure UpdateMtlsetsFrame;

implementation
uses classes, sysutils, fouramdl, uEditorUtils, uModelEditorGlobals, uChooseMaterial, uChooseTexture;

procedure _ResetOverride;
var
	override_shader : Ihandle;
	override_texture : Ihandle;
begin
	override_shader := IupGetDialogChild(MainDialog, 'OVERRIDE_SHADER');
	override_texture := IupGetDialogChild(MainDialog, 'OVERRIDE_TEXTURE');
	IupSetAttribute(override_shader, 'VALUE', '');
	IupSetAttribute(override_texture, 'VALUE', '');
end;

function list_mtlsets_select_cb(ih : Ihandle; title : PAnsiChar; item, state : Longint) : Longint; cdecl;
var
	list : Ihandle;
	hit_preset : Ihandle;
	voice : Ihandle;
	I : Longint;
begin
	if state = 1 then
	begin
		if item = 1 then
			current_mtlset := -1
		else
			current_mtlset := item-2;
			
		Redisplay;
	end;
	
	hit_preset := IupGetDialogChild(ih, 'TEXT_HIT_PRESET');
	voice := IupGetDialogChild(ih, 'TEXT_VOICE');
	list := IupGetDialogChild(ih, 'LIST_MTLSET_OVERRIDES');
	IupSetAttribute(list, 'REMOVEITEM', 'ALL');
	
	if current_mtlset <> -1 then
	begin
		iup.SetStrAttribute(hit_preset, 'VALUE', mtlsets^[current_mtlset].hit_preset);
		iup.SetStrAttribute(voice, 'VALUE', mtlsets^[current_mtlset].voice);
		
		for I := 0 to Length(mtlsets^[current_mtlset].materials) - 1 do
			iup.SetStrAttribute(list, 'APPENDITEM', mtlsets^[current_mtlset].materials[I].surface);		
	end else
	begin
		iup.SetStrAttribute(hit_preset, 'VALUE', '');
		iup.SetStrAttribute(voice, 'VALUE', '');
	end;
		
	_ResetOverride;
		
	Result := IUP_DEFAULT;
end;

function btn_new_cb(ih : Ihandle) : Longint; cdecl;
var
	list : Ihandle;
	name : array[0..255] of Char;
	str : String;
	I : Longint;
label 
	quit;
begin
	name[0] := #0;
	if IupGetParam('Enter name', nil, nil, 'Name: %s'#10, @name) = 1 then
	begin
		str := Trim(PAnsiChar(@name[0]));
		if str <> '' then
		begin
			// check if mtlset with given name already exists
			for I := 0 to Length(mtlsets^) - 1 do
				if mtlsets^[I].name = str then
				begin
					ShowError('Name ''' + str + ''' already used!');
					Goto quit;
				end;
				
			// if not, add new one
			I := Length(mtlsets^);
			SetLength(mtlsets^, I+1);
			
			mtlsets^[I].name       := str;
			mtlsets^[I].hit_preset := '';
			mtlsets^[I].voice      := '';
			mtlsets^[I].flags      := 0;
			mtlsets^[I].materials  := nil;
			
			list := IupGetDialogChild(ih, 'LIST_MTLSETS');
			iup.SetStrAttribute(list, 'APPENDITEM', str);
			
			UpdateMaler;
		end;
	end;
	quit:
	Result := IUP_DEFAULT;
end;

function btn_delete_cb(ih : Ihandle) : Longint; cdecl;
begin
	if current_mtlset <> -1 then
	begin
		Delete(mtlsets^, current_mtlset, 1);
		UpdateMtlsetsFrame;
		Redisplay;
	end;
	
	Result := IUP_DEFAULT;
end;

function list_mtlset_overrides_cb(ih : Ihandle; title : PAnsiChar; item, state : Longint) : Longint; cdecl;
var
	override_shader : Ihandle;
	override_texture : Ihandle;
begin
	if state = 1 then
	begin
		override_shader := IupGetDialogChild(ih, 'OVERRIDE_SHADER');
		override_texture := IupGetDialogChild(ih, 'OVERRIDE_TEXTURE');
		
		iup.SetStrAttribute(override_shader, 'VALUE', mtlsets^[current_mtlset].materials[item-1].new_shader);
		iup.SetStrAttribute(override_texture, 'VALUE', mtlsets^[current_mtlset].materials[item-1].new_texture);
	end;
	Result := IUP_DEFAULT;
end;

function btn_add_override_cb(ih : Ihandle) : Longint; cdecl;
var
	s_list : Ihandle;
	o_list : Ihandle;
	sl : TStringList;
	pmtlset : ^T4AMaterialSet;
	surface : String;
	I : Longint;
	ret : Longint;
	
	function HasOverride(const mtlset : T4AMaterialSet; const surface : String) : Boolean;
	var
		K : Longint;
	begin
		for K := 0 to Length(mtlset.materials) - 1 do
			if mtlset.materials[K].surface = surface then
			begin
				HasOverride := True;
				Exit;
			end;
			
		HasOverride := False;
	end;
begin
	s_list := IupGetDialogChild(ih, 'LIST_SURFACES');
	o_list := IupGetDialogChild(ih, 'LIST_MTLSET_OVERRIDES');
	
	if current_mtlset <> -1 then
	begin
		sl := TStringList.Create;
		pmtlset := @mtlsets[current_mtlset];
		
		for I := 1 to IupGetInt(s_list, 'COUNT') do
		begin
			surface := iup.GetAttribute(s_list, IntToStr(I));
			if not HasOverride(pmtlset^, surface) then
				sl.Add(surface);
		end;
		
		if sl.Count > 0 then
		begin
			ret := iup.ListDialog('Select surface', sl, 1, 20, 15);
			if ret <> -1 then
			begin
				I := Length(pmtlset.materials);
				SetLength(pmtlset.materials, I+1);
				
				pmtlset.materials[I].surface := sl[ret];
				pmtlset.materials[I].new_texture := '';
				pmtlset.materials[I].new_shader  := '';
				
				iup.SetStrAttribute(o_list, 'APPENDITEM', sl[ret]);
			end;
		end else
			IupMessage('Message', 'All surfaces already overriden');
		
		sl.Free;
	end;
	
	Result := IUP_DEFAULT;
end;

function btn_remove_override_cb(ih : Ihandle) : Longint; cdecl;
var
	o_list : Ihandle;
	pmtlset : ^T4AMaterialSet;
	sel : Longint;
begin
	o_list := IupGetDialogChild(ih, 'LIST_MTLSET_OVERRIDES');
	
	if current_mtlset <> -1 then
	begin
		pmtlset := @mtlsets[current_mtlset];
		sel := IupGetInt(o_list, 'VALUE');
		
		if sel > 0 then
		begin
			Delete(pmtlset.materials, sel-1, 1);
			iup.SetStrAttribute(o_list, 'REMOVEITEM', IntToStr(sel));
			
			_ResetOverride;
			UpdateMaler;
			Redisplay;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function text_texture_changed_cb(ih : Ihandle) : Longint; cdecl;
var
	pmtlset : ^T4AMaterialSet;
	override_id : Longint;
begin
	if current_mtlset <> -1 then
	begin
		pmtlset := @mtlsets[current_mtlset];
		override_id := IupGetInt(IupGetDialogChild(ih, 'LIST_MTLSET_OVERRIDES'), 'VALUE');
		if override_id <> 0 then
		begin
			pmtlset.materials[override_id-1].new_texture := IupGetAttribute(ih, 'VALUE');
			UpdateMaler;
			Redisplay;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function text_shader_changed_cb(ih : Ihandle) : Longint; cdecl;
var
	pmtlset : ^T4AMaterialSet;
	override_id : Longint;
begin
	if current_mtlset <> -1 then
	begin
		pmtlset := @mtlsets[current_mtlset];
		override_id := IupGetInt(IupGetDialogChild(ih, 'LIST_MTLSET_OVERRIDES'), 'VALUE');
		if override_id <> 0 then
		begin
			pmtlset.materials[override_id-1].new_shader := IupGetAttribute(ih, 'VALUE');
			UpdateMaler;
			Redisplay;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function btn_shader_cb(ih : Ihandle) : Longint; cdecl;
var
	pmtlset : ^T4AMaterialSet;
	override_id : Longint;
	override_shader : Ihandle;
begin
	if current_mtlset <> -1 then
	begin
		pmtlset := @mtlsets[current_mtlset];
		override_id := IupGetInt(IupGetDialogChild(ih, 'LIST_MTLSET_OVERRIDES'), 'VALUE');
		if override_id <> 0 then
			if ChooseShader(pmtlset.materials[override_id-1].new_shader) then
			begin
				override_shader := IupGetDialogChild(ih, 'OVERRIDE_SHADER');
				iup.SetStrAttribute(override_shader, 'VALUE', pmtlset.materials[override_id-1].new_shader);
				UpdateMaler;
				Redisplay;
			end;
	end;
		
	Result := IUP_DEFAULT;
end;

function btn_texture_cb(ih : Ihandle) : Longint; cdecl;
var
	pmtlset : ^T4AMaterialSet;
	override_id : Longint;
	override_texture : Ihandle;
begin
	if current_mtlset <> -1 then
	begin
		pmtlset := @mtlsets[current_mtlset];
		override_id := IupGetInt(IupGetDialogChild(ih, 'LIST_MTLSET_OVERRIDES'), 'VALUE');
		if override_id <> 0 then
			if ChooseTexture(pmtlset.materials[override_id-1].new_texture) then
			begin
				override_texture := IupGetDialogChild(ih, 'OVERRIDE_TEXTURE');
				iup.SetStrAttribute(override_texture, 'VALUE', pmtlset.materials[override_id-1].new_texture);
				UpdateMaler;
				Redisplay;
			end;
	end;
		
	Result := IUP_DEFAULT;
end;

function CreateMtlsetsFrame : Ihandle;
var
	list_mtlsets : Ihandle;
	btn_new : Ihandle;
	btn_delete : Ihandle;
	btn_load : Ihandle;
	btn_save : Ihandle;
	box_mtlset : Ihandle;
	hit_preset : Ihandle;
	voice : Ihandle;
	
	list_mtlset_overrides : Ihandle;
	btn_override_add : Ihandle;
	btn_override_remove : Ihandle;
	box_override : Ihandle;
	
	override_shader : Ihandle;
	btn_shader : Ihandle;
	box_shader : Ihandle;
	
	override_texture : Ihandle;
	btn_texture : Ihandle;
	box_texture : Ihandle;
	
	fr_mtlsets : Ihandle;
begin
	list_mtlsets := IupList(nil);
	IupSetAttribute(list_mtlsets, 'NAME', 'LIST_MTLSETS');
	IupSetAttribute(list_mtlsets, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(list_mtlsets, 'DROPDOWN', 'YES');
	IupSetAttribute(list_mtlsets, '1', '<none>');
	IupSetAttribute(list_mtlsets, 'VALUE', '1');
	IupSetCallback(list_mtlsets, 'ACTION', @list_mtlsets_select_cb);
	
	btn_new := iup.Button('New', @btn_new_cb);
	btn_delete := iup.Button('Delete', @btn_delete_cb);
	btn_load := iup.Button('Load', nil);
	btn_save := iup.Button('Save', nil);
	
	IupSetAttribute(btn_load, 'ACTIVE', 'NO');
	IupSetAttribute(btn_save, 'ACTIVE', 'NO');
	
	box_mtlset := IupHBox(btn_new, btn_delete, btn_load, btn_save, nil);
	IupSetAttribute(box_mtlset, 'MARGIN', '0x0');
	
	hit_preset := IupSetAttributes(IupText(nil), 'NAME=TEXT_HIT_PRESET, TIP="Hit Preset", EXPAND=HORIZONTAL');
	voice := IupSetAttributes(IupText(nil), 'NAME=TEXT_VOICE, TIP="Voice (for human models)", EXPAND=HORIZONTAL');
	
	list_mtlset_overrides := IupList(nil);
	IupSetAttribute(list_mtlset_overrides, 'NAME', 'LIST_MTLSET_OVERRIDES');
	IupSetAttribute(list_mtlset_overrides, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(list_mtlset_overrides, 'VISIBLELINES', '6');
	IupSetCallback(list_mtlset_overrides, 'ACTION', @list_mtlset_overrides_cb);
	
	btn_override_add := iup.Button('Add Override', @btn_add_override_cb);
	btn_override_remove := iup.Button('Remove Override', @btn_remove_override_cb);
	box_override := IupHBox(btn_override_add, btn_override_remove, nil);
	IupSetAttribute(box_override, 'MARGIN', '0x0');
	
	override_texture := IupSetAttributes(IupText(nil), 'NAME=OVERRIDE_TEXTURE, TIP="New Texture", EXPAND=HORIZONTAL');
	IupSetCallback(override_texture, 'VALUECHANGED_CB', @text_texture_changed_cb);
	btn_texture := iup.Button('Choose', @btn_texture_cb);
	box_texture := IupHBox(override_texture, btn_texture, nil);
	IupSetAttribute(box_texture, 'MARGIN', '0x0');	
	
	override_shader := IupSetAttributes(IupText(nil), 'NAME=OVERRIDE_SHADER, TIP="New Shader", EXPAND=HORIZONTAL');
	IupSetCallback(override_shader, 'VALUECHANGED_CB', @text_shader_changed_cb);
	btn_shader := iup.Button('Choose', @btn_shader_cb);
	box_shader := IupHBox(override_shader, btn_shader, nil);
	IupSetAttribute(box_shader, 'MARGIN', '0x0');
	
	fr_mtlsets := IupFrame(
		IupVBox(
			list_mtlsets,
			box_mtlset, 
			hit_preset, 
			voice, 
			list_mtlset_overrides,
			box_override, 
			box_texture, 
			box_shader, 
			nil
		)
	);
	IupSetAttribute(fr_mtlsets, 'NAME', 'FRAME_MTLSETS');
	//IupSetAttribute(fr_mtlsets, 'TITLE', 'Material sets');
	
	Result := IupSetAttributes(IupExpander(fr_mtlsets), 'TITLE="Material Sets"');
end;

procedure UpdateMtlsetsFrame;
var
	list_mtlsets : Ihandle;
	list_overrides : Ihandle;
	I, count : Longint;
begin
	list_mtlsets := IupGetDialogChild(MainDialog, 'LIST_MTLSETS');
	list_overrides := IupGetDialogChild(MainDialog, 'LIST_MTLSET_OVERRIDES');
	current_mtlset := -1;
	
	count := IupGetInt(list_mtlsets, 'COUNT');
	for I := count downto 2 do
		IupSetInt(list_mtlsets, 'REMOVEITEM', I);
		
	IupSetAttribute(list_overrides, 'REMOVEITEM', 'ALL');
	
	if Assigned(mtlsets) then
		for I := 0 to Length(mtlsets^) - 1 do
			IupSetAttribute(list_mtlsets, 'APPENDITEM', PAnsiChar(mtlsets^[I].name));
			
	_ResetOverride;
end;

end.