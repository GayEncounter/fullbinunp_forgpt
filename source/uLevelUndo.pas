unit uLevelUndo;

interface
uses uEntity;

procedure SetUndoLimit(limit : Longint);

procedure DoUndo;
procedure DoRedo;

procedure UndoClearHistory;
procedure UndoSave(const reason : String = ''; mod_entities : TEntityArray = nil);
procedure UndoSaveEnv(const reason : String = '');

implementation
uses classes, Konfig, uScene, Timer;

type
	TBackupType = (btFullBackup, btPartial, btEnvOnly);

type 
	TUndoRec = object
		backup_type : TBackupType;
		
		konf : TTextKonfig;
		konf_add : TTextKonfig;
		konf_env : TTextKonfig;
		
		entity_ids : array of Longint;
		entity_data : array of TSection;
		
		constructor InitFull;
		constructor InitEnv;
		constructor InitPartial(e : TEntityArray);
		destructor Done;
		
		procedure Restore;
	end;

constructor TUndoRec.InitFull;
begin
	backup_type := btFullBackup;
	
	konf     := Scene.konf.Copy;
	konf_add := Scene.konf_add.Copy;
	konf_env := Scene.SaveEnvironment;
end;

constructor TUndoRec.InitEnv;
begin
	backup_type := btEnvOnly;
	
	konf_env := Scene.SaveEnvironment;
end;

constructor TUndoRec.InitPartial(e : TEntityArray);
var
	I : Longint;
	index : Longint;
begin
	backup_type := btPartial;
	
	SetLength(entity_ids, Length(e));
	SetLength(entity_data, Length(e));
	
	for I := 0 to Length(e) - 1 do
	begin
		index := Scene.konf_entities.items.IndexOf(e[I].data);
		if index <> -1 then
		begin
			entity_ids[I] := index;
		end else
		if Scene.konf_add_entities <> nil then
		begin
			index := Scene.konf_add_entities.items.IndexOf(e[I].data);
			if index <> -1 then
				entity_ids[I] := $FF000000 or index
			else
				WriteLn('!!! Invalid entity, doesn''t belong to level.bin nor level.add.bin');
		end else
			WriteLn('!!! Invalid entity, doesn''t belong to level.bin');
		
		entity_data[I] := e[I].data.Copy as TSection;
	end;
end;
 
destructor TUndoRec.Done;
var
	I : Longint;
begin
	if backup_type = btFullBackup then
	begin
		konf.Free;
		konf_add.Free;
		konf_env.Free;
	end;
	
	if backup_type = btEnvOnly then
	begin
		konf_env.Free;
	end;
	
	if backup_type = btPartial then
	begin
		for I := 0 to Length(entity_data) - 1 do
			entity_data[I].Free;
			
		SetLength(entity_ids, 0);
		SetLength(entity_data, 0);
	end;
end;

function EntityBySection(data : TSection) : Longint;
var
	I : Longint;
begin
	for I := 0 to Scene.entities.Count - 1 do
		if TEntity(Scene.entities[I]).data = data then
		begin
			Result := I;
			Exit;
		end;
		
	Result := -1;
end;

procedure TUndoRec.Restore;
var
	I : Longint;
	index_of_section : Longint; // in Scene.konf_entities or Scene.konf_add_entities
	index_of_entity  : Longint; // in Scene.entities
	old_data : TSection;
	new_data : TSection;
begin
	if backup_type = btFullBackup then
	begin
		Scene.UnloadEntities;
		Scene.LoadEntities(konf.Copy, konf_add.Copy);
		
		Scene.UnloadEnvironment;
		Scene.env_zones := TList.Create;
		Scene.LoadEnvironment(konf_env);
	end;
	
	if backup_type = btEnvOnly then
	begin
		Scene.UnloadEnvironment;
		Scene.env_zones := TList.Create;
		Scene.LoadEnvironment(konf_env);
	end;
	
	if backup_type = btPartial then
	begin
		for I := 0 to Length(entity_ids) - 1 do
		begin
			if (entity_ids[I] and $FF000000) <> 0 then
			begin
				// entity stored in level.add.bin
				index_of_section := entity_ids[I] and $00FFFFFF;
				index_of_entity  := EntityBySection(Scene.konf_add_entities.items[index_of_section]);
				
				if index_of_entity = -1 then
				begin
					WriteLn('UNDO ERROR: Entity stored in partial backup but not found in scene');
					Continue;
				end;
				
				TEntity(Scene.entities[index_of_entity]).Free;
				
				old_data := Scene.konf_add_entities.GetParam(index_of_section) as TSection;
				new_data := entity_data[I].Copy as TSection;
				Scene.konf_add_entities.Replace(old_data, new_data);
				
				Scene.entities[index_of_entity] := TEntity.Create(Scene.ph_scene, new_data);
			end else
			begin
				// entity stored in level.bin
				index_of_section := entity_ids[I] and $00FFFFFF;
				index_of_entity  := EntityBySection(Scene.konf_entities.items[index_of_section]);
				
				if index_of_entity = -1 then
				begin
					WriteLn('UNDO ERROR: Entity stored in partial backup but not found in scene');
					Continue;
				end;
				
				TEntity(Scene.entities[index_of_entity]).Free;
				
				old_data := Scene.konf_entities.GetParam(index_of_section) as TSection;
				new_data := entity_data[I].Copy as TSection;
				Scene.konf_entities.Replace(old_data, new_data);
				
				Scene.entities[index_of_entity] := TEntity.Create(Scene.ph_scene, new_data);
			end;			
		end;
	end;
end;

var
	undo_buffer : array of TUndoRec;
	undo_next : Longint = 0;

var
	redo_buffer : array of TUndoRec;
	redo_next : Longint = 0;

procedure SetUndoLimit(limit : Longint);
begin
	UndoClearHistory;
	SetLength(undo_buffer, limit);
	SetLength(redo_buffer, limit);
end;

procedure UndoClear;
var
	I : Longint;
begin
	for I := 0 to undo_next-1 do
		undo_buffer[I].Done;
	
	undo_next := 0;
end;

procedure UndoPush(backup_type : TBackupType; mod_entities : TEntityArray = nil);
var
	tmr : TTimer;
begin
	if Length(undo_buffer) = 0 then
		Exit;
	
	if undo_next > High(undo_buffer) then
	begin
		undo_buffer[0].Done;
		
		Move(undo_buffer[1], undo_buffer[0], (Length(undo_buffer)-1)*Sizeof(TUndoRec) );
		Dec(undo_next);
	end;
	
	tmr.Start;
	
	case backup_type of
		btFullBackup: undo_buffer[undo_next].InitFull;
		btPartial:    undo_buffer[undo_next].InitPartial(mod_entities);
		btEnvOnly:    undo_buffer[undo_next].InitEnv;
	end;
	
	WriteLn('Backup time: ', tmr.CurrentTime:10:10, 's');
	
	Inc(undo_next);
	
	WriteLn('undo_push next = ', undo_next);
end;

procedure UndoPop;
begin
	undo_buffer[undo_next-1].Restore;
	undo_buffer[undo_next-1].Done;
	Dec(undo_next);
	
	WriteLn('undo_pop next = ', undo_next);
end;

procedure RedoClear;
var
	I : Longint;
begin
	for I := 0 to redo_next-1 do
		redo_buffer[I].Done;
	
	redo_next := 0;
end;

procedure RedoPush;
begin
	if Length(redo_buffer) = 0 then
		Exit;
	
	if redo_next > High(redo_buffer) then
	begin
		redo_buffer[0].Done;
		
		Move(redo_buffer[1], redo_buffer[0], (Length(redo_buffer)-1)*Sizeof(TUndoRec) );
		Dec(redo_next);
	end;
	
	redo_buffer[redo_next].InitFull;
	Inc(redo_next);
	
	WriteLn('redo_push next = ', redo_next);
end;

procedure RedoPop;
begin
	redo_buffer[redo_next-1].Restore;
	redo_buffer[redo_next-1].Done;
	Dec(redo_next);
	
	WriteLn('redo_pop next = ', redo_next);
end;

procedure DoUndo;
begin
	if undo_next > 0 then
	begin
		RedoPush;
		UndoPop;
	end;
end;

procedure DoRedo;
begin
	if redo_next > 0 then
	begin
		UndoPush(btFullBackup);
		RedoPop;
	end;
end;

procedure UndoClearHistory;
begin
	UndoClear;
	RedoClear;
end;

procedure UndoSave(const reason : String = ''; mod_entities : TEntityArray = nil);
begin
	WriteLn('UNDO SAVE - ', reason);
	RedoClear;
	if mod_entities = nil then
		UndoPush(btFullBackup)
	else
		UndoPush(btPartial, mod_entities);
end;

procedure UndoSaveEnv(const reason : String = '');
begin
	WriteLn('UNDO SAVE ENV - ', reason);
	RedoClear;
	UndoPush(btEnvOnly)
end;

end.
