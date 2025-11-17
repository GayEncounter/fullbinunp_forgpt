program level_mtls;
uses sysutils, fouramdl;

var
	I : Longint;
	l : T4ALevel;
begin
	l := T4ALevel.Create;
	
	l.Load(ParamStr(1));
	for I := 0 to Length(l.materials)-1 do
	begin
		WriteLn('Material #', I);
		WriteLn('Shader'#9, '''', l.materials[I].shader, '''');
		WriteLn('Texture'#9, '''', l.materials[I].texture, '''');
		WriteLn('Material'#9, '''', l.materials[I].material, '''');
		WriteLn('Flags'#9, '''', IntToHex(l.materials[I].unkn, 8), '''');
		WriteLn;
	end;
	
	l.Free;
end.