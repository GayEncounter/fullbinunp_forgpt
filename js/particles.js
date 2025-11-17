// partial particles.bin loader (for choose form in level editor)

var physx_type_preset = reader.ReadSection("physx_type_preset");

var effects = reader.ReadArray("effects");
for(var i = 0; effects.More(); i++)
{
	var effect = effects.ReadSection(RecStr("rec_", i, 4), false);
	effect.ReadString("name");
}

var modifs = reader.ReadArray("modifs");
for(var i = 0; modifs.More(); i++)
{
	var modif = modifs.ReadSection(RecStr("rec_", i, 4), false);
	modif.ReadString("name");
}

var systems = reader.ReadArray("systems");
for(var i = 0; systems.More(); i++)
{
	var system = systems.ReadSection(RecStr("rec_", i, 4), false);
	system.ReadString("name");
}