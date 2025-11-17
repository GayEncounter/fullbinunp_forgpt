// partial particles.bin loader

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
	system.ReadFP32("time_limit");
	system.ReadHint("flags", "flags32");
	system.ReadU32("flags");
	system.ReadVec3("force_aabb_mn");
	system.ReadVec3("force_aabb_mx");
	system.ReadHintStr("physx_alias", "choose");
	
	var items = system.ReadArray("items");
	for(var j = 0; items.More(); j++)
	{
		var item = items.ReadSection(RecStr("item_", j, 4), false);
		
		item.ReadHintStr("main_effect_name", "particles_effect, str_shared");
		item.ReadFP32("timetable_1");
		item.ReadFP32("timetable_2");
		item.ReadHintStr("on_playing_effect_name", "particles_effect, str_shared");
		item.ReadHintStr("on_birth_effect_name", "particles_effect, str_shared");
		item.ReadHintStr("on_death_effect_name", "particles_effect, str_shared");
		item.ReadHintStr("on_kill_effect_name", "particles_effect, str_shared");
		item.ReadHint("flags", "flags32");
		item.ReadU32("flags");
	}
}