var startup = reader.TryReadSection("startup") // in case of level.add.bin we don't have startup section
var entities_params = reader.ReadSection("entities_params")
var entities = reader.ReadArray("entities")

var entity_ver = entities_params.ReadU16("version")

if(entity_ver >= 49)
{
	var levelbin_exodus = module("exodus\\levelbin")
	if(startup) levelbin_exodus.ReadStartup(startup, entity_ver)
	levelbin_exodus.ReadEntities(entities, entity_ver)
} else
if(entity_ver >= 43)
{
	var levelbin_a1 = module("a1\\levelbin")
	if(startup) levelbin_a1.ReadStartup(startup, entity_ver)
	levelbin_a1.ReadEntities(entities, entity_ver)
} else
if(entity_ver >= 30)
{
	var levelbin_redux = module("redux\\levelbin")
	if(startup) levelbin_redux.ReadStartup(startup, entity_ver)
	levelbin_redux.ReadEntities(entities, entity_ver)
} else
if(entity_ver >= 28) //if(entity_ver >= 29)
{
	var levelbin_ll = module("ll\\levelbin")
	var exact_ver;
	
	if(entity_ver >= 29)
	{
		exact_ver = ENTITY_VER_29;
	}
	else
	{
		if(g_build_15_10_2012)
			exact_ver = ENTITY_VER_28a;
		else
			exact_ver = ENTITY_VER_28b;
	}
	
	if(startup) levelbin_ll.ReadStartup(startup, exact_ver)
	levelbin_ll.ReadEntities(entities, exact_ver)
} else
	print("unsupported level.bin version!")