var i = 0, arr = reader.ReadArray("mp_class_type")
while(arr.More())
{
	var rec = arr.ReadSection(RecStr("rec_", i++, 4), false)
	
	rec.ReadString("name")
	rec.ReadU32("id")
	rec.ReadHintStr("third_model", "ref_model")
	rec.ReadHintStr("hud_model", "ref_model")
	rec.ReadHintStr("hands_model", "ref_model")
	rec.ReadFP32("luminocity")
	rec.ReadU32("sound_type_walk")
	rec.ReadU32("sound_type_run")
	rec.ReadU32("sound_type_crouch")
	rec.ReadU32("sound_type_sprint")
	rec.ReadU32("sound_type_jump")
	rec.ReadU32("sound_type_landing");
	
	[
		"ammo_545x39mm",
		"ammo_15mm",
		"ammo_044mm",
		"ammo_12x70mm",
		"ammo_arrow",
		"ammo_money",
		"ammo_flame",
		"dynamite",
		"dagger",
		"ammo_762x39mm",
		"ammo_762x39mm_mg",
		"ammo_545x39_fmj",
		"ammo_762x39_mg",
		"ammo_762x39_fmj",
		"ammo_044_fmj",
		"ammo_dshk",
		"wpn_arrow",
		"wpn_dagger",
		"wpn_dynamite",
		"wpn_flame_grenage",
		"wpn_sticky_dynamite",
		"wpn_launcher_time_grenage",
		"wpn_flamethrower_grenage",
		"wpn_claymore",
		"ammo_12x70mm_fire",
		"ammo_044mm_ap",
		"ammo_12x70mm_flamethrower"
	].forEach(function(n) { rec.ReadSection(n).ReadS32("max_count"); })
	
	rec.ReadFP32("base_speed")
	rec.ReadFP32("base_jump")
	rec.ReadS32("dynamite_restrictions")
	rec.ReadS32("sdynamite_restrictions")
	rec.ReadS32("fdynamite_restrictions")
	rec.ReadS32("ldynamite_restrictions")
	rec.ReadS32("arrow_restrictions")
	rec.ReadS32("knife_restrictions")
	rec.ReadS32("medkit_restrictions")
	rec.ReadS32("filter_restrictions")
	rec.ReadS32("claymore_restrictions")
	rec.ReadS32("c4_restrictions")
	rec.ReadBool("ranger_mode")
	
	var immunities = rec.ReadArray("immunities")
	for(var j = 0; immunities.More(); j++)
	{
		var elem = immunities.ReadSection(RecStr("rec_", j, 4), false)
		elem.ReadU8("id")
		elem.ReadFP32("value")
	}
	
	var net_props = rec.ReadSection("net_props");
	
	[
		"primary_wpn",
		"secondary_wpn",
		"devices",
		"granade",
		"perks",
		"unlocks",
		"rank",
		"unlock_store",
	].forEach(function(n) { net_props.ReadU32(n); })
	
	var wpn_rank_data = rec.ReadSection("wpn_rank_data");

	[
		"primary_weapon",
		"secondary_weapon",
		"devices",
		"perks",
	].forEach(function(n)
	{
		var i, arr = wpn_rank_data.ReadArray(n)
		for(i = 0; arr.More(); i++)
		{
			var elem = arr.ReadSection(RecStr("rec_", i, 4), false)
			elem.ReadU32("rank_id")
			elem.ReadU32("wpn_id")
			elem.ReadU32("bits_check")
		}
	});
}