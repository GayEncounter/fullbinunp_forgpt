function ReadUCovers(e)
{
	for(var i = 0; e.More(); i++)
	{
		var cover = e.ReadSection(RecStr("rec_", i, 4), false);
		cover.ReadVec3("position");
		cover.ReadS32("ground_cell");
		cover.ReadS32("conn_reg");
		cover.ReadU32("dist_in_dir");
		cover.ReadU16("cover_id");
		cover.ReadU8("group0");
		cover.ReadU8("group1");
		cover.ReadU16("pe_index");
		cover.ReadU16("direction");
		cover.ReadU8("cover_type_id");
		cover.ReadHint("allowed_actions", "flags8");
		cover.ReadU8("allowed_actions");
		cover.ReadU8("radius", "fp32_q8");
	}
}

function ReadUGroups(e)
{
	for(var i = 0; e.More(); i++)
	{
		var group = e.ReadSection(RecStr("rec_", i, 4), false);
		group.ReadString("name");
	}
}

function ReadULinks(e)
{
	for(var i = 0; e.More(); i++)
	{
		var link = e.ReadSection(RecStr("rec_", i, 4), false);
		link.ReadU16("from", "cover_link, ucover_link");
		link.ReadU16("to", "cover_link, ucover_link");
		link.ReadHintStr("move_action", "animation_str");
		link.ReadHintStr("move_action_add", "animation_str");
		link.ReadU16("trans_frame");
		link.ReadU8("trans_speed");
		link.ReadU32("lnk_type");
		link.ReadU32("type");
		link.ReadU8("anim_state");
		link.ReadU8("mental_state");
	}
}

function ReadULinks_PE(e)
{
	for(var i = 0; e.More(); i++)
	{
		var link = e.ReadSection(RecStr("rec_", i, 4), false);
		link.ReadU16("from", "cover_link, ucover_link");
		link.ReadU16("to", "cover_link, ucover_link");
		link.ReadU32("cost");
		link.ReadU8("anim_state");
		link.ReadU8("mental_state");
	}
}

function ReadStartup(e)
{
	e.ReadString("desc_0")
	e.ReadString("desc_1")
	e.ReadS32("desc_time_0")
	e.ReadS32("desc_time_1")
	e.ReadFP32("weight")
	e.ReadString("next_desc")
	e.ReadS32("next_time")
	e.ReadHintStr("track", "camera_track, str_shared")
	e.ReadHintStr("envmap", "texture, str_shared")
	e.ReadFP32("view_distance")
	e.ReadString("next_level")
	e.ReadString("back_music")
}

var entity_readers = {
	"STATICPROP"                    : ReadUObject_Static,
	"VISUALSCRIPT"                  : ReadUObject,
	"PROXY"                         : ReadUObject_Proxy,
	"O_AIPOINT"                     : ReadUObject_AIPoint,
	"PATROL_POINT"                  : ReadPatrolPoint,
	"EFFECT"                        : ReadUObject_Effect,
	"O_ENTITY"                      : ReadCEntity,
	"o_hlamp"                       : ReadHangingLamp,
	"O_BASEZONE"                    : ReadUObject_Zone,
	"O_WATERZONE"                   : ReadWaterzone,
	"ANIM_OBJECT"					: ReadUObject_Effect,
	"O_EXPLOSION"					: ReadUObject_Explosion,
	"LADDER"                        : ReadLadder,
	"SOFT_ENTITY"                   : ReadSoftEntity,
	"SOFT_ENTITY_INST"              : ReadUObject,
	"FLOWER"                        : ReadFlower,
	"LIAN"                          : ReadCEntity,
	"BIOMASS_LARGE"                 : ReadCEntity,
	
	"SIMPLE_NPC"                    : ReadCEntity,

	"HANDS_FOR_DREZINA"             : ReadUObject_Effect,
	"PLAYERS_HANDS"                 : ReadUObject_Effect,
	"PLAYERS_KNIFE"                 : ReadUObject_Effect,
	"TORCH"                         : ReadHangingLamp,
	
//	"AMMO"                            : ReadAmmo,
//	"CHARGER"                         : ReadDevice,
//	"FILTER"                          : ReadFilter,
//	"GASMASK"                         : ReadGasmask,
//	"HELSING_ARROW"                   : ReadAmmo,
//	"PLAYER_MAP"                      : ReadPlayerMap
}

function ReadVsRef(e, name)
{
	var n = e.ReadHintStr(name, "vs_ref, str_shared");
	e.ReadBool(name + "_dyn_state_exist");
}

function ReadCommonsVs(e)
{
	var k = 0, arr = e.ReadArray("commons_vs");
	while(arr.More())
	{
		var vs = arr.ReadSection(RecStr("rec_", k++, 4), false);
		var name = vs.ReadString("vs_name");
		ReadVsRef(vs, "vs_ref");
		
		if(vs.More())
			print("VS REF " + name + " data left");
	}
}

var visualscript = module("2033\\visualscript")

function ReadUObject(e)
{
	e.ReadU8("oflags", "bool8")
	e.ReadMatrix44("", "pose, matrix")
	e.ReadName("name")
	e.ReadHintStr("visual", "ref_model")
	e.ReadFP32("dao_val")
	visualscript.ReadVssVer6(e)
}

function ReadUObject_Static(e)
{
	ReadUObject(e)
	
	e.ReadBool("raycast")
}

function ReadUObject_Proxy(e)
{
	ReadUObject(e)
	
	var entities = e.ReadArray("entities")
	for(var i = 0; entities.More(); i++)
	{
		var elem = entities.ReadSection(RecStr("rec_", i, 4), false) // really section name is entity name
		elem.ReadU16("entity", "entity_link, uobject_link")
	}
}

function ReadSoftEntity(e)
{
	ReadUObject(e)
	
	e.ReadBool("sleeping")
	e.ReadU16("collision_group")
	e.ReadBool("self_collision")
	e.ReadBool("disable_collision")
	e.ReadBool("apply_gravity")
	e.ReadBool("two_way_collision")
	e.ReadBool("auto_attach")
	e.ReadFP32("two_way_clg_factor")
	e.ReadFP32("attach_clg_factor")
	e.ReadBool("rbody_collidable")
}

function ReadUObject_AIPoint(e)
{
	ReadUObject(e)
	
	for(var i = 0; i < 4; i++)
	{
		var l = e.ReadSection("link_"+i)
		l.ReadU16("object", "entity_link, uobject_link")
		l.ReadFP32("weight")
	}
}

function ReadPatrolPoint(e)
{
	ReadUObject_AIPoint(e)
	
	e.ReadU32("min_wait_time")
	e.ReadU32("max_wait_time")
	e.ReadString("body_state")
	e.ReadString("anim_state")
	e.ReadString("movement_type")
	e.ReadString("weapon_state")
	e.ReadString("action")
	e.ReadU16("target", "entity_link, uobject_link")
	e.ReadHint("flags", "flags32")
	e.ReadU32("flags")
	e.ReadFP32("anim_state_approach_speed")
}

function ReadUObject_Restrictor(e)
{
	ReadUObject(e);
	
	var shapes = e.ReadArray("shapes");
	for(var k = 0; shapes.More(); k++)
	{
		var shape = shapes.ReadSection(RecStr("shape_", k, 2), false);
		var type = shape.ReadU32("type");
		
		switch(type)
		{
			case 0:
				var s = shape.ReadSection("sphere");
				s.ReadVec3("center");
				s.ReadFP32("radius");
			break;
			
			case 1:
				var b = shape.ReadSection("box");
				b.ReadMatrix44("");
				b.ReadVec3("h_size");
			break;
			
			default:
				print("unknown shape type ", type);
		}
	}
	
	e.ReadBool("obstacle")
	e.ReadBool("force_sliding")
}

function ReadUObject_Zone(e)
{
	ReadUObject_Restrictor(e)
	
	e.ReadU8("zone_type")
}

function ReadLadder(e)
{
	ReadUObject_Restrictor(e)
	
	e.ReadFP32("threshold_offset")
	e.ReadU8("detach_side")
	e.ReadFP32("detach_distance")
	e.ReadFP32("detach_speed")
	e.ReadBool("enabled")
}

function ReadWaterzone(e)
{
	ReadUObject_Zone(e)
	
	e.ReadVec4("color", "color, vec4f")
	e.ReadVec2("blend")
	e.ReadFP32("eblend")
	e.ReadFP32("fresnel")
	e.ReadFP32("tsmoosh")
	e.ReadFP32("twave")
	e.ReadVec2("tile")
	e.ReadVec2("amp")
	e.ReadVec2("blur")
	e.ReadFP32("d_power")
	e.ReadFP32("d_nf")
	e.ReadFP32("farplane")
	e.ReadBool("reflectplayer")
	e.ReadFP32("opaque_distance")
}

function ReadUObject_Effect(e)
{
	ReadUObject(e)
	
	e.ReadS32("life_time")
	e.ReadHintStr("startup_animation", "animation_str")
	e.ReadHintStr("bone_part", "part_id")
	e.ReadHintStr("sound", "sound")
	e.ReadFP32("sound_volume")
	e.ReadHintStr("particles", "particles, str_shared")
	e.ReadBool("particles_constrained")
	var i = e.ReadSection("interest")
	i.ReadS32("min_importance")
	i.ReadS32("max_importance")
	i.ReadU32("interest_type")
	i.ReadU32("duration")
	i.ReadFP32("speed")
	i.ReadFP32("distance")
	e.ReadStrArray32("labels", "str_array")
}

function ReadUObject_Explosion(e)
{
	ReadUObject(e)
	
	e.ReadU32("delay")
	e.ReadFP32("wallmark_size")
	e.ReadHintStr("explode_particles", "particles, str_shared")
	e.ReadHintStr("sound_explode", "sound")
	e.ReadHintStr("camera_effect_name", "camera_track, str_shared")
	e.ReadFP32("blast_radius_min")
	e.ReadFP32("blast_radius_max")
	e.ReadFP32("blast_hit_min")
	e.ReadFP32("blast_hit_max")
	e.ReadFP32("blast_hit_impulse_factor")
	e.ReadFP32("up_throw_factor")
	e.ReadU8("hit_blast")
	e.ReadU32("frags_num")
	e.ReadFP32("frags_radius")
	e.ReadFP32("frag_hit")
	e.ReadFP32("frag_hit_impulse")
	e.ReadU8("hit_frag")
	e.ReadVec4("light_color", "color, vec4f")
	e.ReadFP32("light_range")
	e.ReadU32("light_time_max")
	e.ReadFP32("fragment_speed")
	e.ReadU32("explode_duration_max");
}

function ReadCEntity(e)
{
	e.ReadFP32("health")
	e.ReadFP32("stamina")
	
	ReadUObject_Effect(e)
	
	e.ReadBool("collision_exist")
	e.ReadU8("friend_type")
	e.ReadStrArray32("fixed_bones", "str_array")
	e.ReadHintStr("bone_root", "bone_id")
	e.ReadBool("is_physics")
	e.ReadBool("sleeping")
	e.ReadBool("kinematic")
	e.ReadBool("force_kinematic")
	e.ReadBool("raycast")
	e.ReadBool("block_breacking")
	e.ReadFP32("break_impulse_threshold")
	e.ReadU8("collisions_group")
	e.ReadHintStr("break_particles_break", "particles, str_shared")
	e.ReadHintStr("break_particles_death", "particles, str_shared")
	e.ReadU32("ph_shell_model_src")
	e.ReadU32("ph_shell_skltn_src")
	e.ReadU32("ph_shell_skltn_bcount")
	var writed = e.ReadBool("ph_shell_writed")
	if(writed)
	{
		var s = e.ReadSection("physics_shell")
		s.ReadBool("attached")
		s.ReadBool("rootmover_mode")
		var elements = s.ReadArray("elements")
		for(var i = 0; elements.More(); i++)
		{
			var elem = elements.ReadSection(RecStr("rec_", i, 4), false)
			elem.ReadU16("root_bid")
			elem.ReadFP32("accumulated_impulse")
			elem.ReadMatrix44("xform", "pose, matrix")
			elem.ReadVec3("velocity")
			elem.ReadBool("nx_awake")
			var shapes = elem.ReadArray("shapes")
			for(var j = 0; shapes.More(); j++)
			{
				var p = shapes.ReadSection(RecStr("rec_", j, 4), false)
				p.ReadU16("bid")
			}
		}
	}
	var links = e.ReadArray("ph_links")
	// TODO
}

function ReadHangingLamp(e)
{
	ReadCEntity(e)
	
	e.ReadVec3("particles_bone_shift")
	e.ReadBool("initial_state")
	e.ReadU32("die_sound_type")
	e.ReadStrArray32("die_sounds", "str_array")
	e.ReadHintStr("die_particle", "particles, str_shared")
	e.ReadHintStr("light_main_bone", "bone_id")
	e.ReadHintStr("dark_bone", "bone_id")
	e.ReadVec4("color", "color, vec4f")
	e.ReadFP32("brightness")
	e.ReadHintStr("color_animator", "ref_coloranim")
	e.ReadU8("ltype")
	e.ReadBool("volumetric")
	e.ReadBool("x360falt")
	e.ReadBool("ltric")
	e.ReadHint("faces", "flags8")
	e.ReadU8("faces")
	e.ReadVec4("color_middle", "color, vec4f")
	e.ReadVec4("color_back", "color, vec4f")
	e.ReadU8("scurve")
	e.ReadFP32("shadow_clarity")
	e.ReadFP32("range")
	e.ReadFP32("nearplane")
	e.ReadFP32("spot_cone_angle", "angle, fp32")
	e.ReadHintStr("light_texture", "texture, str_shared")
	e.ReadBool("is_point_ambient")
	e.ReadHintStr("ambient_bone", "bone_id")
	e.ReadFP32("ambient_power")
	e.ReadFP32("ambient_radius")
	e.ReadHintStr("ambient_texture", "texture, str_shared")
	e.ReadBool("backlight")
	e.ReadU16("backlight_ref", "entity_link, uobject_link")
	e.ReadFP32("backlight_dist")
	e.ReadBool("backlight_dynamic")
	e.ReadVec3("ellipse_scale")
	e.ReadVec2("quad_sizes")
	e.ReadU16("master", "entity_link, uobject_link")
	e.ReadBool("glow_enabled")
	e.ReadHintStr("glow_texture", "texture, str_shared")
	e.ReadFP32("glow_size")
	e.ReadVec3("glow_shift")
	e.ReadFP32("glow_brightness")
	e.ReadBool("glow_ignore_self")
	e.ReadFP32("glow_min_distance")
}

function ReadFlower(e)
{
	ReadCEntity(e)
	
	e.ReadBool("light_active")
	e.ReadFP32("light_range")
	e.ReadFP32("light_offset")
	e.ReadVec4("light_color", "color, vec4f")
	e.ReadFP32("light_brightness")
	e.ReadFP32("light_shadow_clarity")
	e.ReadHintStr("light_anim_darkest", "ref_coloranim")
	e.ReadHintStr("light_anim_lightest", "ref_coloranim")
	e.ReadU8("light_type")
	e.ReadBool("sounds_enabled")
	e.ReadFP32("range")
}

/*--------------------------------*/
/*						NPCs								*/
/*--------------------------------*/
function ReadBaseNpc(e, brain_unit_read_func)
{
	e.ReadU32("voice_id")
	e.ReadBool("zombie_mode")
	
	ReadCEntity(e)
	
	e.ReadBool("scripted")
	e.ReadFP32("luminocity")
	e.ReadFP32("shoot_dispersion_coef")
	e.ReadFP32("hit_power_coef")
	e.ReadFP32("hit_impulse_coef")
	e.ReadS32("offmesh_penalty")
	e.ReadU32("offmesh_reuse_delay")
	e.ReadU32("offmesh_inter_reuse_delay")
	
	ReadVsRef(e, "movement_hit_vs_ref") // belongs to brain unit too?
	
	brain_unit_read_func(e)
	
	e.ReadBool("zombie")
	e.ReadBool("dying")
	ReadCommonsVs(e)
}

function ReadHuman(e, brain_unit_read_func)
{
	ReadBaseNpc(e, brain_unit_read_func)
	
	var a = e.ReadArray("inv_activation_queue") // TODO parse
	var a = e.ReadArray("inv_activation_queue_civil")
	for(var i = 0; a.More(); i++)
	{
		var elem = a.ReadSection(RecStr("rec_", i, 4), false)
		elem.ReadU8("slot")
		elem.ReadBool("active")
	}
	e.ReadU8("inv_next_active_slot")
	e.ReadU8("inv_next_deactive_slot")
	e.ReadU8("inv_last_slot")
	e.ReadBool("inv_next_active_slot_nmodal")
	e.ReadU8Array("inventory_history")
	e.ReadBool("scripted_fire")
	e.ReadBool("freegun_mode")
	e.ReadString("allowed_speech_groups")
}

function ReadCPlayer(e, brain_unit_read_func)
{	
	e.ReadFP32("cvr_grenade_danger_interval")
	e.ReadFP32("cvr_grenade_danger_distance")
	e.ReadFP32("cvr_grenade_danger_penalty")
	e.ReadFP32("cvr_cover_hit_danger_interval")
	e.ReadFP32("cvr_cover_hit_danger_distance")
	e.ReadFP32("cvr_cover_hit_danger_penalty")
	e.ReadFP32("cvr_hit_danger_interval")
	e.ReadFP32("cvr_hit_danger_distance")
	e.ReadFP32("cvr_hit_danger_penalty")
	e.ReadFP32("cvr_death_danger_interval")
	e.ReadFP32("cvr_death_danger_distance")
	e.ReadFP32("cvr_death_danger_penalty")
	e.ReadFP32("role_border0")
	e.ReadFP32("role_time0")
	e.ReadFP32("role_border1")
	e.ReadFP32("role_time1")
	
	ReadHuman(e, brain_unit_read_func)
	
	e.ReadSection("arms").ReadHintStr("suit_visual", "ref_model")
	e.ReadBool("tired_for_sprint")
	e.ReadU16("players_hands", "entity_link, uobject_link")
	e.ReadU16("players_knife", "entity_link, uobject_link")
	e.ReadU16("players_torch", "entity_link, uobject_link")
	e.ReadFP32("suit_luminocity")
	e.ReadFP32("def_restore_rate")
	e.ReadStrArray32("private_data", "str_array")
	var a = e.ReadArray("property_data") // TODO parse
	var a = e.ReadArray("achievement_data") // TODO parse
	var a = e.ReadArray("points_data") // TODO parse
	e.ReadBool("spend_filters")
}

function ReadBaseBrainUnit(e)
{
	e.ReadU8("anim_state")
	e.ReadU8("movement_type")
	e.ReadU8("body_state")
	e.ReadString("group")
	e.ReadBool("in_group")
	e.ReadBool("memory_valid")
	
	e.ReadFP32("free_vision_range")
	e.ReadFP32("free_vision_bwd_range")
	e.ReadFP32("free_vision_fov", "angle, fp32")
	e.ReadU32("free_vision_min_update_time")
	e.ReadFP32("free_vision_luminocity_fac")
	e.ReadFP32("free_vision_luminocity_factor")
	e.ReadFP32("free_vision_velocity_fac")
	e.ReadFP32("free_vision_decrease_value")
	e.ReadFP32("free_vision_threat_threshold")
	e.ReadFP32("free_vision_light_alert_threshold")
	e.ReadFP32("free_vision_vis_threshold")
	e.ReadFP32("free_vision_min_threshold")
	e.ReadU32("free_vision_transition_time")
	e.ReadFP32("alert_vision_range")
	e.ReadFP32("alert_vision_bwd_range")
	e.ReadFP32("alert_vision_fov", "angle, fp32")
	e.ReadU32("alert_vision_min_update_time")
	e.ReadFP32("alert_vision_luminocity_fac")
	e.ReadFP32("alert_vision_luminocity_factor")
	e.ReadFP32("alert_vision_velocity_fac")
	e.ReadFP32("alert_vision_decrease_value")
	e.ReadFP32("alert_vision_threat_threshold")
	e.ReadFP32("alert_vision_light_alert_threshold")
	e.ReadFP32("alert_vision_vis_threshold")
	e.ReadFP32("alert_vision_min_threshold")
	e.ReadU32("alert_vision_transition_time")
	e.ReadFP32("danger_vision_range")
	e.ReadFP32("danger_vision_bwd_range")
	e.ReadFP32("danger_vision_fov", "angle, fp32")
	e.ReadU32("danger_vision_min_update_time")
	e.ReadFP32("danger_vision_luminocity_fac")
	e.ReadFP32("danger_vision_luminocity_factor")
	e.ReadFP32("danger_vision_velocity_fac")
	e.ReadFP32("danger_vision_decrease_value")
	e.ReadFP32("danger_vision_threat_threshold")
	e.ReadFP32("danger_vision_light_alert_threshold")
	e.ReadFP32("danger_vision_vis_threshold")
	e.ReadFP32("danger_vision_min_threshold")
	e.ReadU32("danger_vision_transition_time")
	
	e.ReadFP32("free_disturb_threshold")
	e.ReadFP32("free_light_alert_threshold")
	e.ReadFP32("free_alert_threshold")
	e.ReadFP32("free_danger_threshold")
	e.ReadFP32("alert_disturb_threshold")
	e.ReadFP32("alert_light_alert_threshold")
	e.ReadFP32("alert_alert_threshold")
	e.ReadFP32("alert_danger_threshold")
	e.ReadFP32("danger_disturb_threshold")
	e.ReadFP32("danger_light_alert_threshold")
	e.ReadFP32("danger_alert_threshold")
	e.ReadFP32("danger_danger_threshold")
	
	e.ReadU32("la_free_inertia_time")
	e.ReadU32("max_sounds_count")
	e.ReadU32("la_alert_time")
	e.ReadU32("vision_time")
	e.ReadU32("vision_period")
	e.ReadU32("sound_count")
	e.ReadU32("sound_period")
	e.ReadU32("a_free_inertia_time")
	
	e.ReadU32("post_enemy_wait_interval_static")
	e.ReadU32("post_enemy_wait_interval_random")
	e.ReadU32("enemy_inertia_time")
	e.ReadU32("enemy_remember_time")
	
	e.ReadString("sound_scheme")
	
	e.ReadBool("can_turn_body")
	e.ReadFP32("max_heading", "angle, fp32")
	e.ReadFP32("max_pitch", "angle, fp32")
	e.ReadBool("no_combat_mode")
}

function ReadAIBrainUnit(e, attacks_list)
{
	ReadBaseBrainUnit(e)
	
	e.ReadU8("target_anim_state")
	e.ReadU8("min_anim_state")
	e.ReadU8("max_anim_state")
	e.ReadU8("min_movement_type")
	e.ReadU8("max_movement_type")
	e.ReadBool("static_combat_mode")
	var c = e.ReadSection("static_combat")
	c.ReadBool("hold_position")
	c.ReadU16("anchor", "entity_link, uobject_link")
	c.ReadFP32("radius")
	c.ReadFP32("enemy_dist")
	c.ReadBool("point_valid")
	c.ReadVec3("point_pos")
	c.ReadVec3("point_dir")
	e.ReadBool("cover_combat_mode")
	var attacks = e.ReadSection("attacks");
	for(var i = 0; i < attacks_list.length; i++)
	{
	/*
		var a = attacks.TryReadSection(attacks_list[i]);
		if(!a)
		{
			print("Warning! Cannot open section '" + attacks_list[i] + "' while loading attacks");
			print("(Note: for dlc_tower1 and dlc_faction_hanza levels this is fine)");
			continue;
		}
	*/
		var a = attacks.ReadSection(attacks_list[i]);	
		a.ReadBool("enabled");
		ReadVsRef(a, "vs_reference")
		a.ReadFP32("probability");
	}
}

function ReadMonsterBrainUnit(e, attacks_list)
{
	ReadAIBrainUnit(e, attacks_list)
	
	e.ReadFP32("monster_stand_lin_vel")
	e.ReadFP32("monster_stand_legs_ang_vel")
	e.ReadFP32("monster_slow_lin_vel")
	e.ReadFP32("monster_slow_legs_ang_vel")
	e.ReadFP32("monster_fast_legs_ang_vel")
}

function ReadHumanBrainUnit(e, attacks_list)
{
	ReadBaseBrainUnit(e)
	
	e.ReadU8("target_anim_state")
	e.ReadU8("min_anim_state")
	e.ReadU8("max_anim_state")
	e.ReadU8("min_movement_type")
	e.ReadU8("max_movement_type")
	e.ReadBool("static_combat_mode")
	var c = e.ReadSection("static_combat")
	c.ReadBool("hold_position")
	c.ReadU16("anchor", "entity_link, uobject_link")
	c.ReadFP32("radius")
	c.ReadFP32("enemy_dist")
	c.ReadBool("point_valid")
	c.ReadVec3("point_pos")
	c.ReadVec3("point_dir")
	
	e.ReadBool("force_active_wo_enemy")
	e.ReadHintStr("static_idle", "animation_str")
	e.ReadHintStr("static_attack", "animation_str")
	e.ReadHintStr("static_reload_idle", "animation_str")
	
	e.ReadBool("cover_combat_mode")
	
	var c = e.ReadSection("cover_combat")
	c.ReadBool("force_active_wo_enemy")
	c.ReadU32("timeout_min")
	c.ReadU32("timeout_max")
	c.ReadU32("timeout_min_see")
	c.ReadU32("timeout_max_see")
	c.ReadFP32("weight_delta")
	c.ReadFP32("enemy_min")
	c.ReadFP32("enemy_max")
	c.ReadFP32("cover_min")
	c.ReadFP32("cover_max")
	c.ReadBool("grenadeer")
	var c = e.ReadSection("cover")
	c.ReadU32("enemy_seen_timeout")
	c.ReadU32("lookout_min")
	c.ReadU32("lookout_max")
	c.ReadU32("aim_while_lookout_timeout")
	var c = e.ReadSection("alert")
	c.ReadU32("timeout_go_search")
	c.ReadU32("timeout_nervous_alert")
	c.ReadU32("timeout_nervous_alert_uber")
	c.ReadBool("search_enabled")
	
	var attacks = e.ReadSection("attacks");
	for(var i = 0; i < attacks_list.length; i++)
	{
	/*
		var a = attacks.TryReadSection(attacks_list[i]);
		if(!a)
		{
			print("Warning! Cannot open section '" + attacks_list[i] + "' while loading attacks");
			print("(Note: for dlc_tower1 and dlc_faction_hanza levels this is fine)");
			continue;
		}
	*/
		var a = attacks.ReadSection(attacks_list[i]);	
		a.ReadBool("enabled");
		ReadVsRef(a, "vs_reference")
		a.ReadFP32("probability");
	}
		
	e.ReadFP32("close_distance")
	e.ReadU32("close_min_queue_size")
	e.ReadU32("close_max_queue_size")
	e.ReadU32("close_min_queue_interval")
	e.ReadU32("close_max_queue_interval")
	e.ReadU32("min_queue_size")
	e.ReadU32("max_queue_size")
	e.ReadU32("min_queue_interval")
	e.ReadU32("max_queue_interval")
	e.ReadU32("behaviour_type")
}

entity_readers["AMEBA"] = function(e)
{
	function ReadBrain(e)
	{
		ReadMonsterBrainUnit(e, [])
	}
	
	ReadBaseNpc(e, ReadBrain)
	
	e.ReadU32("delay")
	e.ReadFP32("wallmark_size")
	e.ReadHintStr("explode_particles", "particles, str_shared")
	e.ReadHintStr("sound_explode", "sound")
	e.ReadHintStr("camera_effect_name", "camera_track, str_shared")
	e.ReadHintStr("suicide_particles", "particles, str_shared")
	e.ReadHintStr("death_particles", "particles, str_shared")
}

entity_readers["ANOMALY"] = function(e)
{
	function ReadBrain(e)
	{
		ReadAIBrainUnit(e, [])
	}
	
	ReadBaseNpc(e, ReadBrain)
}

entity_readers["DARK"] = function(e)
{
	function ReadBrain(e)
	{
		ReadMonsterBrainUnit(e, [])
	}
	
	ReadBaseNpc(e, ReadBrain)
}

entity_readers["HARPY"] = function(e)
{
	function ReadBrain(e)
	{
		ReadMonsterBrainUnit(e, [])
	}
	
	ReadBaseNpc(e, ReadBrain)
}

entity_readers["KID"] = function(e)
{
	ReadBaseNpc(e, ReadBaseBrainUnit)
}

entity_readers["LIBRARIAN"] = function(e)
{
	function ReadBrain(e)
	{
		var attacks = [
			"melee_attack1",
			"melee_attack2",
			"melee_attack0",
			"melee_attack3",
			"melee_attack4",
			"melee_attack5",
			"melee_attack_add0",
			"melee_attack_add1",
			"melee_attack_add2",
			"melee_attack_add3",
			"melee_attack_add4",
			"melee_attack_add5"
		]
		
		ReadMonsterBrainUnit(e, attacks)
		
		e.ReadFP32("librarian_light_dmg_agression_time")
		e.ReadFP32("librarian_heavy_dmg_agression_time")
		e.ReadFP32("librarian_nearby_attack_agression_time")
		e.ReadFP32("librarian_not_look_agression_time")
		e.ReadU32("librarian_aggression_radius")
		e.ReadFP32("librarian_attack_after_turn_probability")
		e.ReadBool("librarian_sprint_acceleration")
		e.ReadFP32("librarian_look_around_probability")
		e.ReadFP32("librarian_look_timeout_min")
		e.ReadFP32("librarian_look_timeout_max")
		e.ReadU32("librarian_look_radius_min")
		e.ReadU32("librarian_look_radius_max")
		e.ReadFP32("librarian_crit_hide_probability")
		e.ReadFP32("librarian_angry_idle_interval_min")
		e.ReadFP32("librarian_angry_idle_interval_max")
		e.ReadU32("librarian_look_eye_fov")
		e.ReadBool("librarian_hide_instead_escape")
		e.ReadFP32("librarian_wait_in_hide_time")
		e.ReadBool("librarian_return_if_enemy_on_map")
	}
	
	ReadBaseNpc(e, ReadBrain)
}

entity_readers["LURKER"] = function(e)
{
	function ReadBrain(e)
	{
		var attacks = [
			"melee_attack_360_jump",
			"melee_attack_360_jaw"
		]
		
		ReadMonsterBrainUnit(e, attacks)
	}
	
	ReadBaseNpc(e, ReadBrain)
}

entity_readers["NOSALIS"] = function(e)
{
	function ReadBrain(e)
	{
		var attacks = [
			"melee_attack_360_jaw",
			"melee_attack_360_arm",
			"melee_attack_360_jump",
			"melee_attack_8m",
			"melee_attack_0",
			"melee_attack_1",
			"melee_attack_2",
			"melee_attack_3",
			"melee_attack_4",
			"melee_attack_run_near",
			"melee_attack_run",
			"melee_attack_run_knife"
		]
		
		ReadMonsterBrainUnit(e, attacks)
	}
	
	ReadBaseNpc(e, ReadBrain)
}

entity_readers["NOSALIS_FEMALE"] = function(e)
{
	function ReadBrain(e)
	{
		ReadAIBrainUnit(e, ["melee_attack_0"])
	}
	
	ReadBaseNpc(e, ReadBrain)
}

entity_readers["NPC_FX"] = function(e)
{
	function ReadNpcBrainUnit(e)
	{
		var attacks = [
			"melee_attack_360_butt",
			"melee_attack_360_kick",
			"melee_attack_360_shtyk",
			"melee_attack_360_bzyk"
		]
		
		ReadHumanBrainUnit(e, attacks)
	}
	
	ReadHuman(e, ReadNpcBrainUnit)
}

entity_readers["PLAYER"] = function(e)
{
	function ReadPlayerBrainUnit(e)
	{
		ReadHumanBrainUnit(e, [])
	}
	
	ReadCPlayer(e, ReadPlayerBrainUnit)
}

entity_readers["RAT"] = function(e)
{
	ReadBaseNpc(e, ReadBaseBrainUnit)
}

entity_readers["SIMPLE_MONSTER"] = function(e)
{
	function ReadBrain(e)
	{
		ReadMonsterBrainUnit(e, [])
	}
	
	ReadBaseNpc(e, ReadBrain)
}

entity_readers["WATCHMAN"] = function(e)
{
	function ReadBrain(e)
	{
		var attacks = [
			"melee_attack_360_arm",
			"melee_attack_360_jaw",
			"melee_attack_360_ahalay",
			"melee_attack_fwd_short",
			"melee_attack_fwd_m_short",
			"watch_melee_attack_1",
			"melee_attack_fwd_m_long",
			"melee_attack_360_jump",
			"watch_melee_attack_4",
			"watch_melee_attack_5",
			"watch_melee_attack_6",
			"watch_melee_attack_7"
		]
		
		ReadMonsterBrainUnit(e, attacks)
	}
	
	ReadBaseNpc(e, ReadBrain)
}

entity_readers["WOMAN"] = function(e)
{
	ReadBaseNpc(e, ReadBaseBrainUnit)
}

/*--------------------------------*/
/*	Inventory Items & Devices			*/
/*--------------------------------*/
function ReadInventoryItemObject(e)
{
	e.ReadU8("item_place")
	e.ReadBool("active")
	e.ReadBool("useful_for_player")
	e.ReadBool("ammo_for_player")
	
	ReadCEntity(e)
}

function ReadDevice(e)
{
	ReadInventoryItemObject(e)

	e.ReadBool("silent_mode")
	e.ReadU8("state")
	e.ReadU8("target_state")
	e.ReadBool("activated_in_hud")
}

function ReadAmmo(e)
{
	ReadInventoryItemObject(e)
	
	e.ReadU16("box_min")
	e.ReadU16("box_max")
	e.ReadU16("box_value")
}

entity_readers["AMMO"] = function(e)
{
	ReadAmmo(e)
}

entity_readers["CHARGER"] = function(e)
{
	ReadDevice(e)
}

entity_readers["FILTER"] = function(e)
{
	ReadDevice(e)
	
	e.ReadFP32("timer")
	e.ReadFP32("spent_time")
}

entity_readers["GASMASK"] = function(e)
{
	ReadDevice(e)
	
	e.ReadFP32("current_time")
	e.ReadFP32("total_time")
	e.ReadFP32("hit_force")
	e.ReadU16("current_filter", "entity_link, uobject_link")
}

entity_readers["HELSING_ARROW"] = function(e)
{
	ReadAmmo(e)
}

entity_readers["MEDKIT"] = function(e)
{
	ReadDevice(e)
	
	e.ReadU32("ampulas_num")
}

entity_readers["NIGHTVISION"] = function(e)
{
	ReadDevice(e)
}

entity_readers["PLAYER_MAP"] = function(e)
{
	ReadInventoryItemObject(e)
	
	e.ReadString("cur_milestone")
	e.ReadString("cur_map")
	e.ReadString("cur_task")
}

/*--------------------------------*/
/*					Weapons								*/
/*--------------------------------*/
function ReadDynamite(e)
{
	ReadInventoryItemObject(e)

	e.ReadU32("delay")
	e.ReadFP32("wallmark_size")
	e.ReadHintStr("explode_particles", "particles, str_shared")
	e.ReadHintStr("sound_explode", "sound")
	e.ReadHintStr("camera_effect_name", "camera_track, str_shared")
}

function ReadWeapon(e)
{
	ReadInventoryItemObject(e)
	
	e.ReadU32("ammo_loaded")
	e.ReadU32("ammo_balance")
}

entity_readers["WEAPON_2012"] = function(e)
{
	ReadWeapon(e)
	e.ReadBool("golden_loot")
}

entity_readers["WEAPON_ABZAC"] = ReadWeapon

entity_readers["WEAPON_AK74_TEST"] = function(e)
{
	ReadWeapon(e)
	e.ReadBool("golden_loot")
	e.ReadBool("optika")
}

entity_readers["WEAPON_AK_74"] = function(e)
{
	ReadWeapon(e)
	e.ReadBool("golden_loot")
	e.ReadBool("optika")
}

entity_readers["WEAPON_DAGGER"] = function(e) 
{ 
	ReadInventoryItemObject(e)
}

entity_readers["WEAPON_DUPLET"] = function(e)
{
	ReadWeapon(e)
}

entity_readers["WEAPON_DYNAMITE"] = function(e)
{
	ReadDynamite(e)
}

entity_readers["WEAPON_FLASH_GRENADE"] = ReadDynamite

entity_readers["WEAPON_HELLBREATH"] = function(e)
{
	ReadWeapon(e)
	
	e.ReadFP32("pressure")
}

entity_readers["WEAPON_HELSING"] = function(e)
{
	ReadWeapon(e)
	
	e.ReadFP32("pressure")
	e.ReadBool("optika")
	e.ReadBool("lazer")
}

entity_readers["WEAPON_MACHETA"] = function(e) 
{ 
	ReadInventoryItemObject(e)
}

entity_readers["WEAPON_REVOLVER"] = function(e)
{
	ReadWeapon(e)
}

entity_readers["WEAPON_STICKY_DYNAMITE"] = function(e)
{
	ReadDynamite(e)
}

entity_readers["WEAPON_TIHAR"] = function(e)
{
	ReadWeapon(e)
	
	e.ReadFP32("pressure")
	e.ReadBool("optika")
}

entity_readers["WEAPON_UBLUDOK"] = function(e)
{
	ReadWeapon(e)
	e.ReadBool("golden_loot")
	e.ReadFP32("temperature")
	e.ReadFP32("temperature_old")
	e.ReadBool("jammed")
}

entity_readers["WEAPON_UBOYNICHEG"] = function(e)
{
	ReadWeapon(e)
	e.ReadBool("bayonet")
}

entity_readers["WEAPON_VSV"] = function(e)
{
	ReadWeapon(e)
	e.ReadBool("golden_loot")
}

/*--------------------------------*/
/*					Vehicles							*/
/*--------------------------------*/
function ReadDrezina(e)
{
	ReadCEntity(e)
	e.ReadString("locator_forward")
	e.ReadBool("locator_reverse")
}

function ReadKulemet(e)
{
	ReadDrezina(e)
	e.ReadFP32("inertion_mouse")
	e.ReadFP32("inertion_kulemet")
	e.ReadFP32("inertion_fov_factor")
	e.ReadHintStr("bone_hpivot", "locator_id")
	e.ReadHintStr("bone_vpivot", "locator_id")
	e.ReadFP32("vpivot_height")
	e.ReadFP32("yaw")
	e.ReadFP32("pitch")
	e.ReadFP32("yaw_max")
	e.ReadString("locator_firepoint")
	e.ReadU32("ammo_num")
	e.ReadU32("ammo_visible")
	e.ReadBool("show_ammo")
	e.ReadFP32("dispersion_base")
	e.ReadFP32("dispersion_aim")
	e.ReadFP32("dispersion_move_factor")
	e.ReadFP32("dispersion_inc")
	e.ReadFP32("dispersion_min")
	e.ReadFP32("dispersion_max")
	e.ReadFP32("dispersion_dec")
	e.ReadFP32("hit_power");
	e.ReadFP32("hit_factor_player");
	e.ReadFP32("hit_impulse");
	e.ReadFP32("fire_distance");
	e.ReadFP32("bullet_speed")
	e.ReadString("bone_patron_prefix")
	e.ReadHintStr("track_aim_in", "camera_track, str_shared")
	e.ReadHintStr("track_aim_idle", "camera_track, str_shared")
	e.ReadHintStr("track_aim_out", "camera_track, str_shared")
	e.ReadU16("gauge", "entity_link, uobject_link")
	e.ReadFP32("heat_boost")
	e.ReadFP32("heat_fade")
	e.ReadU32("npc_fire_burst_min")
	e.ReadU32("npc_fire_burst_max")
	e.ReadFP32("npc_fire_pause_min")
	e.ReadFP32("npc_fire_pause_max")
	e.ReadFP32("target_feedforward_time")
	e.ReadFP32("cannot_fire_distance")
	e.ReadU32("cannot_fire_time")	
}

function ReadStationStand(e)
{
	ReadDrezina(e)
	
	e.ReadFP32("yaw")
	e.ReadFP32("rotation_speed")
	e.ReadU32("acceleration_time")
}

entity_readers["DREZINA_HAND"] = ReadDrezina
entity_readers["DREZINA_MOTO"] = ReadDrezina
entity_readers["KULEMET"] = ReadKulemet
entity_readers["STATION_STAND"] = ReadStationStand

function ReadEntities(array)
{
	var already_listed = new Object
	
	while(array.More())
	{
		var e = array.ReadSection()
		
		e.ReadU16("version")
		var _class = e.ReadString("class")
		var _static_data_key = e.ReadString("static_data_key")
		e.ReadU16("id")
		e.ReadU16("parent_id")
		e.ReadHintStr("att_bone_id", "locator_str")
		e.ReadMatrix44("att_offset", "pose, matrix")
		
		//print(_class, " ", _static_data_key);
		
		if(entity_readers[_class])
		{
			entity_readers[_class](e);
	
			if(e.More())
				print(_class, " data left");
		}
		else
		{
			if(already_listed[_class] != true)
			{
				print("not implemented class ", _class);
				already_listed[_class] = true;
			}
		}
	}
}

// Main
var entities = reader.TryReadArray("entities")
if(entities) // level.add.bin
{
	ReadEntities(entities);
}
else // ordinary level.bin
{
	ReadUCovers(reader.ReadArray("ucovers"));
	ReadUGroups(reader.ReadArray("ugroups"));
	ReadULinks(reader.ReadArray("ulinks"));
	ReadULinks_PE(reader.ReadArray("ulinks_pe"));
	reader.ReadFP32Array("pe_dist_coefs");
	reader.ReadU8Array("pe_dist_infos");
	ReadStartup(reader.ReadSection("startup"));
	ReadEntities(reader.ReadArray("entities"));
}
