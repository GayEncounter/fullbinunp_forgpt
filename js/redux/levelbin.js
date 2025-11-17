// interface
this.ReadStartup = ReadStartup;
this.ReadEntities = ReadEntities;

// implementation
var typed_strings = module("ll\\typed_strings");
var visualscript = module("redux\\visualscript");

function ReadStartup(s)
{
	s.ReadString("desc_0")
	s.ReadString("desc_1")
	s.ReadS32("desc_time_0")
	s.ReadS32("desc_time_1")
	s.ReadFP32("weight")
	s.ReadString("next_desc")
	s.ReadS32("next_time")
	s.ReadHintStr("color_cube0", "choose")
	s.ReadHintStr("color_cube1", "choose")
	s.ReadS32("color_cube_time0")
	s.ReadS32("color_cube_time1")
	s.ReadHintStr("track", "choose")
	s.ReadHintStr("envmap", "choose")
	s.ReadVec4("envmap_factor", "color, vec4f")
	s.ReadHintStr("color_cube", "choose")
	s.ReadFP32("dao_base")
	s.ReadFP32("view_distance")
	s.ReadHintStr("next_level", "choose")
	s.ReadHintStr("back_music", "choose")
	
	var k = 0, weapon_items_link = s.ReadArray("weapon_items_link")
	while(weapon_items_link.More())
	{
		var w = weapon_items_link.ReadSection(RecStr("rec_", k++, 4), false)
		w.ReadU16("items", "entity_link, uobject_link")
	}
	
	s.ReadU8("bought_filters_count")
}

var entity_readers = {
	// basic
	"STATICPROP"                : ReadUObject_Static,
 	"STATICPROP_BREAKABLE"      : ReadUObject_StaticBreakable,
	"EFFECT"                    : ReadUObject_Effect,
	"EFFECTM"                   : ReadUObject_Effect,
	"O_ENTITY"                  : ReadCEntity,
	"o_hlamp"                   : ReadHangingLamp,
	"O_AIPOINT"                 : ReadUObject_AIPoint,
	"PATROL_POINT"              : ReadPatrolPoint,
	"VISUALSCRIPT"              : ReadUObject,
	"O_BASEZONE"                : ReadUObject_Zone,
	"O_WATERZONE"               : ReadWaterZone,
	"ANIM_OBJECT"               : ReadUObject_Effect,
	"PROXY"                     : ReadUObject_Proxy,
	"SOFT_ENTITY"               : ReadSoftEntity,
	"O_INTEREST"                : function(e)
	{
		ReadUObject(e);
		ReadInterest(e);
	},
	"O_EXPLOSION"               : ReadUObject_Explosion,
	"FORCE_FIELD"               : ReadForceField,
	"LADDER"                    : ReadLadder,
	"BREAKABLE_ICE"             : ReadBreakableIce,
	"WEB"                       : ReadWeb,
	"FLOWER"                    : ReadFlower,
	"LIAN"                      : ReadCEntity,
	"SIMPLE_NPC"                : ReadCEntity,
	"BIOMASS_LARGE"             : ReadCEntity,
	"BUSH"                      : ReadCEntity,
	"siege_bomb"                : ReadCEntity,
	"STRETCHY_MAN"              : function(e)
	{
		ReadCEntity(e);
		e.ReadU8("motion_coll");
	},
	
	"HANDS_FOR_DREZINA"         : ReadUObject_Effect,
	"PLAYER_TIMER"              : ReadUObject_Effect,
	"PLAYER_TIMER_2033"         : ReadCEntity,
	"PLAYERS_HANDS"             : ReadUObject_Effect,
	"PLAYERS_KNIFE"             : ReadUObject_Effect,
	"TORCH"                     : ReadHangingLamp,
};

function ReadInterest(e)
{
	var i = e.ReadSection("interest");
	
	i.ReadU16("min_importance");
	i.ReadU16("max_importance");
	i.ReadU8("interest_type");
	i.ReadU16("duration");
	i.ReadFP32("speed");
	i.ReadFP32("distance");
}

function ReadVsRef(e, name)
{
	var n = e.ReadHintStr(name, "vs_ref, str_shared");
	var exist = e.ReadBool(name + "_dyn_state_exist");
	
	// что это означает и откуда берЄтс€??
	if(n === "vs\\npc\\npc_fonarik")
		e.ReadBool("freegun");
	
	if(exist)
	{
		print("!!!! VS REF dyn_state_exist not implemented")
	}
}

function ReadCommonsVs(e)
{
	var k = 0, arr = e.ReadArray("commons_vs");
	while(arr.More())
	{
		var vs = arr.ReadSection(RecStr("rec_", k++, 4), false);
		vs.ReadString("vs_name")
		vs.ReadBool("vs_debug")
		vs.ReadBool("vs_active")
		ReadVsRef(vs, "vs_ref")
	}
}

function ReadRemovedVs(e)
{
	var k = 0, arr = e.ReadArray("removed_vs");
	while(arr.More())
	{
		var vs = arr.ReadSection(RecStr("rec_", k++, 4), false);
		vs.ReadString("vs_name")
		vs.ReadBool("vs_debug")
		vs.ReadBool("vs_active")
		ReadVsRef(vs, "vs_ref")
	}
}

function ReadPhysicsShell(e)
{
	var p = e.ReadSection("physics_shell");
	p.ReadBool("breaking_blocked");
	var elements = p.ReadArray("elements");
	for(var i = 0; elements.More(); i++)
	{
		var l = elements.ReadSection(RecStr("rec_", i, 4), false);
		l.ReadU16("root_bid");
		l.ReadFP32("accumulated_impulse");
		l.ReadMatrix43("xform", "pose, matrix_43T");
		l.ReadVec3("velocity");
		l.ReadBool("nx_awake");
		var shapes = l.ReadArray("shapes");
		for(var j = 0; shapes.More(); j++)
		{
			var s = shapes.ReadSection(RecStr("rec_", j, 4), false);
			s.ReadU16("bid");
		}
	}
}

function ReadUObject(e)
{
	e.ReadName("name")
	e.ReadU8("oflags", "bool8")
	e.ReadU8("sflags", "bool8")
	e.ReadMatrix43("", "pose, matrix")
	e.ReadHintStr("visual", "ref_model")
	e.ReadU16("dao_val")
	e.ReadVec4("render_aux_val", "color, vec4f")
	visualscript.ReadVssVer6(e)
	e.ReadBool("vs_active")
	e.ReadU16("spatial_sector")
}

function ReadUObject_Static(e)
{
	ReadUObject(e)
		
	e.ReadU8("flags", "bool8")
	e.ReadU8("collision_group")
}

function ReadUObject_StaticBreakable(e)
{
	e.ReadFP32("health");
	
	ReadUObject_Static(e);
	
	e.ReadHintStr("die_model", "ref_model");
	e.ReadHintStr("die_sound", "choose");
	e.ReadU8("die_sound_type");
	e.ReadHintStr("die_particles", "particles, str_shared");
	e.ReadBool("die_particles_ignore_rotation");
	e.ReadBool("block_ai_los");
	ReadCommonsVs(e);
}

function ReadBreakableIce(e)
{
	ReadUObject_Static(e);
	
	e.ReadVec4("sphere");
	e.ReadMatrix43("crack_particle_offset", "pose, matrix_43T");
	e.ReadMatrix43("push_particle_offset", "pose, matrix_43T");
	e.ReadBool("cracked");
}

function ReadSoftEntity(e)
{
	ReadUObject(e);
	
//	if(entity_ver < 29) // build 2012-12-03
//		e.ReadBool("sleeping");
	e.ReadU16("collision_group");
	e.ReadBool("self_collision");
	e.ReadBool("disable_collision");
	e.ReadBool("apply_gravity");
	e.ReadBool("two_way_collision");
	e.ReadBool("auto_attach");
	e.ReadFP32("two_way_clg_factor");
	e.ReadFP32("attach_clg_factor");
	e.ReadBool("auto_constrain");
}

function ReadUObject_Effect(e)
{
	ReadUObject(e);
	
	e.ReadHintStr("startup_animation", "animation_str");
	e.ReadHintStr("bone_part", "part_id");
	e.ReadU16("start_frame");
	e.ReadFP32("speed");
	e.ReadU8("startup_animation_flags", "bool8");
	e.ReadU8("force_looped");
	e.ReadHintStr("sound", "sound");
	e.ReadU8("sound_volume", "fp32_q8");
	e.ReadU8("sound_filter");
	e.ReadHintStr("particles", "particles, str_shared");
	e.ReadU8("particle_flags", "bool8");
	ReadInterest(e);
	e.ReadStrArray16("labels");
	ReadCommonsVs(e);
	ReadRemovedVs(e);
}

function ReadCEntity(e)
{
	e.ReadFP32("health");
	e.ReadFP32("stamina");
	e.ReadU32("dying_mask");
	
	ReadUObject_Effect(e);
	
	e.ReadU8("add_physics_flags", "bool8");
	e.ReadU8("friend_type");
	e.ReadU8("reaction_type");
	e.ReadStrArray16("fixed_bones");
	e.ReadU8("physics_flags", "bool8");
	e.ReadFP32("break_impulse_threshold");
	//? e.ReadBool("no_collision");
	e.ReadU8("collisions_group");
	e.ReadHintStr("break_particles_break", "particles, str_shared");
	e.ReadHintStr("break_particles_death", "particles, str_shared");
	e.ReadHintStr("break_sound_death", "sound");
	e.ReadU32("ph_shell_model_src");
	e.ReadU32("ph_shell_skltn_src");
	e.ReadU32("ph_shell_skltn_bcount");
	var writed = e.ReadBool("ph_shell_writed");
	if(writed)
		ReadPhysicsShell(e);
	var k = 0, links = e.ReadArray("ph_links");
	while(links.More())
	{
		var link = links.ReadSection(RecStr("rec_", k++, 4), false);
		
		link.ReadU16("dst_obj", "entity_link, uobject_link");
		link.ReadU16("src_bone");
		link.ReadU16("dst_bone");
		link.ReadFP32("pref_dist");
	}
}

function ReadHangingLamp(e)
{
	ReadCEntity(e);
	
	e.ReadBool("initial_state");
	e.ReadU8("die_sound_type");
	e.ReadHintStr("die_sound", "choose");
	e.ReadHintStr("die_particle", "particles, str_shared");
	e.ReadHintStr("light_main_bone", "locator_id");
	e.ReadHintStr("dark_bone", "locator_id");	
	e.ReadVec4("color", "color, vec4f");
	e.ReadFP32("brightness");
	e.ReadHintStr("color_animator", "ref_coloranim");
	//? e.ReadBool("cast_shadow");
	//? e.ReadBool("is_spot");
	e.ReadU8("ltype");
	e.ReadU8("light_flags", "bool8");
	e.ReadBool("sync_color_to_aux");
	e.ReadHint("faces", "flags8");
	e.ReadU8("faces");
	e.ReadVec4("color_middle", "color, vec4f");
	e.ReadVec4("colo_back", "color, vec4f");
	e.ReadU8("scurve");
	e.ReadFP32("shadow_clarity");
	e.ReadFP32("range");
	e.ReadFP32("nearplane");
	e.ReadFP32("spot_cone_angle", "angle, fp32");
	e.ReadHintStr("light_texture", "texture, str_shared");
	e.ReadBool("is_point_ambient");
	e.ReadHintStr("ambient_bone", "locator_id");
	e.ReadFP32("ambient_power");
	e.ReadFP32("ambient_radius");
	e.ReadHintStr("ambient_texture", "texture, str_shared");
	e.ReadBool("backlight");
	e.ReadU16("backlight_ref", "entity_link, uobject_link");
	e.ReadFP32("backlight_dist");
	e.ReadBool("backlight_dynamic");
	e.ReadHintStr("backlight_ray", "ref_model");
	e.ReadHintStr("backlight_ray_particles", "particles, str_shared");	
	e.ReadVec3("ellipse_scale");
	e.ReadVec2("quad_sizes");
	e.ReadU16("master", "entity_link, uobject_link");	
	e.ReadHintStr("flares", "choose");
	e.ReadHintStr("flares_bone", "locator_id");
	e.ReadU8("flares_axis");
}

function ReadWeb(e)
{
	e.ReadHintStr("subent", "ref_model");
	
	ReadCEntity(e);
	
	e.ReadMatrix43("subent_offset", "pose, matrix_43T");
	e.ReadHintStr("subent_attp", "locator_id");
	e.ReadHintStr("burn_particle_name", "particles, str_shared");
	e.ReadHintStr("burn_particle_locator", "locator_id");
	e.ReadMatrix43("burn_particle_offset", "pose, matrix_43T");
	e.ReadHintStr("burn_hud_particle_name", "particles, str_shared");
	e.ReadU8("burn_hud_particle_type");
	e.ReadVec4("sphere");
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

function ReadUObject_AIPoint(e)
{
	ReadUObject(e);
	
	for(var i = 0 ; i < 4; i++)
	{
		var l = e.ReadSection("link_"+i);
		l.ReadU16("object", "entity_link, uobject_link");
		l.ReadFP32("weight");
	}
	
	e.ReadU8("ai_map", "bool8");
}

function ReadPatrolPoint(e)
{
	ReadUObject_AIPoint(e);
	
	e.ReadU32("min_wait_time");
	e.ReadU32("max_wait_time");
	
	// patrol state
	e.ReadString("body_state");
	e.ReadString("anim_state");
	e.ReadString("movement_type");
	e.ReadString("weapon_state");
	e.ReadString("action");
	e.ReadU16("target", "entity_link, uobject_link");
	e.ReadHint("flags", "flags32");
	e.ReadU32("flags");
	e.ReadFP32("anim_state_approach_speed");
	e.ReadFP32("approaching_accel");
}

function ReadUObject_Restrictor(e)
{
	ReadUObject(e);
	
	e.ReadString("label");
	var k = 0, shapes = e.ReadArray("shapes");
	while(shapes.More())
	{
		var shape = shapes.ReadSection(RecStr("shape_", k++, 2), false);
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
				b.ReadMatrix43("", "pose, matrix");
				b.ReadVec3("h_size");
			break;
			
			default:
				print("unknown shape type ", type);
		}
	}
	
	e.ReadU8("flags0", "bool8");
	e.ReadHintStr("step_gmtrl", "material, str_shared");
	e.ReadU8("dynamic_mode");
}

function ReadUObject_Zone(e)
{
	ReadUObject_Restrictor(e);
	e.ReadU8("zone_type");	
}

function ReadWaterZone(e)
{
	ReadUObject_Zone(e);

	e.ReadHintStr("texture", "texture, str_shared");
	e.ReadHintStr("mask", "texture, str_shared");
	e.ReadFP32("angle", "angle, fp32");
	e.ReadVec4("color", "color, vec4f");
	e.ReadFP32("opacity_near");
	e.ReadFP32("opacity_far");
	e.ReadFP32("cubemap_opacity");
	e.ReadFP32("transparency_depth");
	e.ReadFP32("fresnel");
	e.ReadFP32("density");
	e.ReadFP32("tile_x");
	e.ReadFP32("tile_y");
	e.ReadFP32("speed");
	e.ReadFP32("bump_intensity");
	e.ReadFP32("distort_intensity");
	e.ReadFP32("distort_distant_fade");
	e.ReadFP32("farplane");
	e.ReadFP32("rdist_lim");
	e.ReadBool("reflectgeom");
	e.ReadBool("reflectplayer");
	e.ReadBool("sunshadow");
	e.ReadBool("envlit");
	e.ReadHintStr("material", "material, str_shared");
}

function ReadLadder(e)
{
	ReadUObject_Restrictor(e);
	
	e.ReadFP32("threshold_offset");
	e.ReadU8("detach_side");
	e.ReadFP32("detach_distance");
	e.ReadFP32("detach_speed");
	e.ReadBool("enabled");
	e.ReadBool("custom_movement");
	e.ReadBool("custom_civil_mode");
	e.ReadFP32("up_angle", "angle, fp32");
	e.ReadFP32("down_angle", "angle, fp32");
}

function ReadUObject_Proxy(e)
{
	ReadUObject(e);
	
	var k = 0, entities = e.ReadArray("entities");
	while(entities.More())
	{
		var e = entities.ReadSection(RecStr("rec_", k++, 4), false);
		e.ReadU16("entity", "entity_link, uobject_link");
	}
}

function ReadForceField(e)
{
	ReadUObject(e);
	
	var volumes = e.ReadArray("volumes");
	for(var i = 0; volumes.More(); i++)
	{
		var v = volumes.ReadSection(RecStr("volume_", i, 2), false);
		var type = v.ReadU32("type");
		
		switch(type)
		{
			case 0:
				var s = v.ReadSection("sphere");
				s.ReadVec3("center");
				s.ReadFP32("radius");
			break;
			case 1:
				var b = v.ReadSection("box");
				b.ReadMatrix43("", "pose, matrix");
				b.ReadVec3("h_size");
			break;
			case 2:
				var c = v.ReadSection("capsule");
				c.ReadVec3("center");
				c.ReadFP32("radius");
				c.ReadFP32("height");
			break;
			default:
				print("unknown volume type " + type);
		}
	}
	
	e.ReadBool("active");
	e.ReadU8("type");
	for(var j = 0; j < 9; j++)
		e.ReadFP32("param_"+j);
	e.ReadU8("flags0", "bool8");
	e.ReadVec3("rotation_power");
	e.ReadU8("anim_type");
}

function ReadUObject_Explosion(e)
{
	ReadUObject(e);
	e.ReadFP32("blast_radius_min");
	e.ReadFP32("blast_radius_max");
	e.ReadFP32("blast_hit_min");
	e.ReadFP32("blast_hit_max");
	e.ReadFP32("blast_up_throw_factor");
	e.ReadFP32("blast_hit_impulse_factor");
	e.ReadU8("blast_hit_type");
	e.ReadU8("blast_raycast_type");
	e.ReadU32("delay");
	e.ReadU32("flame_time");
	e.ReadU32("flame_interval");
	e.ReadU32("frags_num");
	e.ReadFP32("frags_radius");
	e.ReadFP32("frag_hit");
	e.ReadFP32("frag_hit_impulse");
	e.ReadFP32("frag_speed");
	
	var s = e.ReadSection("frag_ammo");
	s.ReadFP32("k_power");
	s.ReadFP32("k_impulse");
	s.ReadFP32("k_power_falloff");
	s.ReadFP32("k_impulse_falloff");
	s.ReadFP32("k_speed");
	s.ReadFP32("k_head_coef");
	s.ReadFP32("k_fire_distance_min");
	s.ReadFP32("k_fire_distance_max");
	s.ReadFP32("k_file_distance");
	s.ReadFP32("k_pierce");
	s.ReadFP32("k_tracer_scale_xy");
	s.ReadFP32("k_tracer_scale_z");
	s.ReadFP32("k_tracer_min_dist");
	s.ReadString("k_material");
	s.ReadBool("k_shoot_through");
	s.ReadFP32("blast_radius_min");
	s.ReadFP32("blast_radius_max");
	s.ReadFP32("blast_hit_min");
	s.ReadFP32("blast_hit_max");
	s.ReadFP32("blast_up_throw_factor");
	s.ReadFP32("blast_hit_impulse_factor");
	s.ReadU8("blast_hit_type");
	s.ReadU8("blast_raycast_type");
	s.ReadHintStr("tracer_mesh", "ref_model");
	s.ReadHintStr("tracer_mesh_hud", "ref_model");
	s.ReadHintStr("bullet_particles", "particles, str_shared");
	s.ReadHintStr("impact_particles", "particles, str_shared");
	s.ReadHintStr("trail_mesh", "ref_model");
	s.ReadU32("trail_time");
	s.ReadVec2("trail_beg_scale_xy");
	s.ReadVec2("trail_end_scale_xy");
	s.ReadFP32("wm_size");
	s.ReadFP32("k_monster_coef");
	s.ReadString("ammo_material");
	s.ReadU8("flags0", "bool8");
	s.ReadFP32("k_disp");
	s.ReadS32("buck_shot");
	s.ReadU32("type");
	
	e.ReadU8("hit_frag");
	e.ReadVec4("light_color", "color, vec4f");
	e.ReadFP32("light_range");
	e.ReadU32("light_time_max");
	e.ReadU32("explode_duration_max");
	e.ReadString("label");
}

/*--------------------------------*/
/*						NPCs								*/
/*--------------------------------*/
function ReadBaseBrainUnit(e)
{
	// base_brain_unit
	e.ReadU8("anim_state");
	e.ReadU8("movement_type");
	e.ReadU8("min_movement_type");
	e.ReadU8("max_movement_type");
	e.ReadU8("body_state");
	e.ReadFP32("step_acceleration");
	e.ReadString("group");
	e.ReadU8("flags0", "bool8");
	
	// base_visual_memory_manager
	// отличи€ от LL:
	// параметры min_lum_threshold, min_threshold теперь перед параметром threat_threshold
	// добавлены параметры lum_min_distance, surv_threat_threshold, surv_vis_threshold, surv_lum_min_distance перед параметром transition_time
	e.ReadBool("vision_enabled");
	e.ReadFP32("free_vision_range");
	e.ReadFP32("free_vision_bwd_range");
	e.ReadFP32("free_vision_fov", "angle, fp32");
	e.ReadU32("free_vision_min_update_time");
	e.ReadFP32("free_vision_luminocity_fac");
	e.ReadFP32("free_vision_luminocity_factor");
	e.ReadFP32("free_vision_velocity_fac");
	e.ReadFP32("free_vision_decrease_value");
	e.ReadFP32("free_vision_min_lum_threshold");
	e.ReadFP32("free_vision_min_threshold");
	e.ReadFP32("free_vision_threat_threshold");
	e.ReadFP32("free_vision_light_alert_threshold");
	e.ReadFP32("free_vision_vis_threshold");
	e.ReadFP32("free_vision_lum_min_distance");
	e.ReadFP32("free_vision_surv_threat_threshold");
	e.ReadFP32("free_vision_surv_light_alert_threshold");
	e.ReadFP32("free_vision_surv_vis_threshold");
	e.ReadFP32("free_vision_surv_lum_min_distance");
	e.ReadU32("free_vision_transition_time");
	
	e.ReadFP32("alert_vision_range");
	e.ReadFP32("alert_vision_bwd_range");
	e.ReadFP32("alert_vision_fov", "angle, fp32");
	e.ReadU32("alert_vision_min_update_time");
	e.ReadFP32("alert_vision_luminocity_fac");
	e.ReadFP32("alert_vision_luminocity_factor");
	e.ReadFP32("alert_vision_velocity_fac");
	e.ReadFP32("alert_vision_descrease_value");
	e.ReadFP32("alert_vision_min_lum_threshold");
	e.ReadFP32("alert_vision_min_threshold");
	e.ReadFP32("alert_vision_threat_threshold");
	e.ReadFP32("alert_vision_light_alert_threshold");
	e.ReadFP32("alert_vision_vis_threshold");
	e.ReadFP32("alert_vision_lum_min_distance");
	e.ReadFP32("alert_vision_surv_threat_threshold");
	e.ReadFP32("alert_vision_surv_light_alert_threshold");
	e.ReadFP32("alert_vision_surv_vis_threshold");
	e.ReadFP32("alert_vision_surv_lum_min_distance");
	e.ReadFP32("alert_vision_transition_time");
	
	e.ReadFP32("danger_vision_range");
	e.ReadFP32("danger_vision_bwd_range");
	e.ReadFP32("danger_vision_fov", "angle, fp32");
	e.ReadU32("danger_vision_min_update_time");
	e.ReadFP32("danger_vision_luminocity_fac");
	e.ReadFP32("danger_vision_luminocity_factor");
	e.ReadFP32("danger_vision_velocity_fac");
	e.ReadFP32("danger_vision_descrease_value");
	e.ReadFP32("danger_vision_min_lum_threshold");
	e.ReadFP32("danger_vision_min_threshold");
	e.ReadFP32("danger_vision_threat_threshold");
	e.ReadFP32("danger_vision_light_alert_threshold");
	e.ReadFP32("danger_vision_vis_threshold");
	e.ReadFP32("danger_vision_lum_min_distance");
	e.ReadFP32("danger_vision_surv_threat_threshold");
	e.ReadFP32("danger_vision_surv_light_alert_threshold");
	e.ReadFP32("danger_vision_surv_vis_threshold");
	e.ReadFP32("danger_vision_surv_lum_min_distance");
	e.ReadFP32("danger_vision_transition_time");
	
	// base_sound_memory_manager
	// отличий от ЋЋ нет
	e.ReadBool("sound_enabled");
	e.ReadFP32("free_disturb_threshold");
	e.ReadFP32("free_light_alert_threshold");
	e.ReadFP32("free_alert_threshold");
	e.ReadFP32("free_danger_threshold");
	e.ReadFP32("alert_disturb_threshold");
	e.ReadFP32("alert_light_alert_threshold");
	e.ReadFP32("alert_alert_threshold");
	e.ReadFP32("alert_danger_threshold");
	e.ReadFP32("danger_disturb_threshold");
	e.ReadFP32("danger_light_alert_threshold");
	e.ReadFP32("danger_alert_threshold");
	e.ReadFP32("danger_danger_threshold");	
	
	// base_threat_memory_manager
	// отличи€ от LL:
	// добавлены параметры survival_vision_time, survival_vision_period перед sound_count
	e.ReadU32("la_free_interia_time");
	e.ReadU32("max_sounds_count");
	e.ReadU32("la_alert_time");
	e.ReadU32("vision_time");
	e.ReadU32("vision_period");
	e.ReadU32("survival_vision_time");
	e.ReadU32("survival_vision_period");
	e.ReadU32("sound_count");
	e.ReadU32("sound_period");
	e.ReadU32("a_free_inertia_time");
	
	// base_enemy_memory_manager
	// отличий от ЋЋ нет
	e.ReadU32("post_enemy_wait_interval_static");
	e.ReadU32("post_enemy_wait_interval_random");
	e.ReadU32("enemy_inertia_time");
	e.ReadU32("enemy_remember_time");
	
	// base_sound_player
	// отличий от ЋЋ нет
	e.ReadHintStr("sound_scheme", "choose");
	
	// again base_brain_unit
	e.ReadFP32("max_heading", "angle, fp32");
	e.ReadFP32("max_pitch", "angle, fp32");
}

function ReadAIBrainUnit(e, attacks_list)
{
	ReadBaseBrainUnit(e);
	
	e.ReadU8("min_anim_state");
	e.ReadU8("max_anim_state");
	var attacks = e.ReadSection("attacks");
	for(var i = 0; i < attacks_list.length; i++)
	{
		//var a = attacks.TryReadSection(attacks_list[i]);
		//if(!a)
		//{
		//	print("Warning! Cannot open section '" + attacks_list[i] + "' while loading attacks");
		//	print("(Note: for dlc_tower1 and dlc_faction_hanza levels this is fine)");
		//	continue;
		//}
		var a = attacks.ReadSection(attacks_list[i]);
		a.ReadU8("flags0", "bool8");
		a.ReadHintStr("vs_reference", "vs_ref, str_shared");
		a.ReadFP32("probability");
	}
	
	if(attacks.More())
		print("attack left ", attacks.ReadU32())
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

function ReadNpcBase(e, b_aqua, brain_unit_read_func)
{
	e.ReadBool("fully_dead");
	e.ReadU8("dying_from");
	
	ReadCEntity(e);
	
	e.ReadU8("base_npc_flags");
	e.ReadFP32("luminocity");
	e.ReadFP32("shoot_dispersion_coef");
	e.ReadFP32("hit_power_coef");
	e.ReadFP32("hit_impulse_coef");
	e.ReadS32("offmesh_penalty");
	e.ReadU32("offmesh_reuse_delay");
	e.ReadU32("offmesh_inter_reuse_delay");
	
	// virtual function base_npc::load_dynamic_dhps
	e.ReadU32("damage_handle_preset");
	if(b_aqua == true)
	{
		e.ReadU32("water_damage_handle_preset");
		e.ReadU32("climb_damage_handle_preset");
		e.ReadU32("spt_water_damage_handle_preset");
		e.ReadU32("spt_surface_damage_handle_preset");
	}
	
	ReadVsRef(e, "movement_hit_vs_ref");
	
	brain_unit_read_func(e);
	
	e.ReadBool("ph_dying");
}

function ReadSimpleNpc(e)
{
	ReadNpcBase(e, false, ReadBaseBrainUnit);
}

function ReadHuman(e, brain_unit_read_func)
{
	ReadNpcBase(e, false, brain_unit_read_func);
	
	e.ReadU16("active_item")
	e.ReadU16("close_ranged_weapon", "entity_link, uobject_link")
	e.ReadU16("ranged_weapon", "entity_link, uobject_link")
	e.ReadFP32("close_ranged_weapon_distance")
	e.ReadFP32("ranged_weapon_distance")
	e.ReadU32("dispersion_decrease_time");
	e.ReadU32("dispersion_increate_time");
	e.ReadFP32("min_shoot_dispersion_coef");
	e.ReadFP32("max_shoot_dispersion_coef");
	e.ReadU8("human_flags", "bool8")
	e.ReadBool("auto_reload_disabled")
	e.ReadHintStr("override_voice", "choose");
	e.ReadU32("min_corpse_lum_update_interval");
	e.ReadU32("max_corpse_lum_update_interval");
	e.ReadString("allowed_speech_groups");

	var property_data = e.ReadArray("property_data");
	for(var i = 0; property_data.More(); i++)
	{
		var r = property_data.ReadSection(RecStr("rec_", i, 4), false);
		r.ReadString("name");
		r.ReadS32("value");
	}
}

function ReadPlayer(e)
{
	function ReadBrainUnit(e)
	{
		ReadAIBrainUnit(e, []);
	}

	e.ReadBool("reviving");
	
	ReadHuman(e, ReadBrainUnit);
	
	// class cplayer
	var arms = e.ReadSection("arms");
	arms.ReadHintStr("suit_visual", "ref_model");
	var timer = e.ReadSection("timer"); // empty section
	var sect = e.ReadSection("timer_2033"); // empty section
	e.ReadBool("tired_for_sprint");
	e.ReadU16("players_hands", "entity_link, uobject_link");
	e.ReadU16("players_knife", "entity_link, uobject_link");
	e.ReadU16("players_torch", "entity_link, uobject_link");
	e.ReadU16("players_heap", "entity_link, uobject_link");
	e.ReadU16("players_timer", "entity_link, uobject_link");
	e.ReadU16("players_timer_2033", "entity_link, uobject_link");
	e.ReadFP32("suit_luminocity");
	e.ReadU32("mp_class_type");
	e.ReadFP32("def_restore_rate");
	e.ReadStrArray16("private_data");
	var achievement_data = e.ReadArray("achievement_data");
	for(var i = 0; achievement_data.More(); i++)
	{
		var r = achievement_data.ReadSection(RecStr("rec_", i, 4), false);
		r.ReadString("name");
		r.ReadS32("value");
		r.ReadS32("border");
	}
	var points_data = e.ReadArray("points_data");
	for(var i = 0; points_data.More(); i++)
	{
		var r = points_data.ReadSection(RecStr("rec_", i, 4), false);
		r.ReadS32("points");
		r.ReadString("comments");
	}
	e.ReadBool("spend_filters");
	e.ReadFP32("money_loot_cent");
	e.ReadU32("min_loot_money");
	e.ReadFP32("lean_height_to_wall");
	e.ReadFP32("lean_width_from_corner");
	e.ReadFP32("lean_player_speed_border");
	e.ReadFP32("lean_corner_fix_step");
	e.ReadFP32("lean_wall_out_unlock_angle");
	e.ReadFP32("lean_wall_in_unlock_angle");
	e.ReadFP32("lean_add_height_to_wall");
	e.ReadFP32("lean_add_width_from_corner");
	e.ReadFP32("lean_max_head_angle");
	e.ReadFP32("lean_lock_angle");
	e.ReadHintStr("lean_head_left_camera_track", "camera_track, str_shared");
	e.ReadHintStr("lean_head_right_camera_track", "camera_track, str_shared");
	e.ReadBool("ignore_factor");
	e.ReadBool("use_sqr");
	e.ReadBool("check_static");
	var arr = e.ReadArray("seen_weapon_items");
	for(var i = 0; arr.More(); i++)
	{
		var r = arr.ReadSection(RecStr("rec_", i, 4), false);
		r.ReadU32("weapon_id");
		r.ReadU32("weapon_items_mask");
	}
}

function ReadNpc(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
				"melee_attack_360_butt", 
				"melee_attack_360_kick",
				"melee_attack_360_bzyk"
			]; 
			
		ReadAIBrainUnit(e, attacks_list);
		
		// class human_brain_unit
		e.ReadU8("flags", "bool8");
		e.ReadFP32("suppress_fire_min_dist");
		e.ReadFP32("suppress_fire_timeout");
		e.ReadFP32("close_distance");
		e.ReadU32("close_min_queue_size");
		e.ReadU32("close_max_queue_size");
		e.ReadU32("close_min_queue_interval");
		e.ReadU32("close_max_queue_interval");	
		e.ReadU32("min_queue_size");
		e.ReadU32("max_queue_size");
		e.ReadU32("min_queue_interval");
		e.ReadU32("max_queue_interval");	
		e.ReadU32("behaviour_type");
		var static_combat = e.ReadSection("static_combat");
		static_combat.ReadBool("hold_position");
		static_combat.ReadU16("anchor", "entity_link, uobject_link");
		static_combat.ReadFP32("radius");
		static_combat.ReadFP32("enemy_dist");
		static_combat.ReadBool("point_valid");
		static_combat.ReadVec3("point_pos");
		static_combat.ReadVec3("point_dir");
		e.ReadHintStr("static_idle", "animation_str");
		e.ReadHintStr("static_attack", "animation_str");
		e.ReadHintStr("static_reload_idle", "animation_str");
		e.ReadHintStr("static_shoot", "animation_str");
		var cc = e.ReadSection("common_combat");
		cc.ReadU8("flags", "bool8");
		cc.ReadFP32("cover_min_dist");
		cc.ReadFP32("common_max_dist");
		cc.ReadFP32("min_cover_weight");
		cc.ReadFP32("wounded_max_dist");
		cc.ReadU32("timeout_min");
		cc.ReadU32("timeout_max");
		cc.ReadU32("timeout_min_see");
		cc.ReadU32("timeout_max_see");	
		cc.ReadFP32("weight_delta");
		cc.ReadFP32("fire_enemy_min");
		cc.ReadFP32("fire_enemy_max");
		if(entity_ver >= 29) cc.ReadU8("combat_type");
		var in_cover = cc.ReadSection("in_cover");
		in_cover.ReadU32("enemy_seen_timeout");
		in_cover.ReadU32("lookout_min");
		in_cover.ReadU32("lookout_max");
		in_cover.ReadU32("aim_shile_lookout_timeout");
		in_cover.ReadU32("lookout_cooldown_min");
		in_cover.ReadU32("lookout_cooldown_max");
		var common = cc.ReadSection("common");
		common.ReadFP32("radius_near");
		common.ReadFP32("radius_far");
		common.ReadU32("timeout");
		common.ReadU32("update_interval");
		common.ReadFP32("crouch_probability");
		common.ReadFP32("kneel_probability");
		common.ReadFP32("cover_probability");
		common.ReadBool("kneel_after_corner");
		common.ReadU32("timeout_goto_target");
		common.ReadU32("time_in_cover");
		common.ReadFP32("valid_cover_weight");
		common.ReadU32("kneel_interval");
		var alert = e.ReadSection("alert");
		alert.ReadBool("search_enabled");
		alert.ReadU32("timeout_go_search");
		alert.ReadU32("timeout_nervous_alert");
		alert.ReadU32("timeout_nervous_alert_uber");
		var scary = e.ReadSection("scary"); // empty section
	}
	
	ReadHuman(e, ReadBrainUnit);
}

entity_readers["AMEBA"] = function(e)
{
	function ReadBrain(e)
	{
		ReadMonsterBrainUnit(e, [])
	}
	
	ReadNpcBase(e, false, ReadBrain)
	
	//e.ReadU32("delay") // REMOVED in Redux!!
	//e.ReadFP32("wallmark_size") // REMOVED in Redux!!
	//e.ReadHintStr("explode_particles", "particles, str_shared") // REMOVED in Redux!!
	//e.ReadHintStr("sound_explode", "sound") // REMOVED in Redux!!
	//e.ReadHintStr("camera_effect_name", "camera_track, str_shared") // REMOVED in Redux!!
	e.ReadHintStr("suicide_particles", "particles, str_shared")
	e.ReadHintStr("death_particles", "particles, str_shared")
}

entity_readers["ANOMALY"] = function(e)
{
	function ReadBrain(e)
	{
		ReadAIBrainUnit(e, [])
	}
	
	ReadNpcBase(e, false, ReadBrain)
}

entity_readers["AQUA_FEMALE"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
			"melee_attack_0",
			"melee_attack_1",
			"melee_attack_face",
			"melee_attack_back"
		];
		
		ReadMonsterBrainUnit(e, attacks_list);
		
		e.ReadU8("allowed_state");
		e.ReadFP32("hunter_state_prob");
		e.ReadU8("am_state");
	}
	
	ReadNpcBase(e, true, ReadBrainUnit);
	
	e.ReadU16("head", "entity_link, uobject_link");
	e.ReadFP32("water_level");
}

entity_readers["AQUA_MALE_BIG"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
			"melee_attack_back_0",
			"melee_attack_back_1",
			"melee_attack_face_0",
			"melee_attack_face_1",
			"melee_attack_short_2",
			"melee_attack_short_3",
			"melee_attack_short_90l",
			"melee_attack_long_90l",
			"melee_attack_short_90r",
			"melee_attack_long_90r",
			"melee_attack_short_90_open"
		];
		ReadMonsterBrainUnit(e, attacks_list);
		
		// brain unit
		e.ReadU8("am_state");
		e.ReadBool("tired_closed");
	}

	ReadNpcBase(
		e, 
		false, // sic!
		ReadBrainUnit
	);
}

entity_readers["AQUA_MALE_SMALL"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
			"melee_attack_face_0",
			"melee_attack_back_0",
			"melee_attack_short_0",
			"melee_attack_short_1",
			"melee_attack_short_2",
			"melee_attack_short_3",
			"melee_attack_short_90l",
			"melee_attack_short_90r",
			"melee_attack_long_90l",
			"melee_attack_long_90r",
			"melee_attack_short_90bite",
			"melee_attack_long_90bite"
		];
		ReadMonsterBrainUnit(e, attacks_list);
		
		// brain unit
		e.ReadU8("am_state");
		e.ReadBool("tired_closed");
	}
	
	ReadNpcBase(e, true, ReadBrainUnit);
	
	e.ReadU16("head", "entity_link, uobject_link");
	e.ReadFP32("water_level");
}

entity_readers["ARAHIND"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
			"melee_attack_360", 
			"melee_attack_k1", 
			"melee_attack_k2", 
			"melee_attack_knife", 
			"melee_attack_knife_armour"
		]; 
		ReadMonsterBrainUnit(e, attacks_list);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["BIG_MOTHER"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
			"melee_attack_run",
			"melee_attack_360_arm"
		];
		ReadMonsterBrainUnit(e, attacks_list);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["DARK"] = function(e)
{
	function ReadBrainUnit(e)
	{
		ReadMonsterBrainUnit(e, []);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["DARKCHILD"] = ReadSimpleNpc

entity_readers["GRIZLY"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
			"melee_attack_0"
		];
		ReadMonsterBrainUnit(e, attacks_list);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["HARPY"] = function(e)
{
	function ReadBrainUnit(e)
	{
		ReadMonsterBrainUnit(e, []);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
	
	e.ReadBool("always_crack_ice");
}

entity_readers["KID"] = ReadSimpleNpc

entity_readers["LIBRARIAN"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
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
			"melee_attack_add5"
		]
		ReadMonsterBrainUnit(e, attacks_list)
		e.ReadFP32("librarian_hit_agression_time") // added in Redux
		e.ReadFP32("librarian_light_dmg_agression_time")
		e.ReadFP32("librarian_heavy_dmg_agression_time")
		e.ReadFP32("librarian_nearby_attack_agression_time")
		e.ReadFP32("librarion_not_look_agression_time")
		e.ReadU32("librarian_agression_radius")
		e.ReadFP32("librarian_attack_after_turn_probability")
		e.ReadBool("librarian_sprint_acceleration")
		e.ReadFP32("librarian_look_around_probability")
		e.ReadFP32("librarian_look_timeout_min")
		e.ReadFP32("librarian_look_timeout_max")
		e.ReadU32("librarian_look_radius_min")
		e.ReadU32("librarian_look_radius_max")
		e.ReadFP32("librarian_crit_hide_probability")
		e.ReadFP32("librarian_angry_idle_interval_min")
		e.ReadFP32("librarian_angle_idle_interval_max")
		e.ReadU32("librarian_look_eye_fov")
		e.ReadBool("librarian_hide_instead_escape")
		e.ReadFP32("librarian_wait_in_hide_time")
		e.ReadBool("librarian_return_if_enemy_on_map")
	}
	
	ReadNpcBase(e, false, ReadBrainUnit)
}

entity_readers["LURKER"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
			"melee_attack_360_jump",
			"melee_attack_360_jaw"
		];
		ReadMonsterBrainUnit(e, attacks_list);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["NOSALIS"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
			"melee_attack_360_jump",
			"melee_attack_8m",
			"melee_attack_0",
			"melee_attack_run_knife"
		];
		
		ReadMonsterBrainUnit(e, attacks_list);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["NOSALIS_FEMALE"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
			"melee_attack_0",
			"melee_attack_1",
			"melee_attack_2"
		];
		ReadMonsterBrainUnit(e, attacks_list);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["NPC_FX"] = ReadNpc
entity_readers["PLAYER"] = ReadPlayer
entity_readers["RAT"] = ReadSimpleNpc

entity_readers["SIMPLE_MONSTER"] = function(e)
{
	function ReadBrainUnit(e)
	{
		ReadMonsterBrainUnit(e, [])
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["WATCHMAN"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
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
		];
		
		ReadMonsterBrainUnit(e, attacks_list);
		
		// class watchman_brain_unit
		e.ReadU8("watchman_type");
	}

	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["WOMAN"] = ReadSimpleNpc
entity_readers["WOMAN_STRIP"] = ReadSimpleNpc


/*--------------------------------*/
/*	Inventory Items & Devices			*/
/*--------------------------------*/
function ReadInventoryItemObject(e, float_names, int_names)
{
	// class inventory_item
	e.ReadU8("item_place");
	e.ReadU8("flags0", "bool8"); // active, useful_for_player, ammo_for_player, dao_blink_prevent, ready_after_cloned, ui_force_slot, attached_loot
	e.ReadU16("trade_weight");
	
	// имена могут быть разными в зависимости от класса
	for(var i = 0; i < 6; i++)
	{
		var n;
		
		if(float_names && i < float_names.length)
			n = float_names[i];
		else
			n = "p"+(i+1)+"_fp32"; // default name
			
		e.ReadFP32(n);
	}
	
	for(var j = 0; j < 6; j++)
	{
		var n;
		
		if(int_names && j < int_names.length)
			n = int_names[j];
		else
			n = "p"+(j+1)+"_u32"; // default name
			
		e.ReadU32(n);
	}

	e.ReadHint("bp_flags_c", "flags32");
	e.ReadU32("bp_flags_c");
	e.ReadU8("ui_force_slot_id");

	ReadCEntity(e);
}

function ReadDevice(e, float_names, int_names)
{
	ReadInventoryItemObject(e, float_names, int_names)
	e.ReadBool("silent_mode")
}

function ReadHeap(e)
{
	ReadInventoryItemObject(e)
	
	e.ReadU16("count")
	
	e.ReadU32("ammo_loaded")
	e.ReadU32("dynamite")
	e.ReadU32("sdynamite")
	e.ReadU32("fdynamite")
	e.ReadU32("ldynamite")
	e.ReadU32("medkit")
	e.ReadU32("filter")
	e.ReadU32("knife")
	e.ReadU32("arrow")
	e.ReadU32("claymore")
	e.ReadU32("c4")
	e.ReadU8("heap_type")
	e.ReadU32("auto_delivery_timer")
	e.ReadBool("active_on_level")
	e.ReadU32("money_stor")
	e.ReadU32("killer_idx")
	e.ReadU32("victim_idx")
	e.ReadU32("money_prop_id")
	e.ReadU32("resp_prop_id")
	e.ReadVec4("sphere")
}

entity_readers["AMMO"] = function(e)
{
	ReadInventoryItemObject(e)
	e.ReadU16("box_min")
	e.ReadU16("box_max")
	e.ReadU16("box_value")
};

entity_readers["CHARGER"] = ReadDevice

entity_readers["FILTER"] = function(e)
{
	ReadDevice(e)
	e.ReadFP32("timer")
}

entity_readers["GASMASK"] = function(e)
{
	ReadDevice(e, ["current_time", "total_time", "hit_force", "vs_effect"])
	e.ReadU16("current_filter", "entity_link, uobject_link")
}

entity_readers["HEAP"] = ReadHeap

entity_readers["HELSING_ARROW"] = function(e)
{
	ReadInventoryItemObject(e)
	e.ReadU16("count")
}

entity_readers["LIGHTER"] = ReadDevice

entity_readers["MEDKIT"] = function(e)
{
	ReadDevice(e, [], ["ampulas_num"])
}

entity_readers["NIGHTVISION"] = ReadDevice

entity_readers["PLAYER_MAP"] = ReadInventoryItemObject

entity_readers["WEAPON_ITEM"] = ReadInventoryItemObject
entity_readers["WEAPON_ITEM_LASER"] = ReadInventoryItemObject

entity_readers["WEAPON_ITEM_NOOBEGUN"] = function(e)
{
	ReadInventoryItemObject(e)
	e.ReadU32("noobe_ammo_loaded")
}

entity_readers["WEAPON_ITEM_NOOBETUBE"] = function(e)
{
	ReadInventoryItemObject(e)
	e.ReadU32("noobe_ammo_loaded")
}

/*--------------------------------*/
/*					Weapons								*/
/*--------------------------------*/
function ReadWeaponBase(e)
{
	e.ReadString("dlc_model_name")
	e.ReadHintStr("dlc_hud_model_name", "ref_model")
	
	ReadInventoryItemObject(e, [], ["ammo_loaded", "ammo_balance", "ammunition"])

	e.ReadBool("ignore_difficulty")
}

function ReadDynamite(e)
{
	ReadWeaponBase(e);
	e.ReadU16("count");
	e.ReadU16("box_value");
}

function ReadWeapon(e)
{
	ReadWeaponBase(e)

	e.ReadBool("ad_scope")
	e.ReadBool("ad_kalimator")
	e.ReadBool("ad_envg")
	e.ReadBool("ad_silencer")
	e.ReadBool("ad_barrel")
	e.ReadBool("ad_barrel_2")
	e.ReadBool("ad_laser")
	e.ReadBool("ad_pivot")
	e.ReadBool("ad_butt_1")
	e.ReadBool("ad_butt_2")
	e.ReadBool("ad_magazine_1")
	e.ReadBool("ad_magazine_2")
	e.ReadBool("ad_noobtube")
	e.ReadBool("ad_noobgun")
	e.ReadU8("bullets_in_barrel")
}

entity_readers["WEAPON_2012"] = ReadWeapon
entity_readers["WEAPON_ABZAC"] = function(e)
{
	ReadWeapon(e)
	e.ReadBool("ad_compens")
	e.ReadBool("ad_autofire")
}
entity_readers["WEAPON_AK_74"] = ReadWeapon
entity_readers["WEAPON_AKSU"] = ReadWeapon
entity_readers["WEAPON_ASHOT"] = ReadWeapon
entity_readers["WEAPON_BIGUN"] = ReadWeapon
entity_readers["WEAPON_CLAYMORE"] = function(e)
{
	ReadDevice(e)
	
	e.ReadU16("count")
	e.ReadVec3("trigger_size")
	e.ReadMatrix43("trigger_offset", "pose, matrix_43T")
	e.ReadBool("active_on_level")
}
entity_readers["WEAPON_DAGGER"] = function(e)
{
	ReadWeaponBase(e)
	e.ReadU16("count")
}
entity_readers["WEAPON_DUPLET"] = function(e)
{
	ReadWeapon(e)
	e.ReadBool("ad_grip_base")
	e.ReadBool("ad_grip")
}
entity_readers["WEAPON_DYNAMITE"] = ReadDynamite
entity_readers["WEAPON_FLAME_DYNAMITE"] = ReadDynamite
entity_readers["WEAPON_FLAMETHROWER"] = function(e)
{
	ReadWeapon(e)
	e.ReadFP32("pressure")
	e.ReadBool("ad_engine")
	e.ReadBool("ad_engine_2")
	e.ReadBool("ad_stabilizer")
}
entity_readers["WEAPON_GATLING"] = function(e)
{
	ReadWeapon(e)
	e.ReadFP32("pressure")
	e.ReadBool("ad_engine")
	e.ReadBool("ad_engine_2")
	e.ReadBool("ad_stabilizer")
}
entity_readers["WEAPON_HELLBREATH"] = function(e)
{
	ReadWeapon(e)
	e.ReadFP32("pressure")
	e.ReadBool("ad_battery")
	e.ReadBool("ad_capatitor")
}
entity_readers["WEAPON_HELSING"] = function(e)
{
	ReadWeapon(e)
	e.ReadFP32("pressure")
	e.ReadBool("ad_tank")
	e.ReadBool("ad_tank_2")
}
entity_readers["WEAPON_LAUNCHER_TIME"] = ReadDynamite
entity_readers["WEAPON_MACHETA"] = ReadWeaponBase
entity_readers["WEAPON_MEDVED"] = function(e)
{
	ReadWeapon(e)
	var s = e.ReadSection("launcher")
	s.ReadU32("noobe_ammo_loaded")
	e.ReadBool("line")
}
entity_readers["WEAPON_PADONAG"] = function(e)
{
	ReadWeapon(e)
	e.ReadBool("ad_autofire")
}
entity_readers["WEAPON_PREVED"] = function(e)
{
	ReadWeapon(e)
	e.ReadBool("ad_flashhider")
}
entity_readers["WEAPON_REVOLVER"] = ReadWeapon
entity_readers["WEAPON_RPK"] = ReadWeapon
entity_readers["WEAPON_SAIGA"] = ReadWeapon
entity_readers["WEAPON_STICKY_DYNAMITE"] = ReadDynamite
entity_readers["WEAPON_TIHAR"] = function(e)
{
	ReadWeapon(e)
	e.ReadFP32("pressure")
	e.ReadBool("ad_tank")
	e.ReadBool("ad_tank_2")
}
entity_readers["WEAPON_UBLUDOK"] = function(e)
{
	ReadWeapon(e)
	e.ReadFP32("temperature")
	e.ReadFP32("temperature_old")
	e.ReadBool("ad_radiator")
}
entity_readers["WEAPON_UBOYNICHEG"] = ReadWeapon
entity_readers["WEAPON_VENTIL"] = function(e)
{
	ReadWeapon(e)
	e.ReadBool("ad_flashhider")
}
entity_readers["WEAPON_VSV"] = ReadWeapon
entity_readers["WEAPON_VYHLOP"] = function(e)
{
	ReadWeapon(e)
	e.ReadBool("ad_flashhider")
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
	ReadDrezina(e);
	e.ReadFP32("inertion_mouse");
	e.ReadFP32("inertion_kulemet");
	e.ReadFP32("inertion_fov_factor");
	e.ReadHintStr("bone_hpivot", "locator_id");
	e.ReadHintStr("bone_vpivot", "locator_id");
	e.ReadFP32("vpivot_height");
	e.ReadFP32("yaw");
	e.ReadFP32("pitch");
	e.ReadFP32("yaw_max");
	e.ReadString("locator_firepoint");
	e.ReadU32("ammo_num");
	e.ReadU32("ammo_visible");
	e.ReadBool("show_ammo");
	e.ReadFP32("dispersion_base");
	e.ReadFP32("dispersion_aim");
	e.ReadFP32("dispersion_move_factor");
	e.ReadFP32("dispersion_inc");
	e.ReadFP32("dispersion_min");
	e.ReadFP32("dispersion_max");
	e.ReadFP32("dispersion_dec");
	e.ReadFP32("hit_factor_player");
	e.ReadFP32("blt_power");
	e.ReadFP32("blt_impulse");
	e.ReadFP32("blt_fire_distance");
	e.ReadFP32("blt_speed")
	e.ReadString("bone_patron_prefix");
	e.ReadHintStr("track_aim_in", "camera_track, str_shared");
	e.ReadHintStr("track_aim_idle", "camera_track, str_shared");
	e.ReadHintStr("track_aim_out", "camera_track, str_shared");
	e.ReadU16("gauge", "entity_link, uobject_link");
	e.ReadFP32("heat_boost");
	e.ReadFP32("heat_fade");
	e.ReadU32("npc_fire_burst_min");
	e.ReadU32("npc_fire_burst_max");
	e.ReadFP32("npc_fire_pause_min");
	e.ReadFP32("npc_fire_pause_max");
	e.ReadFP32("target_feedforward_time");
	e.ReadFP32("cannot_fire_distance");
	e.ReadU32("cannot_fire_time");	
}

function ReadStationStand(e)
{
	ReadDrezina(e)
	
	e.ReadFP32("yaw")
	//e.ReadFP32("rotation_speed") // REMOVED in Redux!!
	//e.ReadU32("acceleration_time") // REMOVED in Redux!!
}

entity_readers["DREZINA_HAND"] = ReadDrezina
entity_readers["DREZINA_MOTO"] = ReadDrezina
entity_readers["KULEMET"] = ReadKulemet
entity_readers["STATION_STAND"] = ReadStationStand

function ReadEntities(entities)
{
	var already_listed = new Object;
	while(entities.More())
	{
		var e = entities.ReadSection();
		
		// common params
		var _class = e.ReadStringCrc("class", 
			typed_strings.get_class);
			
		var _static_data_key = e.ReadStringCrc("static_data_key", 
			typed_strings.get_static_data_key);
		
		e.ReadHintStr("att_bone_id", "locator_str");
		e.ReadU16("id");
		e.ReadU16("parent_id");
		e.ReadMatrix43("att_offset", "pose, matrix_43T");
		
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

