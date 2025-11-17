// interface
this.ReadStartup = ReadStartup;
this.ReadEntities = ReadEntities;

// implementation
var typed_strings = module("exodus\\typed_strings");
var visualscript  = module("exodus\\visualscript");

var entity_ver = 0;

var entity_readers = {
	// basic
	"STATICPROP"                 : ReadUObject_Static,
	"STATICPROP_AUX_EFFECT"      : ReadUObject_Static_Aux_Effect,
 	"STATICPROP_BREAKABLE"       : ReadUObject_StaticBreakable,
 	"STATICPROP_MOVABLE"         : ReadUObject_Static,
	"EFFECT"                     : ReadUObject_Effect,
	"EFFECTM"                    : ReadUObject_Effect_MLeaf,
	"O_ENTITY"                   : ReadCEntity,
	"SCRIPTED_ENTITY"            : ReadCEntity,
	"o_hlamp"                    : ReadHangingLamp,
	"O_AIPOINT"                  : ReadUObject_AIPoint,
	"PATROL_POINT"               : ReadPatrolPoint,
	"VISUALSCRIPT"               : ReadUObject,
	"O_BASEZONE"                 : ReadUObject_Zone,
	"O_WATERZONE"                : ReadWaterZone,
	"PROXY"                      : ReadUObject_Proxy,
	"SOFT_ENTITY"                : ReadSoftEntity,
	"O_HELPERTEXT"               : ReadHelperText,
	"O_INTEREST"		         : function(e)
	{
		ReadUObject(e);
		ReadInterest(e);
	},
	"O_EXPLOSION"                : ReadUObject_Explosion,
	"LADDER"                     : ReadLadder,
	"WAVES_EMITTER"              : ReadWavesEmitter,
	"FLOCK"                      : ReadFlock,
	"WALLMARK"                   : ReadWallmark,
	"AI_VISION_HELPER_ENTITY"    : ReadCEntity,
	"O_SCALING_ENTITY"           : ReadScalingEntity,
	"BENDING_TREE"               : ReadBendingTree,
	"WEB"                        : ReadWeb,
	"FORCE_FIELD"                : ReadForceField,
	"SCRIPTED_LIAN"              : ReadScriptedLian,
	"MODIFIER"                   : ReadModifier,
	"WB_WEAPON_HOLDER"           : ReadCEntity,
	"HUD_OBJECT"                 : ReadHUDObject,
	"HUD_LIGHT"                  : ReadHUDLight,
	"ENTITY_AUX_EFFECT"          : ReadEntityAuxEffect,
	"CLOCK"                      : ReadCEntity,
	"RAIL_ENTITY"                : ReadRailEntity,
	"O_ANIM_ENTITY"              : ReadCEntity,
	"FLEXIBLE_ENTITY"            : ReadUObject_Effect,
	"TREADMILL_TILE"             : ReadUObject_Effect,

	// upgrades
	"WEAPON_ITEM_LAMP"           : ReadUpgradeItemLamp,
	"WEAPON_ITEM_LAMP_BACKLIGHT" : ReadUpgradeItemLamp,
	"DEVICE_UPGRADE_LAMP"        : ReadUpgradeItemLamp,
	"WICK_VISUAL"                : ReadUpgradeItem,
	"LIGHTER_VISUAL"             : ReadUpgradeItem,
	"COSTUME_UPGRADE"            : ReadUpgradeItem,
	"COMPASS"                    : ReadUpgradeItem,
	"DEVICE_UPGRADE"             : ReadUpgradeItem,
	"VISOR"                      : ReadUpgradeItem,
	"PLAYER_TIMER_UPGRADE"       : ReadUpgradeItem,
	"MAP_PAD_UPGRADE"            : ReadUpgradeItem,
	"METAL_DETECTOR_UPGRADE"     : ReadUpgradeItem,
	"MOTION_SENSOR_UPGRADE"      : ReadUpgradeItem,
	"PULSOMETER_UPGRADE"         : ReadUpgradeItem,
	"WEAPON_ITEM"                : ReadUpgradeItem,
	"WEAPON_ITEM_AMMO"           : ReadUpgradeItem,
	"WEAPON_ITEM_SILENCER"       : ReadUpgradeItem,	
	"WEAPON_ITEM_OPTIC"          : ReadUpgradeItem,
	"WEAPON_ITEM_LASER"          : ReadUpgradeItem,
	"WEAPON_ITEM_VR"             : ReadUpgradeItem,
	"WEAPON_ITEM_PRESET"         : ReadUpgradeItem,
	"WEAPON_ITEM_MAGAZINE"       : function(e)
	{
		ReadUpgradeItemMagazine(e)
		e.ReadBool("ignore_difficulty")
	},
	"WEAPON_ITEM_SPEEDLOADER"    : ReadUpgradeItemMagazine,
	"WEAPON_ITEM_VR_ATTACH"      : function(e)
	{
		ReadUpgradeItem(e)
		e.ReadHint("preview_model", "choose")
		e.ReadString("preview_model")
	},
	"MOTION_SENSOR_ADV_UPGRADE"  : ReadUpgradeItem,
	"WRISTWATCH"                 : ReadUpgradeItem,
	
	"PLAYERS_HANDS"              : ReadUObject_Effect,
	"HANDS_FOR_DREZINA"          : ReadUObject_Effect
};

function ReadTime(e, name)
{
	e.ReadHint(name, "time")
	e.ReadU32(name)
}

function ReadInterest(e)
{
	var i = e.ReadSection("interest")
	
	i.ReadU16("min_importance")
	i.ReadU16("max_importance")
	i.ReadU8("interest_type")
	i.ReadU16("duration")
	i.ReadFP32("speed")
	i.ReadFP32("distance")
	i.ReadFP32("max_angle_x", "angle, fp32")
	i.ReadFP32("max_angle_y", "angle, fp32")
	i.ReadFP32("angle_coef")
}

function ReadVsRef(e, name)
{
	var n = e.ReadHintStr(name, "choose")
	var exist = e.ReadBool(name + "_dyn_state_exist")
	
	if(exist)
		print("!!!! VS REF dyn_state_exist not implemented")
	
	if(n.length > 0)
	{
		var arr = e.TryReadArray("exposed_blocks")
		if(arr) // don't exist in version 52
		{
			while(arr.MoreElements())
			{
				var b = arr.NextElement();
				b.ReadU16("blkid");
				visualscript.ReadBlock(b);
			}
		}
	}
}

function ReadVSs(e, name)
{
	var k = 0, arr = e.ReadArray(name)
	while(arr.MoreElements())
	{
		var vs = arr.NextElement()
		vs.ReadString("vs_name")
		vs.ReadBool("vs_debug")
		vs.ReadBool("vs_active")
		vs.ReadBool("disable_qsave")
		vs.ReadBool("save_on_nextlevel")
		ReadVsRef(vs, "vs_ref")
	}
}

function ReadPhysicsShell(e)
{
	var p = e.ReadSection("physics_shell")
	
	var elements = p.ReadArray("elements")
	while(elements.MoreElements())
	{
		var l = elements.NextElement()
		l.ReadU16("root_bid")
		l.ReadFP32("accumulated_impulse")
		l.ReadMatrix43("xform", "pose, matrix_43T")
		l.ReadVec3("velocity")
		l.ReadBool("nx_awake")
		var shapes = l.ReadArray("shapes")
		while(shapes.MoreElements())
		{
			var s = shapes.NextElement()
			s.ReadU16("bid")
		}
	}
}

function ReadWavesEmitterData(e)
{
	var wed = e.ReadSection("waves_emitter_data")
	wed.ReadU32("respawn_time")
	wed.ReadFP32("start_len")
	wed.ReadFP32("start_height")
	wed.ReadVec3("start_offset")
	wed.ReadBool("waves_active")
}

function ReadVolumes(e, name)
{
	var i = 0, volumes = e.ReadArray(name, "volume_%.2d");
	while(volumes.MoreElements())
	{
		var v = volumes.NextElement();
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
}

function ReadJoint(e)
{
	// копипаста из арктики.1, даже не проверял как оно там в движке
	var s = e.ReadSection("joint_section")
	s.ReadBool("enabled")
	s.ReadU16("entity_src", "entity_link, uobject_link")
	s.ReadHintStr("bone_src", "attp_str")
	s.ReadU16("entity_dst", "entity_link, uobject_link")
	s.ReadHintStr("bone_dst", "attp_str")
	s.ReadVec3("pos")
	s.ReadVec3("rot", "ang3f")
	
	// cnxph_world::load_joint_desc
	var type = s.ReadU16("joint_type")
	var params = s.ReadSection("params")
	if(type == 261)
	{
		params.ReadU32("motiontype_linearx")
		params.ReadU32("motiontype_lineary")
		params.ReadU32("motiontype_linearz")
	
		// linearlimit
		params.ReadFP32("linearlimit_value")
		params.ReadFP32("linearlimit_restitution")
		params.ReadFP32("linearlimit_spring")
		params.ReadFP32("linearlimit_damping")
	
		params.ReadU32("swing1motion")
	
		// swing1limit
		params.ReadFP32("swing1limit_value")
		params.ReadFP32("swing1limit_restitution")
		params.ReadFP32("swing1limit_spring")
		params.ReadFP32("swing1limit_damping")
		
		params.ReadU32("swing2motion")
		
		// swing2limit
		params.ReadFP32("swing2limit_value")
		params.ReadFP32("swing2limit_restitution")
		params.ReadFP32("swing2limit_spring")
		params.ReadFP32("swing2limit_damping")
		
		params.ReadU32("twist1Motion")
		
		// twistlimit_low
		params.ReadFP32("twistlimit_low_value")
		params.ReadFP32("twistlimit_low_restitution")
		params.ReadFP32("twistlimit_low_spring")
		params.ReadFP32("twistlimit_low_damping")
		
		// twistlimit_high
		params.ReadFP32("twistlimit_high_value")
		params.ReadFP32("twistlimit_high_restitution")
		params.ReadFP32("twistlimit_high_spring")
		params.ReadFP32("twistlimit_high_damping")
	}
}

function ReadUObject(e)
{
	e.ReadName("name")
	e.ReadBool8("oflags", ["reflectable","cast_ao","ghost","shadowgen","rws","disable_impostors","dao_auto"])
	e.ReadBool8("sflags", ["net_synchronization","disable_qsave","neversleep","force_realtime","inactive","need_fp64_xform"])
	e.ReadFP32("cull_distance")
	e.ReadMatrix43("", "pose, matrix")
	e.ReadHintStr("visual", "ref_model")
	e.ReadU16("dao_val")
	e.ReadVec4("render_aux_val", "color, vec4f")
	
	visualscript.ReadVssVer6(e, entity_ver)
	
	e.ReadBool("vs_active")
	e.ReadU16("spatial_sector")
	e.ReadU8("qsave_chunk")
}

function ReadUObject_Static(e)
{
	ReadUObject(e)
	
	// if(version <= 23) e.ReadBool8("flags", ["raycast", "force_render", "clg_from_model"])
	e.ReadBool8("flags", ["raycast", "raycast_ai", "force_render", "clg_override", "jumpover_allowed", "ceiling_test_allowed", "force_sliding", "hud_collidable"])
	e.ReadU8("collision_group")
	
	ReadInterest(e)
}

function ReadUObject_Static_Aux_Effect(e)
{
	ReadUObject_Static(e)
	e.ReadVec3("aux_position")
	e.ReadVec4("aux_selflight", "color, vec4f")
}

function ReadUObject_StaticBreakable(e)
{
	e.ReadFP32("health")
	
	//
	ReadUObject(e)
	
	ReadVSs(e, "commons_vs")
	ReadVSs(e, "removed_vs")

	// if(version <= 23) e.ReadBool8("flags", ["raycast", "force_render", "clg_from_model"])
	e.ReadBool8("flags", ["raycast", "raycast_ai", "force_render", "clg_override", "jumpover_allowed", "ceiling_test_allowed", "force_sliding", "hud_collidable"])
	e.ReadU8("collision_group")
	
	ReadInterest(e)
	//
	
	e.ReadHintStr("die_model", "choose")
	e.ReadHintStr("die_sound", "choose")
	e.ReadU8("die_sound_type")
	e.ReadHintStr("die_particles", "choose")
	e.ReadBool8("pflags", ["die_particles_ignore_rotation", "die_particles_from_mesh_center", "phys_hit_allowed"])
	//if(version < 43) e.ReadBool("block_ai_los")
	e.ReadFP32("death_impulse")
}

function ReadHelperText(e)
{
	ReadUObject(e)
	
	e.ReadString("text")
	e.ReadHintStr("text_key", "choose")
	e.ReadFP32("size")
	e.ReadVec4("color", "color, vec4f")
	e.ReadHintStr("font", "choose")
	e.ReadBool8("flags0", ["geometry_check", "glow", "text_visible", "post_forward"])
	e.ReadFP32("width")
	e.ReadFP32("height")
	e.ReadU8("h_alignment")
	e.ReadFP32("display_dist")
}

function ReadUObject_Effect(e)
{
	ReadUObject(e)
	
	ReadVSs(e, "commons_vs")
	ReadVSs(e, "removed_vs")
	
	e.ReadHintStr("startup_animation", "animation_str")
	e.ReadHintStr("bone_part", "part_str")
	e.ReadU16("start_frame")
	e.ReadFP32("speed")
	e.ReadBool8("startup_animation_flags", ["speed_replace", "force_paused"])
	e.ReadU8("force_looped")
	e.ReadHintStr("sound", "sound")
	e.ReadU8("sound_volume", "fp32_q8")
	e.ReadU8("sound_filter")
	e.ReadBool8("particle_flags", ["particles_active", "particles_constrained", "particles_ignore_parent_rotation", "particles_mesh_source"])
	e.ReadHintStr("particles", "choose")
	ReadInterest(e)
	e.ReadStrArray32("labels")
}

function ReadUObject_Effect_MLeaf(e)
{
	ReadUObject_Effect(e)
	
	// in Arktika.1 type was color, u32
	e.ReadVec4("particles_color", "color, vec4f")
}

function ReadCEntity(e)
{
	// class uobject_hit<>
	e.ReadFP32("health")
	
	// unknown class
	e.ReadU32("dying_mask")
	e.ReadBool8("physics_flags", ["is_physics", "allow_on_detach", "kinematic", "force_kinematic", "stay_in_forced_kinematic", "block_breacking", "raycast", "block_ai_los"])
	e.ReadBool8("physics_flags1", ["fast_rbody", "destroy_on_contact", "collision_exist", "skip_nx_update", "posrot_movement", "hud_collidable", "sub_scene_root", "semi_breakable"])
	e.ReadBool8("physics_flags2", ["jumpover_allowed", "ceiling_test_allowed", "force_sliding"])
	
	ReadUObject_Effect(e);
	
	// class centity
	e.ReadU8("friend_type")
	e.ReadU8("reaction_type")
	e.ReadHintStr("fixed_bones", "choose_array, str_shared")
	e.ReadFP32("break_impulse_threshold")
	e.ReadU8("collisions_group")
	e.ReadU8("scene_type")
	e.ReadHintStr("break_particles_break", "choose")
	e.ReadHintStr("break_particles_death", "choose")
	e.ReadHintStr("break_sound_death", "choose")
	e.ReadU8("break_sound_death_ai_type")
	e.ReadHint("type_mask", "flags64")
	e.ReadU64("type_mask")
	e.ReadU32("ph_shell_model_src")
	e.ReadU32("ph_shell_skltn_src")
	e.ReadU32("ph_shell_skltn_bcount")
	var writed = e.ReadBool("ph_shell_writed")
	if(writed)
		ReadPhysicsShell(e);
	var attach = e.ReadBool("attach_with_joint")
	if(attach)
		ReadJoint(e);
	e.ReadFP32("footprint_size")
	e.ReadFP32("footprint_power")
}

function ReadLightData(e, sect_name)
{
	var l = e.ReadSection(sect_name)
	l.ReadU8("type")
	l.ReadVec4("color", "color, vec4f")
	l.ReadFP32("brightness")
	l.ReadFP32("range_far")
	l.ReadFP32("lod_scale")
	l.ReadVec3("data1")
	l.ReadVec2("data2")
	l.ReadFP32("ibl_gen_radius")
	l.ReadFP32("range_near")
	l.ReadFP32("source_size")
	l.ReadFP32("cone", "angle, fp32")
	l.ReadFP32("quality")
	l.ReadVec3("position")
	l.ReadVec3("direction")
	l.ReadVec3("right")
	l.ReadHintStr("color_ca", "choose")
	l.ReadHintStr("texture", "choose")
	l.ReadHint("faces", "flags8")
	l.ReadBool8("faces", ["face0","face1","face2","face3","face4","face5"], "u8")
	l.ReadBool8("light_flags1", ["affect_via", "playershadow", "sscale", "pure", "envlink", "omni_stealth", "skip_stealth", "mul_by_ao"])
	l.ReadBool8("light_flags2", ["reverse_play", "linear_falloff", "fademode", "active", "projector_mode", "visual_luma_mode", "fake_gi"])
}

function ReadLamp(e)
{
	e.ReadBool("initial_state")
	e.ReadU8("die_sound_type")
	e.ReadHintStr("die_sound", "choose")
	e.ReadHintStr("die_particle", "choose")
	e.ReadHintStr("light_main_bone", "attp_str")
	e.ReadHintStr("dark_bone", "attp_str")
	e.ReadHintStr("broken_bone", "attp_str")
	
	ReadLightData(e, "main_light")
	
	e.ReadBool("color_to_aux")
	e.ReadBool("sync_color_to_aux")
	e.ReadU8("secondary_type")
	e.ReadHintStr("secondary_bone", "attp_str")
	e.ReadFP32("secondary_power")
	e.ReadFP32("secondary_radius")
	e.ReadBool("secondary_mul_by_ao")
	e.ReadBool("backlight")
	e.ReadU16("backlight_ref", "entity_link, uobject_link")
	e.ReadFP32("backlight_dist")
	e.ReadBool("backlight_dynamic")
	e.ReadBool("backlight_ignore_parents")
	e.ReadBool("backlight_brightness_compensation")
	e.ReadFP32("backlight_force_offset")
	e.ReadHintStr("backlight_ray", "choose")
	e.ReadHintStr("backlight_ray_particles", "choose")
	e.ReadBool("backlight_trace_npc_only")
	e.ReadU16("master", "entity_link, uobject_link")

	var f = e.ReadSection("flares_data")
	f.ReadHintStr("name", "choose")
	f.ReadHintStr("bone", "choose")
	f.ReadU8("axis")
	if(entity_ver > 49) // not sure
		f.ReadVec4("cmul", "color, vec4f")
}

function ReadHangingLamp(e)
{
	ReadCEntity(e)
	ReadLamp(e)
}

function ReadUObject_AIPoint(e)
{
	ReadUObject(e)
	
	for(var i = 0 ; i < 4; i++)
	{
		var l = e.ReadSection("link_"+i)
		l.ReadU16("object", "entity_link, uobject_link")
		l.ReadFP32("weight")
	}
	
	e.ReadBool8("ai_map", ["on_ai_map", "not_on_ai_map", "e_valid"])
	e.ReadHintStr("cover_group", "choose")
}

function ReadPatrolPoint(e)
{
	ReadUObject_AIPoint(e)
	
	e.ReadU32("min_wait_time")
	e.ReadU32("max_wait_time")
	
	// patrol state
	e.ReadString("body_state")
	e.ReadString("anim_state")
	e.ReadString("movement_type")
	e.ReadString("weapon_state")
	e.ReadString("action")
	e.ReadU16("target", "entity_link, uobject_link")
	e.ReadHint("flags", "flags32")
	e.ReadBool32("flags",
		["precise_hit", "exact_rotation", "force_sprint", "disable_strafing"],
		[ 0x01,          0x02,             0x04,           0x08]
	)
	e.ReadFP32("anim_state_approach_speed")
	e.ReadFP32("approaching_accel")
}

function ReadUObject_Restrictor(e)
{
	ReadUObject(e)
	
	e.ReadString("label")
	
	var k = 0, shapes = e.ReadArray("shapes", "shape_%.2d")
	while(shapes.MoreElements())
	{
		var shape = shapes.NextElement()
		var type = shape.ReadU32("type")
		
		switch(type)
		{
			case 0:
				var s = shape.ReadSection("sphere")
				s.ReadVec3("center")
				s.ReadFP32("radius")
			break;
			
			case 1:
				var b = shape.ReadSection("box")
				b.ReadMatrix43("", "pose, matrix")
				b.ReadVec3("h_size")
			break;
			
			default:
				print("unknown shape type ", type)
		}
	}
	
	e.ReadU8("collisions_group")
	e.ReadU8("obstacle_collision_group")
	e.ReadBool8("flags0", ["obstacle", "block_ai_vision_free", "jumpover_allowed", "force_sliding", "hud_collidable", "active"])
	e.ReadU8("block_ai_vision")
	e.ReadU8("scene_type")
	e.ReadHintStr("step_gmtrl", "choose")
	e.ReadU8("dynamic_mode")
}

function ReadUObject_Zone(e)
{
	ReadUObject_Restrictor(e)
	e.ReadHint("type_mask", "flags64")
	e.ReadU64("type_mask")
	e.ReadBool("type_filter_on")
}

function ReadWaterZone(e)
{
	ReadUObject_Zone(e);

	e.ReadHintStr("texture", "choose");
	e.ReadHintStr("mask", "choose");
	e.ReadHintStr("shader", "choose");
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
	e.ReadBool("envlit");
	e.ReadBool("sunshadow");
	e.ReadBool("tesselated"); // NEW in Exodus !!!
	e.ReadFP32("physx_wave_height");
	e.ReadFP32("physx_wave_speed");	
	e.ReadBool("allow_ripples");
	e.ReadHintStr("material", "choose");
}

function ReadUObject_Proxy(e)
{
	ReadUObject(e)
	
	// class uobject_proxy
	e.ReadU16("slice_count")
	var k = 0, entities = e.ReadArray("entities")
	while(entities.MoreElements())
	{
		var e = entities.NextElement()
		e.ReadU16("entity", "entity_link, uobject_link")
	}
}

function ReadInventoryItemObject(e)
{
	// class inventory_item
	e.ReadBool8("flags0", ["useful_for_player", "ammo_for_player", "dao_blink_prevent", "ready_after_cloned", "ui_force_slot", "attached_loot", "processed", "spare"])
	e.ReadU16("trade_weight")
	e.ReadU8("ui_force_slot_id")

	ReadCEntity(e)
}

function ReadUpgradeItem(e)
{
	ReadInventoryItemObject(e)
	
	// class chud_item_container (probably)
	e.ReadBool("anim_simplification")
	
	// class upgrade_item
	e.ReadHintStr("upgrade_id", "choose")
	e.ReadBool("free_on_level")
}

function ReadUpgradeItemMagazine(e)
{
	e.ReadBool("loot_ammo_limited_by_mag_size")
	e.ReadU16("loot_ammo_count")
	
	ReadUpgradeItem(e)
}

function ReadUpgradeItemLamp(e)
{
	ReadUpgradeItem(e)
	ReadLamp(e)
}

function ReadHUDObject(e)
{
	ReadInventoryItemObject(e)
	
	// class chud_item_container (probably)
	e.ReadBool("anim_simplification")
	
	// 
	e.ReadBool("use_hud_offset")
}

function ReadHUDLight(e)
{
	ReadHUDObject(e)
	ReadLamp(e)
}

function ReadLadder(e)
{
	// совершенно не похоже на ту лестницу которая была в прошлых частях...

	ReadCEntity(e)
	
	e.ReadString("bone_name_prefix")
	e.ReadU32("visible_bone_count")
}

function ReadWavesEmitter(e)
{
	ReadUObject(e)
	ReadWavesEmitterData(e)
}

function ReadFlock(e)
{
	ReadUObject_Effect(e)
	
	e.ReadU32("birds_count")
	e.ReadHintStr("bird_model", "choose")
	e.ReadHintStr("bird_animation", "animation_str")
	e.ReadFP32("bird_anim_speed_min")
	e.ReadFP32("bird_anim_speed_max")
	e.ReadFP32("birds_spawn_radius")
	
	e.ReadFP32("speed_multiplayer")
	e.ReadFP32("view_radius")
	e.ReadFP32("opt_distance")
	e.ReadFP32("alightment_force_part")
	e.ReadFP32("inertness")
	e.ReadFP32("vertical_priority")
	e.ReadFP32("attraction_force")
	e.ReadFP32("vel_shange_speed")
	e.ReadFP32("dir_change_speed")
	e.ReadFP32("attraction_boost_radius")
	e.ReadFP32("attraction_boost")
	e.ReadFP32("attraction_boost_min")
	e.ReadFP32("attraction_point_radius")
	e.ReadU32("waypoints_count")
	e.ReadFP32("waypoints_min_dist")
}

function ReadWallmark(e)
{
	ReadUObject(e)
	
	e.ReadU8("tex_index")
	e.ReadHint("marks", "tex_frame, u16*, u32")
	e.ReadU16Array("marks")
	e.ReadFP32("marks_size")
	e.ReadHint("marks_flags", "flags16")
	e.ReadU16("marks_flags")
	e.ReadVec4("aux_params")
	e.ReadBool("put_wallmark")
	e.ReadFP32("project_dist")
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
	e.ReadU8("blast_raycast_mode");
	e.ReadU32("delay");
	e.ReadU32("flame_time");
	e.ReadU32("flame_interval");
	e.ReadVec4("light_color", "color, vec4f");  // NEW in Exodus !!
	e.ReadFP32("light_range");                  // ..
	e.ReadU32("light_time_max");                // ..
	e.ReadU32("explode_duration_max");          // ..
	e.ReadU32("frags_num");
	e.ReadFP32("frags_radius");
	e.ReadFP32("frag_hit");
	e.ReadFP32("frag_hit_impulse");
	e.ReadFP32("frag_speed");
	e.ReadFP32("frags_dispersion");             // NEW in Exodus !!
	
	var s = e.ReadSection("frag_ammo");
	s.ReadFP32("k_power");
	s.ReadFP32("k_impulse");
	s.ReadFP32("k_power_falloff");
	s.ReadFP32("k_impulse_falloff");
	s.ReadFP32("k_speed");
	s.ReadFP32("k_head_coef");
	s.ReadFP32("k_monster_coef");               // NEW in Exodus !!
	s.ReadFP32("k_fire_distance_min");
	s.ReadFP32("k_fire_distance_max");
	s.ReadFP32("k_fire_distance");
	s.ReadFP32("k_pierce");
	s.ReadFP32("k_gravity_mod");                // NEW in Exodus !!
	s.ReadFP32("k_tracer_scale_xy");
	s.ReadFP32("k_tracer_scale_z");
	s.ReadFP32("k_tracer_min_dist");
	s.ReadFP32("k_tracer_probability");         // NEW in Exodus !!
	s.ReadFP32("k_trail_probability");          // NEW in Exodus !!
	s.ReadString("k_material");
	s.ReadHintStr("tracer_mesh", "choose");
	s.ReadHintStr("tracer_mesh_hud", "choose");
	s.ReadHintStr("tracer_particles", "choose");         // NEW in Exodus !!
	s.ReadHintStr("tracer_particles_hud", "choose");     // ..
	s.ReadHintStr("tracer_particles_ric", "choose");     // ..
	s.ReadHintStr("tracer_particles_ric_hud", "choose"); // ..
	s.ReadHintStr("impact_particles", "choose");
	s.ReadHintStr("trail_particles", "choose");          // NEW in Exodus !!
	s.ReadHintStr("trail_particles_hud", "choose");      // ..	
	s.ReadFP32("trail_min_dist");                        // ..
	s.ReadFP32("wm_size");
	s.ReadString("ammo_material");
	s.ReadBool8("flags0", ["noreload", "unlimited", "valuable", "burn"]);
	s.ReadU8("explosive"); // NEW in Exodus !!
	s.ReadFP32("blast_radius_min");
	s.ReadFP32("blast_radius_max");
	s.ReadFP32("blast_hit_min");
	s.ReadFP32("blast_hit_max");
	s.ReadFP32("blast_up_throw_factor");
	s.ReadFP32("blast_hit_impulse_factor");
	s.ReadU8("blast_hit_type");
	s.ReadU8("blast_raycast_mode");
	s.ReadU32("delay");
	s.ReadU32("flame_time");
	s.ReadU32("flame_interval");
	s.ReadVec4("light_color", "color, vec4f");
	s.ReadFP32("light_range");
	s.ReadU32("light_time_max");
	s.ReadU32("explode_duration_max");
	s.ReadU32("sound_explode_ai_type");
	ReadLightData(s, "explosive_light");
	s.ReadHintStr("coloranim", "choose")
	s.ReadHintStr("water_coloranim", "choose")
	s.ReadBool("hide_on_explosion")
	s.ReadFP32("light_deviation")
	s.ReadHintStr("sound_explode", "choose")
	s.ReadHintStr("sound_explode_echo", "choose")
	s.ReadHintStr("water_sound", "choose")
	s.ReadHintStr("water_surface_sound", "choose")
	s.ReadHintStr("explode_particles", "choose")
	s.ReadHintStr("water_particles", "choose")
	s.ReadHintStr("water_surface_particles", "choose")
	s.ReadFP32("dangerous_radius_coef")
	s.ReadFP32("wallmark_ray_dist")
	s.ReadString("explosive_material")
	s.ReadHintStr("track_explosion", "choose")
	s.ReadFP32("track_explosion_dist")
	s.ReadFP32("track_explosion_target")
	s.ReadBool("track_explosion_single_inst")
	s.ReadBool("explosion_dir_addpos")
	s.ReadFP32("max_dist_exp_particles")
	s.ReadU32("max_dist_exp_particles_delay")
	var arr = s.ReadArray("explosion_dir_sets")
	while(arr.MoreElements())
	{
		var rec = arr.NextElement()
		rec.ReadHintStr("particles_ground", "choose");
		rec.ReadHintStr("particles_ground_gm", "choose");
		rec.ReadHintStr("particles_water", "choose");
		rec.ReadHintStr("particles_water_gm", "choose");		
	}
	e.ReadU8("hit_frag");
	e.ReadString("label");
}

function ReadScalingEntity(e)
{
	ReadUObject_Effect(e)
	
	e.ReadU16("target", "entity_link, uobject_link")
	e.ReadHintStr("target_bone", "attp_str")
}

function ReadBendingTree(e)
{
	ReadUObject_Effect(e)
		
	e.ReadU16("bender", "entity_link, uobject_link")
	e.ReadFP32("power_factor")
	e.ReadFP32("lambda")
	e.ReadFP32("omega")
	e.ReadFP32("timer_factor")
	e.ReadFP32("oscillation_duration_s")
}

function ReadWeb(e)
{
	// Паутина от редукса отличается только хинтами
	e.ReadHintStr("subent", "choose"); // ref_model -> choose
	
	ReadCEntity(e);
	
	e.ReadMatrix43("subent_offset", "pose, matrix_43T");
	e.ReadHintStr("subent_attp", "attp_str"); // locator_id -> attp_str
	e.ReadHintStr("burn_particle_name", "choose"); // particles, str_shared -> choose
	e.ReadHintStr("burn_particle_locator", "attp_str"); // locator_id -> attp_str
	e.ReadMatrix43("burn_particle_offset", "pose, matrix_43T");
	e.ReadHintStr("burn_hud_particle_name", "choose"); // particles, str_shared -> choose
	e.ReadU8("burn_hud_particle_type");
	e.ReadVec4("sphere");
}

function ReadForceField(e)
{
	ReadUObject(e);
	
	ReadVolumes(e, "volumes");
	
	e.ReadBool("active");
	
	// force_field_data::load_dynamic
	e.ReadBool("player_only");
	var type = e.ReadU8("type");
	var desc = e.ReadSection("desc");
	
	switch(type)
	{
		case 0:
			var s = desc.ReadSection("tornado");
			s.ReadFP32("centrifugal_power");
			s.ReadFP32("attraction_power");
			s.ReadFP32("lifting_power");
			s.ReadFP32("centrifugal_velocity");
			s.ReadFP32("lifting_velocity");
		break;
		
		case 1:
			var s = desc.ReadSection("explosion");
			s.ReadFP32("power");
			s.ReadFP32("vel");
		break;
		
		case 2:
			var s = desc.ReadSection("directional");
			s.ReadFP32("power");
			s.ReadVec3("vel");
		break;
		
		case 3:
			var s = desc.ReadSection("tornado_advanced");
			s.ReadFP32("centrifugal_power");
			s.ReadFP32("attraction_power");
			s.ReadFP32("lifting_power");
			s.ReadFP32("centrifugal_velocity");
			s.ReadFP32("lifting_velocity");
			s.ReadFP32("bottom_radius_coef");
		break;
		
		case 4:
			var s = desc.ReadSection("attractor");
			s.ReadFP32("power");
			s.ReadFP32("vel");
		break;		
		
		case 5:
			var s = desc.ReadSection("directional_animated");
			s.ReadFP32("power_first");
			s.ReadVec3("vel_first");
			s.ReadFP32("power_second");
			s.ReadVec3("vel_second");
			s.ReadFP32("anim_period");
		break;
	}
	
	//for(var j = 0; j < 9; j++)
	//	e.ReadFP32("param_"+j);
	
	e.ReadBool8("flags0", ["interact_rbodies","interact_cloths","interact_fluids","break_rbodies"]);
	e.ReadBool("enable_rotation");
	e.ReadVec3("rotation_power");
	e.ReadU8("anim_type");
	// end of force_field_data::load_dynamic
	
	var entities = e.ReadArray("entities");
	while(entities.MoreElements())
	{
		var z = entities.NextElement();
		z.ReadU16("entity", "entity_link, uobject_link");
	}
	
	e.ReadStrArray32("labels")
}

function ReadScriptedLian(e)
{
	ReadCEntity(e)
	e.ReadBool("attack_only_player")
}

function ReadModifier(e)
{
	ReadUObject(e)
	e.ReadHintStr("modifier_name", "choose")
	e.ReadBool8("modifier_flags", ["modifier_active"])
}

function ReadEntityAuxEffect(e)
{
	ReadCEntity(e)
	e.ReadVec3("aux_position")
	e.ReadVec4("aux_selflight", "color, vec4f")
}

function ReadRailEntity(e)
{
	ReadCEntity(e)
	
	var arr = e.ReadArray("hidden_bones")
	while(arr.MoreElements())
		arr.NextElement().ReadU16("id")
		
	e.ReadBool("force_physics_callback")
}

function ReadSoftEntity(e)
{
	ReadUObject(e);
	
	e.ReadU16("collision_group");
	e.ReadBool("self_collision");
	e.ReadBool("apply_gravity");
	e.ReadBool("two_way_collision");
	e.ReadBool("auto_attach");
	e.ReadFP32("two_way_clg_factor");
	e.ReadFP32("attach_clg_factor");
	e.ReadBool("auto_constrain");
	e.ReadHintStr("collision_model", "choose");
	
	ReadVolumes(e, "constrain_volumes");
	ReadVolumes(e, "collision_volumes");
}

/*----------------------------------------------------------*/
/*                      NPCs                                */
/*----------------------------------------------------------*/
function ReadBaseBrainUnit(e, no_species_behavior_task)
{
	// base_brain_unit
	e.ReadU8("anim_state")
	e.ReadU8("movement_type")
	e.ReadU8("min_movement_type")
	e.ReadU8("max_movement_type")
	e.ReadU8("body_state")
	e.ReadFP32("step_acceleration")
	e.ReadString("group")
	e.ReadBool8("flags0", ["in_group", "memory_valid", "can_turn_body", "no_combat_mode", "can_be_group_leader", "invisible_grenades_reaction"])
	
	// base_visual_memory_manager
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
	e.ReadFP32("free_vision_force_danger_range")
	e.ReadFP32("free_vision_surv_threat_threshold")
	e.ReadFP32("free_vision_surv_light_alert_threshold")
	e.ReadFP32("free_vision_surv_vis_threshold")
	e.ReadFP32("free_vision_surv_lum_min_distance")
	e.ReadFP32("free_vision_peripheral_dist")
	e.ReadFP32("free_vision_peripheral_angle_x", "angle, fp32")
	e.ReadFP32("free_vision_peripheral_angle_y", "angle, fp32")
	e.ReadFP32("free_vision_peripheral_lum_min_distance")
	e.ReadU32("free_vision_transition_time")
	e.ReadFP32("alert_vision_range");
	e.ReadFP32("alert_vision_bwd_range");
	e.ReadFP32("alert_vision_fov", "angle, fp32");
	e.ReadU32("alert_vision_min_update_time");
	e.ReadFP32("alert_vision_luminocity_fac");
	e.ReadFP32("alert_vision_luminocity_factor");
	e.ReadFP32("alert_vision_velocity_fac");
	e.ReadFP32("alert_vision_decrease_value");
	e.ReadFP32("alert_vision_min_lum_threshold");
	e.ReadFP32("alert_vision_min_threshold");
	e.ReadFP32("alert_vision_threat_threshold");
	e.ReadFP32("alert_vision_light_alert_threshold");
	e.ReadFP32("alert_vision_vis_threshold");
	e.ReadFP32("alert_vision_lum_min_distance");
	e.ReadFP32("alert_vision_force_danger_range")
	e.ReadFP32("alert_vision_surv_threat_threshold")
	e.ReadFP32("alert_vision_surv_light_alert_threshold")
	e.ReadFP32("alert_vision_surv_vis_threshold")
	e.ReadFP32("alert_vision_surv_lum_min_distance")
	e.ReadFP32("alert_vision_peripheral_dist")
	e.ReadFP32("alert_vision_peripheral_angle_x", "angle, fp32")
	e.ReadFP32("alert_vision_peripheral_angle_y", "angle, fp32")
	e.ReadFP32("alert_vision_peripheral_lum_min_distance")
	e.ReadU32("alert_vision_transition_time")
	e.ReadFP32("danger_vision_range");
	e.ReadFP32("danger_vision_bwd_range");
	e.ReadFP32("danger_vision_fov", "angle, fp32");
	e.ReadU32("danger_vision_min_update_time");
	e.ReadFP32("danger_vision_luminocity_fac");
	e.ReadFP32("danger_vision_luminocity_factor");
	e.ReadFP32("danger_vision_velocity_fac");
	e.ReadFP32("danger_vision_decrease_value");
	e.ReadFP32("danger_vision_min_lum_threshold");
	e.ReadFP32("danger_vision_min_threshold");
	e.ReadFP32("danger_vision_threat_threshold");
	e.ReadFP32("danger_vision_light_alert_threshold");
	e.ReadFP32("danger_vision_vis_threshold");
	e.ReadFP32("danger_vision_lum_min_distance");
	e.ReadFP32("danger_vision_force_danger_range")
	e.ReadFP32("danger_vision_surv_threat_threshold")
	e.ReadFP32("danger_vision_surv_light_alert_threshold")
	e.ReadFP32("danger_vision_surv_vis_threshold")
	e.ReadFP32("danger_vision_surv_lum_min_distance")
	e.ReadFP32("danger_vision_peripheral_dist")
	e.ReadFP32("danger_vision_peripheral_angle_x", "angle, fp32")
	e.ReadFP32("danger_vision_peripheral_angle_y", "angle, fp32")
	e.ReadFP32("danger_vision_peripheral_lum_min_distance")
	e.ReadU32("danger_vision_transition_time")
	
	// base_sound_memory_manager
	e.ReadBool("sound_enabled")
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
	
	// base_threat_memory_manager
	e.ReadU32("la_free_inertia_time")
	e.ReadU32("a_free_inertia_time")
	e.ReadU32("ua_free_inertia_time")
	e.ReadU32("d_la_sounds_all_time")
	e.ReadU32("d_la_sounds_with_gap")
	e.ReadU32("d_la_sounds_gap")
	e.ReadU32("la_a_vision_all_time")
	e.ReadU32("la_a_vision_with_gap")
	e.ReadU32("la_a_vision_gap")
	e.ReadU32("la_a_sounds_all_time")
	e.ReadU32("la_a_sounds_with_gap")
	e.ReadU32("la_a_sounds_gap")
	e.ReadU32("a_ua_sounds_all_time")
	e.ReadU32("a_ua_sounds_with_gap")
	e.ReadU32("a_ua_sounds_gap")
	e.ReadU32("a_ua_vision_all_time")
	e.ReadU32("a_ua_vision_with_gap")
	e.ReadU32("a_ua_vision_gap")
	e.ReadU32("danger_ua_inertia_time")
	e.ReadBool("ua_free_corpse_block")
	
	// base_sound_player
	e.ReadHintStr("sound_scheme", "choose")
	
	// again base_brain_unit
	e.ReadFP32("max_heading", "angle, fp32")
	e.ReadFP32("max_pitch", "angle, fp32")
	
	// ???
	e.ReadBool8("species_flags", ["species_behavior_enabled", "attack_peaceful_species", "skip_cover_in_enemy_dir", "skip_cover_in_enemy_dir_fear", "can_stand_in_fear", "can_stand_in_fear_cover"])
	e.ReadU32("species_type")
	e.ReadFP32("npc_threat_level")
	e.ReadFP32("npc_courage_threshold")
	e.ReadFP32("npc_fear_threshold")
	e.ReadFP32("species_attack_confusion_dist")
	e.ReadFP32("species_fallback_cover_dist")
	e.ReadU32("species_fallback_cover_fear")
	e.ReadU32("species_fallback_cover")
	e.ReadFP32("species_threat_can_stand_dist")
	e.ReadU32("species_min_fallback_time")
	e.ReadIdentifierArray("species_enemy_blacklist")
	e.ReadIdentifierArray("species_attack_blacklist")
	
	if(!no_species_behavior_task)
	{
		var s = e.ReadSection("species_behavior_task") // empty section
	}
	
	// ?????
	e.ReadBool8("smell_flags", ["smell_enabled", "smell_player_only"])
	e.ReadFP32("smell_radius")
	e.ReadU32("smell_interval")
	e.ReadU8("smell_threat_type")
}

function ReadAIBrainUnit(e, attacks_list, no_species_behavior_task)
{
	ReadBaseBrainUnit(e, no_species_behavior_task);
	
	e.ReadBool("scary_enabled");
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
		a.ReadBool8("flags0", ["enabled", "vs_reference_dyn_state_exist"]);
		a.ReadHintStr("vs_reference", "choose");
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
	
	var static_combat = e.ReadSection("static_combat")
	static_combat.ReadBool("hold_position");
	static_combat.ReadU16("anchor", "entity_link, uobject_link");
	static_combat.ReadFP32("radius");
	static_combat.ReadFP32("enemy_dist");
	static_combat.ReadBool("point_valid");
	static_combat.ReadVec3("point_pos");
	static_combat.ReadVec3("point_dir");
	
	e.ReadBool("static_combat_mode")
	e.ReadBool("force_active_wo_enemy")
	
 	var s = e.ReadSection("_spot_enemy_task")
//var s = e.ReadSection("_species_behavior_task")               // REMOVED in Exodus !!
}

function ReadWatchmanBrainUnit(e, attacks_list)
{
	ReadMonsterBrainUnit(e, attacks_list)
	
	e.ReadU8("watchman_type")
	e.ReadHint("watchman_flags", "flags32") // TODO flags
	e.ReadU32("watchman_flags")
}

function ReadNpcBase(e, i_aqua, brain_unit_read_func)
{
	e.ReadBool("fully_dead")
	e.ReadU8("dying_from")
	
	ReadCEntity(e)
	
	e.ReadBool8("base_npc_flags", ["level_mental", "scripted", "immortal", "invulnerable", "forbid_melee_kill", "survival_mode_params", "update_corpse_luminocity", "attached_move"])
	e.ReadBool8("base_npc_flags1", ["force_ragdoll_death"])
	e.ReadFP32("luminocity")
	e.ReadU32("min_corpse_lum_update_interval")
	e.ReadU32("max_corpse_lum_update_interval")
	e.ReadFP32("shoot_dispersion_coef")
	e.ReadFP32("hit_power_coef")
	e.ReadFP32("hit_impulse_coef")
	e.ReadFP32("illuminated_react_level")
	e.ReadS32("offmesh_penalty")
	e.ReadU32("offmesh_reuse_delay")
	e.ReadU32("offmesh_inter_reuse_delay")
	e.ReadBool("stick_to_ground")
	e.ReadFP32("stick_to_ground_max_step")
	e.ReadFP32("stick_to_ground_ai_map_max_upward")
	e.ReadFP32("stick_to_ground_ai_map_max_downward")
	
	// virtual function load_dynamic_dhps
	e.ReadU32("damage_handle_preset")
	if(i_aqua == 1)
	{
		e.ReadU32("water_damage_handle_preset");
		e.ReadU32("climb_damage_handle_preset");
		e.ReadU32("closed_damage_handle_preset");
		e.ReadU32("sprint_damage_handle_preset");
	}
	if(i_aqua == 2)
	{
		e.ReadU32("water_damage_handle_preset");
		e.ReadU32("climb_damage_handle_preset");
		e.ReadU32("spt_water_damage_handle_preset");
		e.ReadU32("spt_surface_damage_handle_preset");
	}
		
	ReadVsRef(e, "movement_hit_vs_ref")
	
	brain_unit_read_func(e)
	
	e.ReadBool("ph_dying")
	
	// read waves emitter data
	ReadWavesEmitterData(e)
}

function ReadSimpleNpc(e)
{
	ReadNpcBase(e, false, ReadBaseBrainUnit)
}

function ReadHuman(e, brain_unit_read_func)
{
	// в арктике.1 было тут
	//e.ReadU16("rucksack", "entity_link, uobject_link") // from inventory_owner::load_dynamic
	//e.ReadU16("active_item") // from inventory::load_dynamic
	
	ReadNpcBase(e, false, brain_unit_read_func)
	
	// в исходе перенесли сюда
	e.ReadU16("rucksack", "entity_link, uobject_link") // from inventory_owner::load_dynamic
	e.ReadU16("active_item") // from inventory::load_dynamic
	
	e.ReadHint("human_flags", "flags16")
	e.ReadBool16("human_flags",
		["scripted_fire", "freegun_mode", "panic_enabled", "danger_delta_enabled", "animated_gunmode", "auto_reload_disabled", "gasmask_voice", "default_close_ranged_weapon"],
		[ 0x0001,          0x0002,         0x0400,          0x0008,                 0x0004,             0x0010,                 0x0080,          0x0200]
	)
	e.ReadU16("close_ranged_weapon", "entity_link, uobject_link")
	e.ReadU16("ranged_weapon", "entity_link, uobject_link")
	e.ReadFP32("close_ranged_weapon_distance")
	e.ReadFP32("ranged_weapon_distance")
	e.ReadU32("dispersion_decrease_time")
	e.ReadU32("dispersion_increase_time")
	e.ReadFP32("min_shoot_dispersion_coef")
	e.ReadFP32("max_shoot_dispersion_coef")
	e.ReadHintStr("override_voice", "choose")
	e.ReadHintStr("allowed_speech_groups", "choose_array, str_shared")

	var property_data = e.ReadArray("property_data");
	while(property_data.MoreElements())
	{
		var r = property_data.NextElement();
		r.ReadString("name");
		r.ReadS32("value");
	}
	
	e.ReadU8("speech_name") // NEW in Exodus !!
}

function ReadPlayer(e)
{
	function ReadBrainUnit(e)
	{
		ReadAIBrainUnit(e, [], true);
	}

	e.ReadBool("reviving");
	
	ReadHuman(e, ReadBrainUnit);
	
	e.ReadBool("waiting_for_take");
	e.ReadU16("take", "entity_link, uobject_link");
	var v = e.ReadArray("drop_vec");
	while(v.MoreElements())
	{
		var rec = v.NextElement();
		rec.ReadMatrix43("offset");
		rec.ReadU16("pos", "entity_link, uobject_link");
		rec.ReadU16("item", "entity_link, uobject_link");
		rec.ReadU8("drop_type");
	}
	e.ReadU16("players_body", "entity_link, uobject_link");
	e.ReadU16("players_hands", "entity_link, uobject_link");
	e.ReadU16("players_knife", "entity_link, uobject_link");
	e.ReadU16("players_torch", "entity_link, uobject_link");
	e.ReadU16("players_heap", "entity_link, uobject_link");
	e.ReadBool("new_game_plus_started");
	e.ReadFP32("suit_luminocity");
	e.ReadU32("mp_class_type");
	e.ReadFP32("def_restore_rate");
	e.ReadStrArray32("private_data");
	var achievement_data = e.TryReadArray("achievement_data"); // don't exist in editor's lua files
	if(achievement_data)
	{
		while(achievement_data.MoreElements())
		{
			var r = achievement_data.NextElement();
			r.ReadString("name");
			r.ReadS32("value");
			r.ReadS32("border");
		}
	}
	var points_data = e.ReadArray("points_data");
	while(points_data.MoreElements())
	{
		var r = points_data.NextElement();
		r.ReadS32("points");
		r.ReadString("comments");
	}
	e.ReadFP32("stamina");
	e.ReadBool("spend_filters");
	e.ReadFP32("money_loot_cent");
	e.ReadU32("min_loot_money");
	var arr = e.ReadArray("seen_weapon_items");
	while(arr.MoreElements())
	{
		var r = arr.NextElement();
		r.ReadString("sdata");
	}
	e.ReadU32("costume_active");
	var s = e.ReadSection("spartain")
	var u = s.ReadArray("upgrades")
	while(u.MoreElements())
	{
		var rec = u.NextElement()
		rec.ReadString("upgrade_id")
	}
	e.ReadU16("force_head_obj", "entity_link, uobject_link");
	
	[
		"ammo_545x39mm",
		"ammo_15mm",
		"ammo_044mm",
		"ammo_12x70mm",
		"ammo_arrow",
		"ammo_money",
		"ammo_762x39mm",
		"ammo_762x39mm_mg",
		"ammo_dshk",
		"ammo_12x70mm_fire",
		"ammo_044mm_ap",
		"ammo_12x70mm_flamethrower",
		"ammo_breakable_arrow",
		"craft_metal",
		"craft_chemical",
		"craft_consumable",
		"ammo_flamethrower_alt",
		"ammo_flame",
		"ammo_15mm_sabot",
		"knife_slot",
		"secondary_slot",
		"primary_slot_1",
		"grenade_slot",
		"gasmask_slot",
		"nightvision_slot",
		"map_slot",
		"medkit_slot",
		"filter_slot",
		"grenade_sticky_slot",
		"grenade_launcher_slot",
		"macheta_slot",
		"charger_slot",
		"grenade_flame_slot",
		"claymore_slot",
		"hud_object_slot",
		"primary_slot_2",
		"arrow_slot",
		"c4_slot",
		"lighter_slot",
		"grenade_flamethrower_slot",
		"shield_slot",
		"arrow_breakable_slot",
		"arm_device_slot",
		"torchlight_slot",
		"decoy_slot",
		"flare_slot",
		"binoculars_slot",
		"backpack_slot_1",
		"backpack_slot_2",
		"tape_slot",
	].forEach(function(ammo) {
		var sect = e.ReadSection(ammo);
		sect.ReadS32("max_amount");
	});
	
	e.ReadU32("timeout_grenade_friend");
	e.ReadU32("timeout_grenade_enemy");
	e.ReadStrArray32("opened_weapon_items");
	
	// player_new::load_dynamic
	e.ReadU16("workbench_weapon_holder", "entity_link, uobject_link");
}

function ReadNpc(e, attacks_list, b_woman_combat)
{
	function ReadBrainUnit(e)
	{
		ReadAIBrainUnit(e, attacks_list);
		
		// class human_brain_unit
		e.ReadBool8("flags", ["is_sniper", "suppress_fire", "static_combat_mode", "force_active_wo_enemy", "surrender_enabled", "skip_group_no_fire_time"]);
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
		
		// virtual function static_combat_task::load_dynamic
		e.ReadBool8("static_combat_flags", ["play_shot_delta", "aim_disabled", "aim_yaw", "aim_pitch", "not_use_cover", "shoot_while_moving"]);
		e.ReadHintStr("static_idle", "animation_str");
		e.ReadHintStr("static_attack", "animation_str");
		e.ReadHintStr("static_reload_idle", "animation_str");
		e.ReadHintStr("static_shoot", "animation_str");
		e.ReadHintStr("static_turn180l", "animation_str");  
		e.ReadHintStr("static_turn90l", "animation_str"); 
		e.ReadHintStr("static_turn0", "animation_str"); 
		e.ReadHintStr("static_turn90r", "animation_str"); 
		e.ReadHintStr("static_turn180r", "animation_str");
		e.ReadHintStr("static_turn_idle", "animation_str");
		e.ReadHintStr("static_sit_idle", "animation_str");
		e.ReadHintStr("static_stand_idle", "animation_str");
		e.ReadHintStr("static_in", "animation_str");
		e.ReadHintStr("static_out", "animation_str");
		
		var cc = e.ReadSection("common_combat");
		cc.ReadBool8("flags", ["cover_combat_allowed", "wounded_combat_allowed", "wo_enemy_allowed", "grenadier"]);
		cc.ReadFP32("cover_min_dist");
		cc.ReadFP32("common_max_dist");
		cc.ReadFP32("min_cover_weight");
		cc.ReadFP32("min_ualert_cover_weight"); // NEW in Exodus !!!
		cc.ReadFP32("wounded_max_dist");
		cc.ReadU32("timeout_min");
		cc.ReadU32("timeout_max");
		cc.ReadU32("timeout_min_see");
		cc.ReadU32("timeout_max_see");	
		cc.ReadFP32("weight_delta");
		cc.ReadFP32("fire_enemy_min");
		cc.ReadFP32("fire_enemy_max");
		cc.ReadU8("combat_type");
		cc.ReadHint("disabled_actions", "flags32") // NEW in Exodus !!!
		cc.ReadU32("disabled_actions") // NEW in Exodus !!!
		if(!b_woman_combat)
		{
			var in_cover = cc.ReadSection("in_cover");
			in_cover.ReadU32("enemy_seen_timeout");
			in_cover.ReadU32("lookout_min");
			in_cover.ReadU32("lookout_max");
			in_cover.ReadU32("aim_while_lookout_timeout");
			in_cover.ReadU32("lookout_cooldown_min");
			in_cover.ReadU32("lookout_cooldown_max");
			in_cover.ReadBool8("cover_task_flags", ["suppress_cover_enabled", "blind_fire_enabled", "force_suppress_cover"]);  // NEW in Exodus !!!
			//in_cover.ReadU32("lookout_cooldown_min_far");     // REMOVED in Exodus !!!
			//in_cover.ReadU32("lookout_cooldown_max_far");     // ..
			//in_cover.ReadFP32("lookout_cooldown_dist_near");  // ..
			//in_cover.ReadBool("supperess_cover_enabled");     // ..
			//in_cover.ReadBool("blind_fire_enabled");          // ..
			in_cover.ReadU32("suppress_after_hit_delay_min"); 
			in_cover.ReadU32("suppress_after_hit_delay_max"); 
			in_cover.ReadU32("suppress_out_delay_min");       
			in_cover.ReadU32("suppress_out_delay_max");      
			in_cover.ReadU8("min_suppress_fire_count");
			in_cover.ReadU8("max_suppress_fire_count");
		}
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
		common.ReadU32("hide_type");
		var alert = e.ReadSection("alert");
		if(!b_woman_combat)
		{
			//alert.ReadBool("search_enabled"); // REMOVED in Exodus !!!
			alert.ReadU32("timeout_go_search");
			//alert.ReadU32("timeout_nervous_alert"); // REMOVED in Exodus !!!
			//alert.ReadU32("timeout_nervous_alert_uber"); // REMOVED in Exodus !!!
		}
		if(!b_woman_combat)
		{
			e.ReadSection("scary"); // empty section
			e.ReadSection("panic"); // empty section
			//e.ReadSection("human_species_behavior"); // empty section (REMOVED in Exodus !!!)
			e.ReadSection("surrender"); // empty section
		}
		e.ReadSection("neutral"); // empty section
		if(!b_woman_combat)
		{
			var gd = e.ReadSection("grenade_dodging");
			gd.ReadBool("grenade_dodging_enabled");
			e.ReadSection("light_alert"); // empty section (NEW in Exodus !!!)
		}
		
		// странно как-то тут сделано
		var val = e.ReadU32("aim_min_time_hi");
		if(val > 0) e.ReadU32("aim_min_time_lo");
	}
	
	ReadHuman(e, ReadBrainUnit);
}

entity_readers["ARAHIND"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks = [
			"melee_attack_360",
			"melee_attack_k1",
			"melee_attack_k2",
			"melee_vs_player_1",
			"melee_vs_player_2",
			"melee_vs_player_3",
		];
		
		ReadMonsterBrainUnit(e, attacks);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["ANOMALY"] = function(e)
{
	function ReadBrainUnit(e)
	{
		ReadAIBrainUnit(e, [], true);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
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
	
	ReadNpcBase(e, 2, ReadBrainUnit);
	
	e.ReadU16("head", "entity_link, uobject_link");
	e.ReadFP32("water_level");
}

entity_readers["AQUA_MALE_SMALL"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks = [
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
			"melee_attack_long_90bite",
		];
		
		ReadMonsterBrainUnit(e, attacks);
		
		e.ReadU8("am_state")
		e.ReadBool("tired_closed")
	}
	
	ReadNpcBase(e, 1, ReadBrainUnit);
	
	e.ReadU16("head", "entity_link, uobject_link");
	e.ReadFP32("water_level");
}

entity_readers["BLIND"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks = [
			"melee_vs_player_1"
		];
		
		ReadMonsterBrainUnit(e, attacks);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["CANNIBAL"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks = [
			"melee_attack_short",
			"melee_attack_middle",
			"melee_attack_universal",
			"melee_attack_long",
			"melee_gun_in",
			"melee_gun_point",
			"melee_gun_out",
			"melee_leg_in",
			"melee_leg_point",
			"melee_leg_out"
		];
		
		ReadMonsterBrainUnit(e, attacks);
		
		e.ReadU8("thrower_type");
		e.ReadFP32("thrower_orbit_radius")
		e.ReadFP32("thrower_melee_radius")
		
		e.ReadU8("gunner_type")
		e.ReadU32("gunner_show_cooldown")
	}
	
	ReadHuman(e, ReadBrainUnit);
}

entity_readers["CATFISH"] = function(e)
{
	function ReadBrainUnit(e)
	{	
		var attacks = [
			"melee_attack_eat",
			"melee_attack_hit_back",
		];
		
		ReadMonsterBrainUnit(e, attacks);
		
		e.ReadBool("hold_depth_state")
		e.ReadFP32("hold_depth")
	}
	
	ReadNpcBase(e, false, ReadBrainUnit)
	
	e.ReadU16("permanent_water", "entity_link, uobject_link")
}

entity_readers["DARKCHILD"] = ReadSimpleNpc

entity_readers["DEER"] = function(e)
{
	function ReadBrainUnit(e)
	{
		ReadMonsterBrainUnit(e, []);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["DOG"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks = [
			"melee_attack_0",
			"melee_attack_1",
			"melee_vs_player_1",
			"melee_vs_player_2",
			"melee_vs_player_3",
			"melee_vs_player_4",
		];
		ReadWatchmanBrainUnit(e, attacks);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["FLYING_CREATURE"] = ReadSimpleNpc;
entity_readers["FROG"] = ReadSimpleNpc;

entity_readers["GRIZLY"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks = [
			"grizly_melee_attack"
		];
		
		ReadMonsterBrainUnit(e, attacks);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["HARPY"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks = [
			"melee_attack_0",
			"melee_attack_jump",
			"melee_attack_fly",
			"melee_attack_close",
			"melee_attack_1",
		];
		
		ReadMonsterBrainUnit(e, attacks);
		
		e.ReadHint("demon_flags", "flags32") // TODO flags
		e.ReadU32("demon_flags")
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
	
	e.ReadBool("always_crack_ice");
	e.ReadFP32("min_height");
	e.ReadFP32("max_height");
	e.ReadFP32("alert_min_height");
	e.ReadFP32("alert_max_height");	
	e.ReadFP32("danger_min_height");
	e.ReadFP32("danger_max_height");	
	e.ReadFP32("alert_takeoff_min_dist");
	e.ReadFP32("alert_takeoff_max_dist");
	ReadTime(e, "wounded_time");
}

entity_readers["HUMANIMAL"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks = [
			"melee_attack_0",
			"melee_attack_1",
			"melee_attack_long_0",
			"melee_attack_long_1",
			"melee_attack_low",
			"melee_vs_player_1",
			"melee_vs_player_3",
			"melee_vs_player_4",
			"melee_vs_player_5",
		];
		
		ReadMonsterBrainUnit(e, attacks);
		
		e.ReadU8("thrower_type");
		e.ReadFP32("thrower_orbit_radius")
		e.ReadFP32("thrower_melee_radius")
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["KID"] = ReadSimpleNpc;

entity_readers["LURKER"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks = [
			"melee_attack_360_jump",
			"melee_attack_360_jaw",
			"melee_vs_player_1",
			"melee_vs_player_2",
			"melee_vs_player_3",
		];
		
		ReadMonsterBrainUnit(e, attacks);
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
			"melee_attack_1",
			"melee_vs_player_1",
			"melee_vs_player_2",
			"melee_vs_player_3",
		];
		
		ReadMonsterBrainUnit(e, attacks_list);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["NPC_FX"] = function(e)
{
	var attacks_list = [
			"melee_attack_360_butt", 
			"melee_attack_360_kick",
			"melee_attack_360_bzyk",
			"melee_attack_360_club" // NEW in Exodus !!
		];
		
	ReadNpc(e, attacks_list);
}

entity_readers["PLAYER"] = ReadPlayer;
entity_readers["RABBIT"] = ReadSimpleNpc;
entity_readers["RAT"] = ReadSimpleNpc;

entity_readers["SEA_CUCUMBER"] = function(e)
{
	function ReadBrainUnit(e)
	{
		ReadMonsterBrainUnit(e, []);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["SNAKE"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks = [
			"melee_attack_0"
		];
		
		ReadMonsterBrainUnit(e, attacks);
		
		// if(version < 52) e.ReadBool8("motor_flags", ["movable", "ignore_anim_in_speed"])
		e.ReadBool8("motor_flags", ["movable", "ignore_anim_in_speed", "jaw_control"])
		e.ReadFP32("water_vertical_offset")
		e.ReadFP32("vertical_offset")
		e.ReadFP32("water_amplitude")
		e.ReadFP32("amplitude")
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
	
	e.ReadFP32("collision_test_dh")
	e.ReadFP32("dive_depth")
	e.ReadHintStr("ragdoll_model", "choose")
}

entity_readers["WATCHMAN"] = function(e)
{
	function ReadBrainUnit(e)
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
			"watch_melee_attack_7",
			"melee_vs_player_1",
			"melee_vs_player_2",
			"melee_vs_player_3",
		];
		ReadWatchmanBrainUnit(e, attacks);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["WOMAN"] = ReadSimpleNpc;

entity_readers["WOMAN_COMBAT"] = function(e)
{
	var attacks_list = [
			"melee_attack_360_butt"
		];
		
	ReadNpc(e, attacks_list, true);
}

entity_readers["WOMAN_STRIP"] = ReadSimpleNpc;

entity_readers["WORM"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks = [
			"melee_attack_0"
		];
		
		ReadMonsterBrainUnit(e, attacks);
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

/*--------------------------------------*/
/*     Inventory Items & Devices        */
/*--------------------------------------*/

function ReadAmmo(e)
{
	ReadInventoryItemObject(e)
	e.ReadU16("box_value")
}

function ReadHelsingArrow(e)
{
	ReadAmmo(e)
	e.ReadU16("count")
}

function ReadUpgradableItem(e)
{
	ReadHUDObject(e)
	
	var a = e.ReadSection("upgradable_0")
	var u = a.ReadArray("upgrades")
	while(u.MoreElements())
	{
		var rec = u.NextElement()
		rec.ReadString("upgrade_id")
	}
}

entity_readers["AMMO"] = ReadAmmo
entity_readers["ARM_DEVICES"] = ReadUpgradableItem
entity_readers["BINOCULARS"] = ReadUpgradableItem
entity_readers["CHARGER"] = ReadUpgradableItem
entity_readers["FILTER"] = function(e)
{
	ReadHUDObject(e)
	e.ReadFP32("timer")
}
entity_readers["GASMASK"] = function(e)
{
	ReadUpgradableItem(e)
	
	e.ReadFP32("current_time")
	e.ReadFP32("total_time")
	e.ReadFP32("hit_force")
	e.ReadFP32("vs_effect")
	e.ReadU16("current_filter", "entity_link, uobject_link")
}

entity_readers["HELSING_ARROW"] = ReadHelsingArrow
entity_readers["HELSING_ARROW_BREAKABLE"] = ReadHelsingArrow
entity_readers["LIGHTER"] = ReadUpgradableItem
entity_readers["MEDKIT"] = function(e)
{
	ReadHUDObject(e)
	e.ReadU32("ampulas_num")
}

entity_readers["NIGHTVISION"] = function(e)
{
	ReadHUDObject(e)
	e.ReadU16("light", "entity_link, uobject_link")
	e.ReadU16("improved_light", "entity_link, uobject_link")
}
	
entity_readers["PLAYER_MAP"] = ReadUpgradableItem

entity_readers["TAPE"] = ReadHUDObject;

entity_readers["TORCHLIGHT_UPGRADABLE"] = function(e)
{
	ReadUpgradableItem(e)
	ReadLamp(e)
	e.ReadFP32("cone_def", "angle, fp32")
	e.ReadFP32("cone_improved", "angle, fp32")
}

/*--------------------------------*/
/*            Weapons             */
/*--------------------------------*/

function ReadWeaponBase(e)
{
	e.ReadString("dlc_model_name")
	ReadHUDObject(e)
	e.ReadBool("ignore_difficulty")
}

function ReadWeapon(e)
{
	e.ReadString("physics_model_name")
	
	ReadWeaponBase(e)
	
	e.ReadU32("ammo_loaded")
	e.ReadU8("bullets_in_barrel")
	e.ReadBool8("flags0", ["alt_ammo_installed", "need_breaking", "dismantlable"])
	
	var u = e.ReadArray("upgrades")
	while(u.MoreElements())
	{
		var rec = u.NextElement()
		rec.ReadString("upgrade_id")
	}
}

function ReadWeaponPneumo(e)
{
	ReadWeapon(e)
	e.ReadFP32("pressure")
}

function ReadDynamite(e)
{
	ReadHUDObject(e)
	e.ReadU16("count")
}

entity_readers["WEAPON_AK_74"] = ReadWeapon
entity_readers["WEAPON_AK_SAMMY"] = ReadWeapon
entity_readers["WEAPON_ASHOT"] = ReadWeapon
entity_readers["WEAPON_DAGGER"] = ReadDynamite
entity_readers["WEAPON_DECOY"] = ReadDynamite
entity_readers["WEAPON_DYNAMITE"] = ReadDynamite
entity_readers["WEAPON_FLAMETHROWER"] = ReadWeaponPneumo
entity_readers["WEAPON_FLAME_DYNAMITE"] = ReadDynamite
entity_readers["WEAPON_FLARE"] = ReadDynamite
entity_readers["WEAPON_GATLING"] = ReadWeaponPneumo
entity_readers["WEAPON_HELSING"] = ReadWeaponPneumo
entity_readers["WEAPON_KOLYA"] = ReadWeapon
entity_readers["WEAPON_REVOLVER"] = ReadWeapon
entity_readers["WEAPON_MACHETA"] = ReadWeaponBase
entity_readers["WEAPON_TIHAR"] = ReadWeaponPneumo
entity_readers["WEAPON_UBLUDOK"] = function(e)
{
	ReadWeapon(e)
	e.ReadFP32("temperature")
	e.ReadFP32("temperature_old")
}
entity_readers["WEAPON_UBOYNICHEG"] = ReadWeapon
entity_readers["WEAPON_VENTIL"] = ReadWeapon
entity_readers["WEAPON_VYHLOP"] = ReadWeapon

/*------------------------------------------------------*/
/*                  Vehicles                            */
/*------------------------------------------------------*/
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
	e.ReadHintStr("bone_hpivot", "attp_str");
	e.ReadHintStr("bone_vpivot", "attp_str");
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
	e.ReadHintStr("track_aim_in", "choose");
	e.ReadHintStr("track_aim_idle", "choose");
	e.ReadHintStr("track_aim_out", "choose");
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

entity_readers["BOAT"] = function(e)
{
	ReadCEntity(e)
	ReadWavesEmitterData(e)
	
	var d = e.ReadSection("data")
	d.ReadFP32("velocity_forward_max")
	d.ReadU32("time_acceleration_forward")
	d.ReadFP32("velocity_backward_max")
	d.ReadU32("time_acceleration_backward")
	d.ReadU32("time_stop_neutral")
	d.ReadU32("time_stop_brake")
	d.ReadFP32("velocity_forsage")
	d.ReadString("bone_with_bottom")
	d.ReadU8("guide_axis")
	d.ReadFP32("turn_vel_max", "angle, fp32")
	d.ReadFP32("turn_power")
	d.ReadU32("turn_time2maxvel")
	d.ReadFP32("boat_stability")
	d.ReadFP32("tilt_threshold")
	if(d.More()) // may be, or may not be
	{
		var s = d.ReadSection("steering_system")
		s.ReadFP32("max_steer", "angle, fp32")
		s.ReadU32("vel_vs_steer_size")
		var arr = s.ReadArray("vel_vs_steer");
		while(arr.MoreElements())
		{
			var rec = arr.NextElement();
			rec.ReadVec2("vel_steer");
		}
	}
	
	if(e.More()) // may be, or may not be
	{
		e.ReadU16("steer", "entity_link, uobject_link")
		e.ReadU16("gearstick_lever", "entity_link, uobject_link")
	}
}

entity_readers["DREZINA_HAND"] = ReadDrezina
entity_readers["DREZINA_MOTO"] = ReadDrezina
entity_readers["KULEMET"] = ReadKulemet

entity_readers["VEHICLE"] = function(e)
{
	ReadCEntity(e)
	
	function ReadVehInputParam(e, name)
	{
		var param = e.ReadSection(name)
		param.ReadFP32("raise")
		param.ReadFP32("fall")
	}
	
	function ReadVehInputData(e, name)
	{
		// veh_input_data::load_dynamic
		var input = e.ReadSection(name)
		var r = input.ReadSection("raise_fall_rate")
		var move = r.ReadSection("move")
		ReadVehInputParam(move, "accel")
		ReadVehInputParam(move, "breake")
		ReadVehInputParam(move, "hand_breake")
		var steer = r.ReadSection("steer")
		ReadVehInputParam(steer, "left")
		ReadVehInputParam(steer, "right")
	}
	
	// so much nested sections...
	
	{ // veh_data::load_dynamic
		var data = e.ReadSection("data")
		
		{ // veh_engine_data::load_dynamic
			data.ReadVec3("com_offset")
			
			var engine = data.ReadSection("engine")
			// veh_base_engine_data::load_dynamic
			engine.ReadFP32("max_rpm")
			engine.ReadFP32("max_torque")
			// veh_air_inlet_data::load_dynamic
			engine.ReadVec3("air_inlet_offset")
			engine.ReadFP32("air_inlet_radius")
			
			var curve = engine.ReadSection("curve")
			curve.ReadFP32("torque_curve0")
			curve.ReadFP32("torque_curve1")
			curve.ReadFP32("torque_curve2")
			curve.ReadFP32("torque_curve3")
			curve.ReadFP32("torque_curve4")
			curve.ReadFP32("torque_curve5")
			curve.ReadFP32("torque_curve6")
			curve.ReadFP32("torque_curve7")
		}
		
		ReadVehInputData(data, "digital_input")
		ReadVehInputData(data, "analog_input")
		
		var t = data.ReadSection("transmission")
		t.ReadU8("diff_type")
		
		{ // veh_suspension_system_data::load_dynamic
			var s = data.ReadSection("suspension_system")
			var w = s.ReadSection("wheels")
			var wheels = ["l_fwd", "r_fwd", "l_bck", "r_bck"];
			for(var i = 0; i < wheels.length; i++)
			{
				var wheel = w.ReadSection(wheels[i])
				{ // veh_tire_data::load_dynamic
					var tire = wheel.ReadSection("tire")
					tire.ReadU8("type")
					tire.ReadFP32("damp")
				}
				{ // veh_dampfer_data::load_dynamic
					var dampfer = wheel.ReadSection("dampfer")
					dampfer.ReadFP32("spring_compression")
					dampfer.ReadFP32("spring_elongation")
					dampfer.ReadFP32("spring_strength")
					dampfer.ReadFP32("ammortizator_strength")
				}
			}
		}
		
		var system = data.ReadSection("steering_system")
		system.ReadFP32("max_steer", "angle, fp32")
		system.ReadU32("vel_vs_steer_size")
		var arr = system.ReadArray("vel_vs_steer")
		while(arr.MoreElements())
		{
			var rec = arr.NextElement()
			rec.ReadVec2("vel_steer")
		}
	}
	
	e.ReadU16("steer", "entity_link, uobject_link")
	e.ReadU16("speedometer", "entity_link, uobject_link")
}

/*--------------------------------*/
/*        Main function's         */
/*--------------------------------*/
function ReadStartup(s, entity_ver)
{
	function ReadPresetBlend(e)
	{
		e.ReadString("name")
		e.ReadBool("game_time")
		e.ReadBool("removing")
		e.ReadS32("start")
		e.ReadS32("finish")	
	}
	
	function ReadPresetTablet(e)
	{
		e.ReadVec3("map_positional_min")
		e.ReadVec3("map_positional_max")
		e.ReadBool("map_positional_preview_aabb")
		e.ReadHintStr("map_menu_name", "choose")
		e.ReadU8("map_rotation")
		e.ReadString("subst_tablet_model")
	}

	s.ReadU32("game_time")
	s.ReadFP32("dc_prev_speed")
	s.ReadFP32("dc_target_speed")
	s.ReadFP32("dc_curr_speed")
	s.ReadS32("dc_time_0")
	s.ReadS32("dc_time_1")
	s.ReadString("main_preset_name")
	s.ReadString("current")
	ReadPresetBlend(s)
	s.ReadU8("source_type")
	
	var modifiers = s.ReadArray("modifiers")
	for(var i = 0; modifiers.More(); i++)
	{
		var m = modifiers.ReadSection(RecStr("rec_", i, 4), false);
		ReadPresetBlend(m);
	}
	
	s.ReadU64("_total_time")
	s.ReadHintStr("weather_preset", "choose")
	ReadTime(s, "dc_start")
	ReadTime(s, "dc_duration")
	s.ReadHintStr("foliage_set", "choose")
	s.ReadFP32("foliage_fuzziness")
	
	if(entity_ver >= 53) // DLC Sam Story
	{
		s.ReadHint("raytrace_options", "flags32")
		s.ReadU32("raytrace_options")
		s.ReadU8("tablet_presets_count")
		
		var presets = s.ReadArray("tablet_presets_vector")
		for(var i = 0; presets.More(); i++)
			ReadPresetTablet(presets.ReadSection(RecStr("rec_", i, 4), false))
	}
	else
	{
		ReadPresetTablet(s)
	}
	
	s.ReadString("next_level")
	s.ReadString("back_music")
	s.ReadU16("migration_rules")
}

function ReadEntities(entities, entity_version)
{
	var not_implemented = new Array
	
	entity_ver = entity_version
	
	while(entities.MoreElements())
	{
		var e = entities.NextElement()
		
		// common params
		var _class = e.ReadStringCrc("class", typed_strings.get_class)
		var _static_data_key = e.ReadStringCrc("static_data_key", typed_strings.get_static_data_key)
		e.ReadHintStr("att_bone_id", "choose")
		e.ReadU16("id")
		e.ReadU16("parent_id")
		e.ReadMatrix43("att_offset", "pose, matrix_43T")
		e.ReadBool("att_root")
		
		if(entity_readers[_class])
		{
			entity_readers[_class](e);
	
			if(e.More())
				print(_class, " data left");
		}
		else
		{
			if(not_implemented.indexOf(_class) === -1)
				not_implemented.push(_class)
		}
	}
	
	for(var i = 0; i < not_implemented.length; i++)
		print("not implemented class ", not_implemented[i]);
}

