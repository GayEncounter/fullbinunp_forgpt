// interface
this.ReadStartup = ReadStartup;
this.ReadEntities = ReadEntities;

// implementation
var typed_strings = module("a1\\typed_strings");
var visualscript = module("a1\\visualscript");

function ReadVolumes(e, name)
{
	var i = 0, volumes = e.ReadArray(name);
	while(volumes.More())
	{
		var v = volumes.ReadSection(RecStr("volume_", i++, 2), false);
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

var entity_readers = {
	// basic
	"STATICPROP"            : ReadUObject_Static,
 	"STATICPROP_BREAKABLE"  : ReadUObject_StaticBreakable,
	"EFFECT"                : ReadUObject_Effect,
	"EFFECTM"               : ReadUObject_Effect_MLeaf,
	"O_ENTITY"              : ReadCEntity,
	"o_hlamp"               : ReadHangingLamp,
	"O_AIPOINT"             : ReadUObject_AIPoint,
	"PATROL_POINT"          : ReadPatrolPoint,
	"VISUALSCRIPT"          : ReadUObject,
	"O_BASEZONE"            : ReadUObject_Zone,
	"O_WATERZONE"           : ReadWaterZone,
	"GRAB_ZONE"             : ReadGrabZone,
	"PROXY"                 : ReadUObject_Proxy,
	"SOFT_ENTITY"           : ReadSoftEntity,
	//"O_INTEREST"          : function(e)
	//{
	//	ReadUObject(e);
	//	ReadInterest(e);
	//},
	"O_EXPLOSION"           : ReadUObject_Explosion,
	"FORCE_FIELD"           : ReadForceField,
	//"LADDER"              : ReadLadder,
	//"BREAKABLE_ICE"       : ReadBreakableIce,
	"VR_CUBE"               : ReadVRCube,
	"VR_ENTITY"             : ReadVREntity,
	"VIRTUAL_HAND"          : ReadCEntity,
	"VIRTUAL_CAMERA"        : ReadVirtualCamera,
	"VIRTUAL_MONITOR"       : ReadVirtualMonitor,
	"SCRIPTED_ENTITY"       : ReadCEntity,
	"FLEXIBLE_ENTITY"       : ReadUObject_Effect,
	"EFFECT_PAUSABLE"       : ReadUObject_Effect,
	"TELEPORT"              : ReadTeleport,
	"MECH_ENTITY"           : ReadMechEntity,
	"O_SCALING_ENTITY"      : ReadScalingEntity,
	"O_HELPERTEXT"          : ReadHelperText,
	"O_HELPERTEXT_VR_INFO"  : ReadHelperText,
	"O_HELPERTEXT_COUNTER"  : ReadHelperTextCounter,
	"TURRET"                : ReadTurret,
	"MAGNETIC_HOLSTER"      : ReadMagneticHolster,
	"TORCHLIGHT_UPGRADABLE" : ReadTorchlight,
	"PLAYERS_HANDS"         : ReadUObject_Effect,
	"O_ANIM_ENTITY"         : ReadCEntity,
}

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
}

function ReadVsRef(e, name)
{
	var n = e.ReadHintStr(name, "choose")
	e.ReadBool(name + "_dyn_state_exist")
	
	if(n.length > 0)
	{
		var i = 0, arr = e.ReadArray("exposed_blocks")
		while(arr.More())
		{
			var b = arr.ReadSection(RecStr("rec_", i++, 4), false);
			b.ReadU16("blkid");
			visualscript.ReadBlock(b);
		}
	}
}

function ReadVSs(e, name)
{
	var k = 0, arr = e.ReadArray(name)
	while(arr.More())
	{
		var vs = arr.ReadSection(RecStr("rec_", k++, 4), false)
		vs.ReadString("vs_name")
		vs.ReadBool("vs_debug")
		vs.ReadBool("vs_active")
		vs.ReadBool("disable_qsave")
		ReadVsRef(vs, "vs_ref")
	}
}

function ReadPhysicsShell(e)
{
	var p = e.ReadSection("physics_shell");

	var elements = p.ReadArray("elements");
	for(var i = 0; elements.More(); i++)
	{
		var l = elements.ReadSection(RecStr("rec_", i, 4), false);
		l.ReadU16("root_bid");
		l.ReadFP32("accumulated_impulse");
		l.ReadMatrix43("xform", "pose, matrix_43T"); // if (version < 28) ReadMatrix44
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

function ReadJoint(e)
{
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
		params.ReadFP32("value")
		params.ReadFP32("restitution")
		params.ReadFP32("spring")
		params.ReadFP32("damping")
	
		params.ReadU32("swing1motion")
	
		// swing1limit
		params.ReadFP32("value")
		params.ReadFP32("restitution")
		params.ReadFP32("spring")
		params.ReadFP32("damping")
		
		params.ReadU32("swing2motion")
		
		// swing2limit
		params.ReadFP32("value")
		params.ReadFP32("restitution")
		params.ReadFP32("spring")
		params.ReadFP32("damping")
		
		params.ReadU32("twist1Motion")
		
		// twistlimit_low
		params.ReadFP32("value")
		params.ReadFP32("restitution")
		params.ReadFP32("spring")
		params.ReadFP32("damping")
		
		// twistlimit_high
		params.ReadFP32("value")
		params.ReadFP32("restitution")
		params.ReadFP32("spring")
		params.ReadFP32("damping")
	}
}

function ReadUObject(e)
{
	e.ReadName("name");
	e.ReadBool("force_hud")
	e.ReadBool("ghost2")
	e.ReadU8("oflags", "bool8")
	e.ReadU8("sflags", "bool8")
	e.ReadFP32("cull_distance")
	e.ReadMatrix43("", "pose, matrix");
	e.ReadHintStr("visual", "choose")
	e.ReadU16("dao_val");
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
	e.ReadFP32("health")
	
	//ReadUObject_Static(e)
	
	//
	ReadUObject(e)
	ReadVSs(e, "commons_vs")
	ReadVSs(e, "removed_vs")
	
	e.ReadU8("flags", "bool8")
	e.ReadU8("collision_group")
	//
	
	e.ReadHintStr("die_model", "choose")
	e.ReadHintStr("die_sound", "choose")
	e.ReadU8("die_sound_type")
	e.ReadHintStr("die_particles", "choose")
	e.ReadU8("pflags", "bool8")
	//if(version < 43) e.ReadBool("block_ai_los")
	e.ReadFP32("death_impulse")
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
	e.ReadU8("startup_animation_flags", "bool8")
	e.ReadU8("force_looped")
	e.ReadHintStr("sound", "choose")
	e.ReadU8("sound_volume", "fp32_q8")
	e.ReadU8("sound_filter")
	e.ReadHintStr("particles", "choose")
	e.ReadU8("particle_flags", "bool8")
	ReadInterest(e)
	e.ReadStrArray32("labels")
}

function ReadUObject_Effect_MLeaf(e)
{
	ReadUObject_Effect(e)
	
	e.ReadU32("particles_color", "color, u32")
}

function ReadCEntity(e)
{
	e.ReadFP32("health")
	e.ReadU32("dying_mask")
	
	ReadUObject_Effect(e)
	
	e.ReadU8("add_physics_flags", "bool8")
	e.ReadU8("friend_type")
	e.ReadU8("reaction_type")
	e.ReadHintStr("fixed_bones", "choose_array, str_shared")
	e.ReadU8("physics_flags0", "bool8")
	e.ReadU8("physics_flags1", "bool8")
	e.ReadFP32("break_impulse_threshold")
	e.ReadU8("collisions_group")
	e.ReadU8("scene_type")
	e.ReadHintStr("break_particles_break", "choose")
	e.ReadHintStr("break_particles_death", "choose")
	e.ReadHintStr("break_sound_death", "choose")
	e.ReadHint("type_mask", "flags64")
	e.ReadU64("type_mask")
	e.ReadU32("ph_shell_model_src")
	e.ReadU32("ph_shell_skltn_src")
	e.ReadU32("ph_shell_skltn_bcount")
	var writed = e.ReadBool("ph_shell_writed")
	if(writed)
		ReadPhysicsShell(e)
	var attach = e.ReadBool("attach_with_joint")
	if(attach)
		ReadJoint(e)
	e.ReadFP32("footprint_size")
	e.ReadFP32("footprint_power")
}

function ReadLampData(e)
{
	e.ReadBool("initial_state")
	e.ReadU8("die_sound_type")
	e.ReadHintStr("die_sound", "choose")
	e.ReadHintStr("die_particle", "choose")
	e.ReadHintStr("light_main_bone", "attp_str")
	e.ReadHintStr("dark_bone", "attp_str")
	e.ReadHintStr("broken_bone", "attp_str")
	
	// function load_light_data
	var l = e.ReadSection("main_light")
	l.ReadU8("type")
	l.ReadVec4("color", "color, vec4f")
	l.ReadFP32("brightness")
	l.ReadFP32("range_far")
	l.ReadVec3("data1")
	l.ReadVec2("data2")
	l.ReadFP32("ibl_gen_radius")
	l.ReadFP32("range_near")
	l.ReadFP32("source_size")
	l.ReadFP32("cone", "angle, fp32")
	l.ReadFP32("quality")
	l.ReadU32("min_ql")
	l.ReadU32("min_ql_shad")
	l.ReadVec3("position")
	l.ReadVec3("direction")
	l.ReadVec3("right")
	l.ReadHintStr("color_ca", "choose")
	l.ReadHintStr("texture", "choose")
	l.ReadHint("faces", "flags8")
	l.ReadU8("faces")
	l.ReadU8("light_flags1", "bool8")
	l.ReadU8("light_flags2", "bool8")

	e.ReadBool("color_to_aux")
	e.ReadBool("sync_color_to_aux")
	e.ReadBool("is_point_ambient")
	e.ReadHintStr("ambient_bone", "attp_str")
	e.ReadFP32("ambient_power")
	e.ReadFP32("ambient_radius")
	e.ReadHintStr("ambient_texture", "choose")
	e.ReadBool("backlight")
	e.ReadU16("backlight_ref", "entity_link, uobject_link")
	e.ReadFP32("backlight_dist")
	e.ReadBool("backlight_dynamic")
	e.ReadBool("backlight_ignore_parents")
	e.ReadFP32("backlight_force_offset")
	e.ReadHintStr("backlight_ray", "choose")
	e.ReadHintStr("backlight_ray_particles", "choose")
	e.ReadBool("backlight_trace_npc_only")
	e.ReadU16("master", "entity_link, uobject_link")
	e.ReadHintStr("flares", "choose")
	e.ReadHintStr("flares_bone", "choose")
	e.ReadU8("flares_axis")
}

function ReadHangingLamp(e)
{
	ReadCEntity(e);
	ReadLampData(e);
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
	
	e.ReadU8("ai_map", "bool8")
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
	e.ReadString("action");
	e.ReadU16("target", "entity_link, uobject_link")
	e.ReadHint("flags", "flags32")
	e.ReadU32("flags")
	e.ReadFP32("anim_state_approach_speed")
	e.ReadFP32("approaching_accel")
}

function ReadUObject_Restrictor(e)
{
	ReadUObject(e)
	
	e.ReadString("label")
	
	var k = 0, shapes = e.ReadArray("shapes")
	while(shapes.More())
	{
		var shape = shapes.ReadSection(RecStr("shape_", k++, 2), false)
		var type = shape.ReadU32("type")
		
		switch(type)
		{
			case 0:
				var s = shape.ReadSection("sphere")
				s.ReadVec3("center")
				s.ReadFP32("radius")
			break
			
			case 1:
				var b = shape.ReadSection("box")
				b.ReadMatrix43("", "pose, matrix")
				b.ReadVec3("h_size")
			break
			
			default:
				print("unknown shape type ", type)
		}
	}
	
	e.ReadU8("collisions_group")
	e.ReadU8("obstacle_collision_group")
	e.ReadU8("flags0", "bool8")
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
	e.ReadFP32("physx_wave_height");
	e.ReadFP32("physx_wave_speed");	
	e.ReadBool("allow_ripples");
	e.ReadHintStr("material", "choose");
}

function ReadGrabZone(e)
{
	ReadUObject_Zone(e);
	e.ReadBool("right_slot");
}

function ReadUObject_Proxy(e)
{
	ReadUObject(e)
	
	var k = 0, entities = e.ReadArray("entities")
	while(entities.More())
	{
		var e = entities.ReadSection(RecStr("rec_", k++, 4), false)
		e.ReadU16("entity", "entity_link, uobject_link")
	}
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
	s.ReadFP32("k_fire_distance");
	s.ReadFP32("k_pierce");
	s.ReadFP32("k_tracer_scale_xy");
	s.ReadFP32("k_tracer_scale_z");
	s.ReadFP32("k_tracer_min_dist");
	s.ReadFP32("k_gravity_multiplier");
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
	s.ReadHintStr("tracer_mesh", "choose");
	s.ReadHintStr("tracer_mesh_hud", "choose");
	s.ReadHintStr("bullet_particles", "choose");
	s.ReadHintStr("impact_particles", "choose");
	s.ReadHintStr("trail_mesh", "choose");
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
	s.ReadBool("craft_res");
	
	e.ReadU8("hit_frag");
	e.ReadVec4("light_color", "color, vec4f");
	e.ReadFP32("light_range");
	e.ReadU32("light_time_max");
	e.ReadU32("explode_duration_max");
	e.ReadString("label");
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
	
	e.ReadU8("flags0", "bool8");
	e.ReadBool("enable_rotation");
	e.ReadVec3("rotation_power");
	e.ReadU8("anim_type");
	
	var entities = e.ReadArray("entities");
	var j = 0;
	while(entities.More())
	{
		var z = entities.ReadSection(RecStr("rec_", j++, 4), false);
		z.ReadU16("entity", "entity_link, uobject_link");
	}
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

function ReadVREntity(e) 
{ 
	ReadCEntity(e); 
	ReadVrMissile(e); 
}

function ReadVRCube(e)
{
	function ReadAuxParams(e, n)
	{
		var r = e.ReadSection(n)
		
		r.ReadVec4("color", "color, vec4f")
		r.ReadVec3i("blend_time")
		r.ReadU8("flags", "bool8")
	}
	
	ReadUObject_Effect(e)
	
	e.ReadVec3("displacement_scale")
	e.ReadFP32("displacement_speed")
	e.ReadU8("flags", "bool8")
	
	ReadAuxParams(e, "highlight_default_section")
	ReadAuxParams(e, "highlight_by_name_section")
	ReadAuxParams(e, "highlight_player_section")
	ReadAuxParams(e, "highlight_by_group_section")
	
	e.ReadU16("target", "entity_link, uobject_link")
	e.ReadString("group_name")
	e.ReadFP32("distance_to_target")
	e.ReadFP32("distance_to_player")
	e.ReadFP32("distance_to_group_member")
}

function ReadVirtualCamera(e)
{
	ReadUObject_Effect(e)
	
	e.ReadFP32("aspect")
	e.ReadFP32("fov_vert")
	e.ReadFP32("far_plane")
}

function ReadVirtualMonitor(e)
{
	ReadCEntity(e)
	
	e.ReadVec2("monitor_size")
	e.ReadFP32("monitor_quality")
	e.ReadFP32("monitor_quality_max")
	e.ReadBool("force_aspect")
	e.ReadBool("mirror_reflect_in_plane")
	e.ReadBool("clear_distortion")
	e.ReadBool("cull_fwd")
	e.ReadU16("camera", "entity_link, uobject_link")
	e.ReadU8("rmode")
	e.ReadU32("thermal_cold_color", "color, u32")
	e.ReadU32("thermal_heat_color", "color, u32")
	e.ReadFP32("thermal_dist_fade")
	e.ReadFP32("thermal_albedo_fade")
	e.ReadFP32("thermal_heat_mp")
	e.ReadFP32("thermal_fog")
	e.ReadFP32("post_process_brightness")
	e.ReadFP32("post_process_contrast1")
	e.ReadFP32("post_process_contrast2")
	e.ReadFP32("post_process_saturation")
}

function ReadTeleport(e)
{
	ReadUObject_Effect(e)
	
	e.ReadU8("mode")
	e.ReadU8("flags0", "bool8")
	e.ReadU16("custom_point", "entity_link, uobject_link")
	e.ReadFP32("icon_scale")
	ReadTime(e, "icon_warmup_time")
	e.ReadHintStr("icon_particles_blue_in", "choose")
	e.ReadHintStr("icon_particles_blue_idle", "choose")
	e.ReadHintStr("icon_particles_blue_out", "choose")
	e.ReadHintStr("icon_particles_orange_in", "choose")
	e.ReadHintStr("icon_particles_orange_idle", "choose")
	e.ReadHintStr("icon_particles_orange_out", "choose")
	e.ReadHintStr("dummy_mesh_particles_blue_idle", "choose")
	e.ReadHintStr("dummy_mesh_particles_blue_aimed", "choose")
	e.ReadHintStr("dummy_mesh_particles_orange_idle", "choose")
	e.ReadHintStr("dummy_mesh_particles_orange_aimed", "choose")
	e.ReadFP32("dummy_scale");
	
	[
		"noise",
		"vertex_noise",
		"dissolve",
		"uv_scroll_1",
		"uv_scroll_2",
		"additional_on_select"
	].forEach(function(n) {
		var s = e.ReadSection(n)
		s.ReadHintStr("model", "choose")
		s.ReadHintStr("preset_blue", "choose")
		s.ReadHintStr("preset_green", "choose")
		s.ReadHintStr("preset_orange", "choose")
		s.ReadVec4("aux", "color, vec4f")
		
		if(n === "vertex_noise") {
			s.ReadVec4("aux_blue", "color, vec4f")
			s.ReadVec4("aux_orange", "color, vec4f")
		}
		if(n === "dissolve") {
			s.ReadVec4("aux_start", "color, vec4f")
			s.ReadVec4("aux_target", "color, vec4f")
		}
	});
}

function ReadMechEntity(e)
{
	ReadCEntity(e)
	
	e.ReadBool("continuous")
	e.ReadFP32("continuous_vel")
	e.ReadFP32("flick_mul")
	
	var s = e.ReadSection("mech_params")
	
	s.ReadHintStr("mesh_bone", "attp_str")
	s.ReadU8("axis")
	s.ReadFP32("vel_dampen")
	s.ReadU8("mech_flags", "bool8")
	s.ReadBool("move")
	s.ReadFP32("limit_min")
	s.ReadFP32("limit_max")
	s.ReadFP32("vel_mul")
	s.ReadFP32("limit_trig_norm")
	s.ReadFP32("tick", "angle, fp32")
}

function ReadScalingEntity(e)
{
	ReadUObject_Effect(e)
	
	e.ReadU16("target", "entity_link, uobject_link")
	e.ReadHintStr("target_bone", "attp_str")
}

function ReadHelperText(e)
{
	ReadUObject(e)
	
	e.ReadString("text")
	e.ReadHintStr("text_key", "choose")
	e.ReadFP32("size")
	e.ReadVec4("color", "color, vec4f")
	e.ReadHintStr("font", "choose")
	e.ReadU8("flags0", "bool8")
	e.ReadFP32("width")
	e.ReadFP32("height")
	e.ReadU8("h_alignment")
	e.ReadFP32("display_dist")
}

function ReadHelperTextCounter(e)
{
	ReadHelperText(e)
	
	e.ReadU8("counter_format")
	ReadTime(e, "time")
	e.ReadBool("count_up")
	e.ReadU16("counter_ref", "entity_link, uobject_link")
}

function ReadTurret(e)
{
	ReadCEntity(e)
	
	e.ReadFP32("max_yaw", "angle, fp32")
	e.ReadFP32("max_pitch", "angle, fp32")
	e.ReadFP32("rotation_speed")
	ReadTime(e, "fire_cooldown_min")
	ReadTime(e, "fire_cooldown_max")
	e.ReadU16("fire_queue_min")
	e.ReadU16("fire_queue_max")
	e.ReadFP32("aim_threshold_yaw", "angle, fp32")
	e.ReadFP32("aim_threshold_pitch", "angle, fp32")	
	e.ReadBool("fire_if_out_of_sight")
}

function ReadMagneticHolster(e)
{
	ReadUObject(e)
	
	var s = e.ReadSection("box_left")
	s.ReadMatrix43("", "pose, matrix_43T")
	s.ReadVec3("h_size")
	
	var s = e.ReadSection("box_right")
	s.ReadMatrix43("", "pose, matrix_43T")
	s.ReadVec3("h_size")	
}

function ReadTorchlight(e)
{
	ReadInventoryItemObject(e);
	
	var sect = e.ReadSection("upgradable_0");
	var i = 0, u = sect.ReadArray("upgrades");
	while(u.More())
	{
		var s = u.ReadSection(RecStr("rec_", i++, 4), false)
		s.ReadU8("is_installed")
		s.ReadString("upgrade_id")
		// не уверен что это всё
	}
	
	ReadLampData(e);
	
	e.ReadFP32("power_consumption_ex");
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
	// отличия от LL:
	// параметры min_lum_threshold, min_threshold теперь перед параметром threat_threshold
	// добавлены параметры lum_min_distance, surv_threat_threshold, surv_vis_threshold, surv_lum_min_distance перед параметром transition_time
	// отличия от Redux:
	// параметр alert_vision_threat_threshold теперь после alert_vision_min_threshold
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
	// отличий от Redux нет
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
	// отличия от LL:
	// добавлены параметры survival_vision_time, survival_vision_period перед sound_count
	// отличия от Redux:
	// параметр a_free_inertia_time теперь после la_free_interia_time
	// добавлены параметры ua_free_inertia_time, d_la_sounds_all_time, d_la_sounds_with_gap, d_la_sounds_gap, la_a_vision_all_time, la_a_vision_with_gap, la_a_vision_gap, la_a_sounds_all_time, la_a_sounds_with_gap, la_a_sounds_gap, a_ua_sounds_all_time, a_ua_sounds_with_gap, a_ua_sounds_gap, ua_free_corpse_block
	// убраны параметры max_sounds_count, la_alert_time, vision_time, vision_period, survival_vision_time, survival_vision_period, sound_count, sound_period
	e.ReadU32("la_free_interia_time");
	e.ReadU32("a_free_inertia_time");
	e.ReadU32("ua_free_inertia_time");
	e.ReadU32("d_la_sounds_all_time");
	e.ReadU32("d_la_sounds_with_gap");
	e.ReadU32("d_la_sounds_gap");
	e.ReadU32("la_a_vision_all_time");
	e.ReadU32("la_a_vision_with_gap");
	e.ReadU32("la_a_vision_gap");
	e.ReadU32("la_a_sounds_all_time");
	e.ReadU32("la_a_sounds_with_gap");
	e.ReadU32("la_a_sounds_gap");
	e.ReadU32("a_ua_sounds_all_time");
	e.ReadU32("a_ua_sounds_with_gap");
	e.ReadU32("a_ua_sounds_gap");
	e.ReadBool("ua_free_corpse_block");
	
	// base_enemy_memory_manager
	// отличий от ЛЛ нет
	// отличий от Redux нет
	e.ReadU32("post_enemy_wait_interval_static");
	e.ReadU32("post_enemy_wait_interval_random");
	e.ReadU32("enemy_inertia_time");
	e.ReadU32("enemy_remember_time");
	
	// base_sound_player
	// отличий от ЛЛ нет
	// отличий от Redux нет
	e.ReadHintStr("sound_scheme", "choose");
	
	// again base_brain_unit
	e.ReadFP32("max_heading", "angle, fp32");
	e.ReadFP32("max_pitch", "angle, fp32");
	
	// base_brain_unit::species_params::load_dynamic
	// новый подкласс в арктике.1
	e.ReadBool("species_behavior_enabled")
	e.ReadU32("species_type")
	e.ReadFP32("npc_threat_level")
	e.ReadFP32("npc_courage_threshold")
	e.ReadFP32("npc_fear_threshold")
	e.ReadFP32("species_attack_confusion_dist")
	e.ReadFP32("species_fallback_cover_dist")
	e.ReadU32("species_fallback_cover")
	e.ReadBool("attack_peaceful_species")
	e.ReadU32Array("species_enemy_blacklist", "identifier_array")
	e.ReadU32Array("species_attack_blacklist", "identifier_array")
}

function ReadAIBrainUnit(e, attacks_list)
{
	ReadBaseBrainUnit(e);
	
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
		a.ReadU8("flags0", "bool8");
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
	
	var static_combat = e.ReadSection("static_combat")            // NEW in Arktika.1 !!!
	static_combat.ReadBool("hold_position");                      // ..
	static_combat.ReadU16("anchor", "entity_link, uobject_link"); // ..
	static_combat.ReadFP32("radius");                             // ..
	static_combat.ReadFP32("enemy_dist");                         // ..
	static_combat.ReadBool("point_valid");                        // ..
	static_combat.ReadVec3("point_pos");                          // ..
	static_combat.ReadVec3("point_dir");                          // ..
	
	e.ReadBool("static_combat_mode")                              // ..
	e.ReadBool("force_active_wo_enemy")                           // ..
	
 	var s = e.ReadSection("_spot_enemy_task")                     // .. (empty section)
 	var s = e.ReadSection("_species_behavior_task")               // .. (empty section)
}

function ReadNpcBase(e, b_aqua, brain_unit_read_func)
{
	e.ReadBool("fully_dead");
	e.ReadU8("dying_from");
	
	ReadCEntity(e);
	
	e.ReadU8("balance_preset");
	e.ReadU8("base_npc_flags", "bool8");
	e.ReadU8("base_npc_flags1", "bool8");
	e.ReadFP32("luminocity");
	e.ReadU32("min_corpse_lum_update_interval")
	e.ReadU32("max_corpse_lum_update_interval")
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
	
	var s = e.ReadSection("waves_emitter_data")
	s.ReadU32("respawn_time")
	s.ReadFP32("start_len")
	s.ReadFP32("start_height")
	s.ReadVec3("start_offset")
	s.ReadBool("waves_active")
}

function ReadSimpleNpc(e)
{
	ReadNpcBase(e, false, ReadBaseBrainUnit);
}

function ReadHuman(e, brain_unit_read_func)
{
	e.ReadU16("rucksack", "entity_link, uobject_link") // from inventory_owner::load_dynamic
	e.ReadU16("active_item") // from inventory::load_dynamic
	
	ReadNpcBase(e, false, brain_unit_read_func)
	
	e.ReadHint("human_flags", "flags16")
	e.ReadU16("human_flags")
	e.ReadU16("close_ranged_weapon", "entity_link, uobject_link")
	e.ReadU16("ranged_weapon", "entity_link, uobject_link")
	e.ReadFP32("close_ranged_weapon_distance")
	e.ReadFP32("ranged_weapon_distance")
	e.ReadU32("dispersion_decrease_time")
	e.ReadU32("dispersion_increate_time")
	e.ReadFP32("min_shoot_dispersion_coef")
	e.ReadFP32("max_shoot_dispersion_coef")
	e.ReadHintStr("override_voice", "choose")
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

	e.ReadFP32("suit_luminocity");
	e.ReadU32("mp_class_type");
	e.ReadFP32("def_restore_rate");
	e.ReadStrArray32("private_data");
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
	e.ReadFP32("stamina");
	e.ReadBool("spend_filters");
	e.ReadFP32("money_loot_cent");
	e.ReadU32("min_loot_money");
	var arr = e.ReadArray("seen_weapon_items");
	for(var i = 0; arr.More(); i++)
	{
		var r = arr.ReadSection(RecStr("rec_", i, 4), false);
		r.ReadU32("sdata");
	}
	e.ReadU32("costume_active");
	e.ReadU16("force_head_obj", "entity_link, uobject_link");
	
	// player_new::load_dynamic
	e.ReadFP32("teleport_fov", "angle, fp32");
	e.ReadFP32("max_teleport_fov", "angle, fp32");
	e.ReadFP32("teleport_distance");
	e.ReadFP32("teleport_distance_max");
	e.ReadFP32("teleport_distance_min");
	e.ReadU32("distance_factor");
	ReadTime(e, "teleport_cooldown");
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
		
		// virtual function static_combat_task::load_dynamic
		e.ReadU8("static_combat_flags", "bool8"); // NEW in Arktika.1 !!!
		e.ReadHintStr("static_idle", "animation_str");
		e.ReadHintStr("static_attack", "animation_str");
		e.ReadHintStr("static_reload_idle", "animation_str");
		e.ReadHintStr("static_shoot", "animation_str");
		e.ReadHintStr("static_turn180l", "animation_str");   // NEW in Arktika.1 !!!
		e.ReadHintStr("static_turn90l", "animation_str");    // ..
		e.ReadHintStr("static_turn0", "animation_str");      // ..
		e.ReadHintStr("static_turn90r", "animation_str");    // ..
		e.ReadHintStr("static_turn180r", "animation_str");   // ..
		e.ReadHintStr("static_turn_idle", "animation_str");  // ..
		e.ReadHintStr("static_sit_idle", "animation_str");   // ..
		e.ReadHintStr("static_stand_idle", "animation_str"); // ..
		e.ReadHintStr("static_in", "animation_str");         // ..
		e.ReadHintStr("static_out", "animation_str");        // ..
		
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
		in_cover.ReadU32("lookout_cooldown_min_far");     // NEW in Arktika.1 !!!
		in_cover.ReadU32("lookout_cooldown_max_far");     // ..
		in_cover.ReadFP32("lookout_cooldown_dist_near");  // ..
		in_cover.ReadBool("supperess_cover_enabled");     // ..
		in_cover.ReadBool("blind_fire_enabled");          // ..
		in_cover.ReadU32("suppress_after_hit_delay_min"); // ..
		in_cover.ReadU32("suppress_after_hit_delay_max"); // ..
		in_cover.ReadU32("suppress_out_delay_min");       // ..
		in_cover.ReadU32("suppress_out_delay_max");       // ..
		in_cover.ReadU8("min_suppress_fire_count");       // ..
		in_cover.ReadU8("max_suppress_fire_count");       // ..
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
		common.ReadU32("hide_type"); // NEW in Arktika.1 !!! 
		var alert = e.ReadSection("alert");
		alert.ReadBool("search_enabled");
		alert.ReadU32("timeout_go_search");
		alert.ReadU32("timeout_nervous_alert");
		alert.ReadU32("timeout_nervous_alert_uber");
		var scary = e.ReadSection("scary"); // empty section
		var panic = e.ReadSection("panic"); // empty section
		var human_species_behavior = e.ReadSection("human_species_behavior"); // empty section
		e.ReadSection("surrender"); // empty section
		e.ReadSection("neutral"); // empty section
		var gd = e.ReadSection("grenade_dodging");
		gd.ReadBool("grenade_dodging_enabled");
	}
	
	ReadHuman(e, ReadBrainUnit);
}

entity_readers["ANOMALY"] = function(e)
{
	function ReadBrain(e)
	{
		ReadAIBrainUnit(e, [])
	}
	
	ReadNpcBase(e, false, ReadBrain)
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

entity_readers["HUMANIMAL"] = function(e)
{
	function ReadBrainUnit(e)
	{
		var attacks_list = [
			"melee_attack_0",
			"melee_attack_1",
			"melee_attack_long_0",
			"melee_attack_long_1",
			"melee_vs_player_1",
			"melee_vs_player_2",
			"melee_vs_player_3",
			"melee_vs_player_4",
			"melee_vs_player_5",
		];
		
		ReadMonsterBrainUnit(e, attacks_list);
		
		e.ReadU8("thrower_type")
	}
	
	ReadNpcBase(e, false, ReadBrainUnit);
}

entity_readers["NPC_FX"] = ReadNpc

entity_readers["PLAYER"] = ReadPlayer

entity_readers["RAT"] = ReadSimpleNpc

entity_readers["WOMAN"] = ReadSimpleNpc

/*--------------------------------*/
/*   Inventory Items & Devices    */
/*--------------------------------*/
function ReadInventoryItemObject(e)
{
	// class inventory_item
	e.ReadU8("flags0", "bool8") // active, useful_for_player, ammo_for_player, dao_blink_prevent, ready_after_cloned, ui_force_slot, attached_loot
	e.ReadU16("trade_weight")
	e.ReadU8("ui_force_slot_id")

	ReadCEntity(e)
}

function ReadUpgradeItem(e)
{
	ReadInventoryItemObject(e)
	
	e.ReadString("upgrade_id")
}

function ReadStateBasedAnim(e)
{
	// state_based_anim::load
	var i = 0, arr = e.ReadArray("states")
	while(arr.More())
	{
		var s = arr.ReadSection(RecStr("rec_", i++, 4), false)
		s.ReadString("state")
		s.ReadHintStr("motion_name", "animation_str")
		
		var n = s.ReadSection("sound")
		n.ReadHintStr("name", "choose")
		n.ReadU32("id")
		n.ReadU32("ai_type")
		n.ReadBool("is_looped")
		n.ReadBool("do_interrupt")
		n.ReadFP32("accrue")
		
		s.ReadHintStr("track_name", "choose")
	}
}

function ReadWeaponItem(e)
{
	ReadUpgradeItem(e)
	
	e.ReadBool("vr_attach")
	e.ReadBool("free_on_level")
	
	ReadStateBasedAnim(e)
}

function ReadVrMissile(e)
{
	{ // vr_missile::load_dynamic
		{ // missile_weapon::load_dynamic
			e.ReadU16("count")
		}
		
		e.ReadFP32("vr_vel_coef_mul")
		e.ReadFP32("vr_vel_ang_coef_mul")
		e.ReadFP32("vr_ang_vel_coef_mul")
		e.ReadFP32("vr_ang_vel_damp_mul")
		e.ReadFP32("vr_ang_vel_thr_mul")
		e.ReadFP32("vr_ang_vel_max_mul")
		e.ReadFP32("vr_max_aim_angle", "angle, fp32")
		
		{ // citadel::weapon_description::load_dynamic
			e.ReadU8("desc_type")
			e.ReadHintStr("desc_name_key", "choose")
			e.ReadHintStr("desc_desc_key", "choose")
			e.ReadHintStr("desc_reload_key", "choose")
			e.ReadHintStr("desc_special_key", "choose")
			e.ReadHintStr("desc_special_upg_key", "choose")
			e.ReadS32("desc_special_upg_index")
			e.ReadS32("desc_cost")
			e.ReadU16("desc_dps")
			e.ReadU16("desc_capacity")
			e.ReadU16("desc_max_capacity")
			e.ReadU8("desc_sort_index")
			var i = 0, arr = e.ReadArray("desc_upgs")
			while(arr.More())
			{
				var s = arr.ReadSection(RecStr("rec_", i++, 4), false)
				s.ReadHintStr("desc_upg_name", "choose")
				s.ReadHintStr("desc_upg_desc", "choose")
				s.ReadHintStr("desc_upg_desc_02", "choose")
				s.ReadHintStr("desc_upg_desc_03", "choose")
				s.ReadS32("desc_upg_cost")
				s.ReadU16("desc_upg_dps")
				s.ReadFP32("desc_upg_damage_mul")
				s.ReadU16("desc_upg_capacity")
			}
		}
		
		e.ReadHint("restrictor_mask", "flags64")
		e.ReadU64("restrictor_mask")
		e.ReadFP32("restrictor_slowdown")		
	}
}

function ReadWeaponItemVrAttach(e)
{
	ReadWeaponItem(e)
	ReadVrMissile(e)
	
	e.ReadU8("attach_slot")
	e.ReadFP32("velosity_mul")
	e.ReadFP32("damage_mul")
	e.ReadHintStr("preview_model", "choose")
}

entity_readers["WEAPON_ITEM_MAGAZINE"] = function(e)
{
	ReadWeaponItem(e)
	
	e.ReadHintStr("own_anim_mainspring_pos", "animation_str")
	e.ReadHintStr("bone_part_clip", "part_str")
	e.ReadString("bone_patron_prefix")
	e.ReadString("s_bone_format")
}

entity_readers["WEAPON_ITEM_VR_ATTACH"] = ReadWeaponItemVrAttach

entity_readers["AMMO"] = function(e)
{
	ReadInventoryItemObject(e)
	e.ReadU16("box_value")
}

entity_readers["HELSING_ARROW"] = function(e)
{
	ReadInventoryItemObject(e)
	e.ReadU16("count")
}

entity_readers["WEAPON_FLARE"] = function(e)
{
	ReadInventoryItemObject(e)
	ReadVrMissile(e)
	e.ReadU16("box_value")
}

/*--------------------------------*/
/*            Weapons             */
/*--------------------------------*/

// really class weapon;
function ReadWeaponBase(e)
{
	e.ReadString("dlc_model_name")
	
	ReadInventoryItemObject(e)

	e.ReadBool("ignore_difficulty")
}

// really class weapon_magazined;
function ReadWeapon(e)
{
	ReadWeaponBase(e)

	e.ReadU32("ammo_loaded")
	e.ReadU32("ammo_balance")
	e.ReadU32("ammunition")
	
	ReadStateBasedAnim(e)
	
	e.ReadU8("bullets_in_barrel")
	e.ReadU8("flags0", "bool8") // name conflict with cinventory_item?
	
	var i = 0, u = e.ReadArray("upgrades");
	while(u.More())
	{
		var s = u.ReadSection(RecStr("rec_", i++, 4), false)
		s.ReadU8("is_installed")
		s.ReadString("upgrade_id")
		// не уверен что это всё
	}
}

entity_readers["WEAPON_AK_74"] = ReadWeapon
entity_readers["WEAPON_ASHOT"] = ReadWeapon
entity_readers["WEAPON_DECOY"] = function(e)
{
	ReadInventoryItemObject(e)
	ReadVrMissile(e)
}
entity_readers["WEAPON_DUPLET"] = function(e)
{
	ReadWeapon(e)
	ReadVrMissile(e)
}
entity_readers["WEAPON_DYNAMITE"] = function(e)
{
	ReadInventoryItemObject(e)
	ReadVrMissile(e)
	e.ReadU16("box_value")
}
entity_readers["WEAPON_FLAME_DYNAMITE"] = function(e)
{
	ReadInventoryItemObject(e)
	ReadVrMissile(e)
	e.ReadU16("box_value")
}
entity_readers["WEAPON_GATLING"] = function(e)
{
	ReadWeapon(e)
	e.ReadFP32("pressure")
}
entity_readers["WEAPON_HELLBREATH"] = function(e)
{
	ReadWeapon(e)
	e.ReadFP32("pressure")
	ReadVrMissile(e)
}
entity_readers["WEAPON_PADONAG"] = function(e)
{
	ReadWeapon(e)
	ReadVrMissile(e)
}
entity_readers["WEAPON_SAIGA"] = ReadWeapon
entity_readers["WEAPON_VENTIL"] = ReadWeapon

function ReadVRWeapon(e)
{
	ReadWeapon(e)
	e.ReadFP32("pressure")
	ReadVrMissile(e);
	e.ReadU32("noobe_ammo_loaded");
	
	[
		"Barrel",
		"Rotor",
		"Battery",
		"Scope",
		"Torch"
	].forEach(function(n) {
		var s = e.ReadSection(n)
		
		s.ReadVec3("size")
		s.ReadMatrix43("offset", "pose, matrix_43T")
	
		s.ReadHintStr("locator", "attp_str")
		s.ReadU16("module", "entity_link, uobject_link")
	});
}

entity_readers["VR_WEAPON"] = ReadVRWeapon
entity_readers["VR_WEAPON_CARVER"] = ReadVRWeapon
entity_readers["VR_WEAPON_SF4"] = ReadVRWeapon
entity_readers["VR_WEAPON_MODULAR"] = ReadVRWeapon

/*--------------------------------*/
/*           Vehicles             */
/*--------------------------------*/

// not exist in original game, but engine supports it
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
		system.ReadFP32("speed", "angle, fp32")
	}
}

/*--------------------------------*/
/*        Main function's         */
/*--------------------------------*/
function ReadStartup(s)
{
	function ReadPresetBlend(e)
	{
		e.ReadString("name")
		e.ReadBool("game_time")
		e.ReadBool("removing")
		e.ReadS32("start")
		e.ReadS32("finish")	
	}
	
	s.ReadU32("game_time")
	s.ReadFP32("dc_prev_speed")
	s.ReadFP32("dc_target_speed")
	s.ReadFP32("dc_curr_speed")
	s.ReadS32("dc_time_0")
	s.ReadS32("dc_time_1")
	s.ReadString("current")
	ReadPresetBlend(s)
	var modifiers = s.ReadArray("modifiers")
	for(var i = 0; modifiers.More(); i++)
		ReadPresetBlend(modifiers.ReadSection(RecStr("rec_", i, 4), false))
	s.ReadHintStr("weather_preset", "choose")
	ReadTime(s, "dc_start")
	ReadTime(s, "dc_duration")
	s.ReadHintStr("foliage_set", "choose")
	s.ReadFP32("foliage_fuzziness")
	s.ReadVec3("map_positional_min")
	s.ReadVec3("map_positional_max")
	s.ReadBool("map_positional_preview_aabb")
	s.ReadHintStr("map_menu_name", "choose")
	s.ReadU8("map_rotation")
	s.ReadHintStr("subst_tablet_model", "choose")
	s.ReadFP32("dao_base")
	s.ReadHintStr("next_level", "choose")
	s.ReadHintStr("back_music", "choose")
	var wil = s.ReadArray("weapon_items_link")
	for(var j = 0; wil.More(); j++)
		wil
			.ReadSection(RecStr("rec_", j, 4), false)
				.ReadU16("items", "entity_link, uobject_link")
}

function ReadEntities(entities)
{
	var already_listed = new Object
	while(entities.More())
	{
		var e = entities.ReadSection()
		
		// common params
		var _class = e.ReadStringCrc("class", typed_strings.get_class)
		var _static_data_key = e.ReadStringCrc("static_data_key", typed_strings.get_static_data_key)
		e.ReadHintStr("att_bone_id", "choose")
		e.ReadU16("id")
		e.ReadU16("parent_id")
		e.ReadMatrix43("att_offset", "pose, matrix_43T")
		e.ReadBool("att_root")
		
		//print(_class, " ", _static_data_key);
		
		if(entity_readers[_class])
		{
			entity_readers[_class](e)
	
			if(e.More())
				print(_class, " data left")
		}
		else
		{
			if(already_listed[_class] != true)
			{
				print("not implemented class ", _class)
				already_listed[_class] = true
			}
		}
	}
}

