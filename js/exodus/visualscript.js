// interface
this.ReadVssVer6 = ReadVssVer6
this.ReadBlock = ReadBlock

// implementation
var already_printed = new Object // not implemented classes
var vs_clsid = module("vs_clsid")
var entity_ver = 0;

function ReadVssVer6(e, entity_version)
{
	entity_ver = entity_version;

	var v = e.TryReadArray("vss_ver_6") || e.ReadArray("vss_ver_7")
	
	while(v.MoreElements())
	{
		var rec = v.NextElement()
		ReadVS(rec)
	}
}

function ReadVS(e)
{
	var groups = e.ReadArray("groups") // it's even used ???
	
	var blocks = e.ReadSection("blocks")
	
	blocks.ReadU16("version")
	blocks.ReadU32("block_count")

	var arr = blocks.ReadArrayWithNoKey('block_%.5d');
	while(arr.MoreElements())
	{
		var block = arr.NextElement()
		ReadBlock(block)
	}
	
	var link_count = e.ReadU32("link_count")
	
	for(var j = 0; j < link_count; j++)
		e.ReadVec4S16(j);
}

function ReadBlock(block)
{
	var clsid = block.ReadStringCrc("clsid", vs_clsid.get_clsid)
	block.ReadU16("posx")
	block.ReadU16("posy")
	
	var r = block_readers[clsid]
	
	if(r)
	{
		r(block)
		if(block.More())
			print("block " + clsid + " data left")
	}
	else
	{
		if(r !== null && !already_printed[clsid])
		{
			print("not implemented block class ", clsid, " (crc = 0x", crc32(clsid).toString(16), " size = ", block.Size(), ")")
			already_printed[clsid] = true
		}
	}	
}

// maybe create module for these functions shared with levelbin.js?
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
	e.ReadBool(name + "_dyn_state_exist")
	
	if(n.length > 0)
	{
		var arr = e.TryReadArray("exposed_blocks")
		if(arr) // don't exist in version 52
		{
			while(arr.MoreElements())
			{
				var b = arr.NextElement();
				b.ReadU16("blkid");
				ReadBlock(b);
			}
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

function ReadActionEngine3DText(e)
{
	// 3d_text
	e.ReadVec3("offset")
	e.ReadFP32("rotation")
	e.ReadHintStr("key", "choose")
	e.ReadFP32("size")
	e.ReadU8("h_alignment")
	e.ReadU8("v_alignment")
	e.ReadVec4("color", "color, vec4f")
	e.ReadVec4("rect")
	e.ReadU8("line_spacing")
	e.ReadFP32("curvature")
	e.ReadBool("subtitles")
	/* NEW in Exodus */ e.ReadHintStr("dbg_model", "choose")
	/* NEW in Exodus */ e.ReadString("dbg_skel")
	e.ReadHintStr("bone", "choose")
	e.ReadHintStr("font", "choose")
	e.ReadBool8("flags0", ["geometry_check", "forward_render", "glow"])
	e.ReadString("group_id")
}

function ReadActionHit(e)
{
	//e.ReadU16("initiator", "entity_link, uobject_link"); // REMOVED in Arktika.1
	e.ReadFP32("amount")
	e.ReadHintStr("bone", "choose_array, str_shared") // Arktika.1: changed 'bone_str' to 'choose_array, str_shared'
	e.ReadU8("hit_type")
	//e.ReadBool("forbid_ai") // REMOVED in Arktika.1
	
	if(entity_ver < 53)
		e.ReadBool8("flags", ["forbid_ai", "use_weapon"]) // NEW in Arktika.1 !!!
	else
		e.ReadBool8("flags", ["forbid_ai", "use_weapon", "forbid_sound"]) // NEW in Arktika.1 !!!
}

function ReadActionMove(e)
{
	e.ReadBool8("flags", ["keep_move", "ignore_rot_x", "ignore_rot_y", "ignore_rot_z", "ignore_pos_x", "ignore_pos_y", "ignore_pos_z", "use_dest_coords"])
	e.ReadBool8("ex_flags", ["offset_as_dest", "use_speed", "negate_vr_offset"])
	e.ReadU8("smooth_type")
	e.ReadVec3("offset")
	e.ReadVec3("offset_angle", "ang3f")
	ReadTime(e, "interp_time")
	//ReadTime(e, "delay_min") // REMOVED in Exodus !!!
	//ReadTime(e, "delay_max") // REMOVED in Exodus !!!
	e.ReadHintStr("object_attp_id", "attp_str")
}

function ReadActionPlayMotion(e)
{
	e.ReadHintStr("dbg_model", "choose")  // Arktika.1: changed 'ref_model' to 'choose'
	e.ReadString("dbg_skel")
	e.ReadHintStr("animation", "animation_str")
	e.ReadHintStr("bone_part", "part_str") // Arktika.1: changed 'part_id' to 'part_str'
	e.ReadFP32("blend")
	e.ReadFP32("speed")
	e.ReadU8("force_looped") // 0 = none, 1 = looped, 2 = not looped
	e.ReadBool8("flags", ["overlay", "inverted", "speed_replace", "force_update"])
}

function ReadActionPlayMotionControl(e)
{
	e.ReadHintStr("dbg_model", "choose") // Arktika.1: changed 'ref_model' to 'choose'
	e.ReadString("dbg_skel")
	e.ReadHintStr("anim_fwd", "animation_str")
	e.ReadHintStr("anim_bwd", "animation_str")		
	e.ReadHintStr("bone_part", "part_str") // Arktika.1: changed 'part_id' to 'part_str'
	e.ReadFP32("accel_fwd")
	e.ReadFP32("accel_bwd")
	//if(version < 27) e.ReadFP32("accel_pause")
	ReadTime(e, "time_to_pause_ms") // NEW in Arktika.1 !!!
	e.ReadFP32("spd_coef")
	e.ReadFP32("spd_coef_acc")
	e.ReadFP32("spd_coef_decc")
	e.ReadBool8("flags", ["force_looped", "fwd_inverted", "bwd_inverted"])
	e.ReadU16("fwd_start_frame") // NEW in Arktika.1 !!!
	e.ReadU16("fwd_end_frame")   // ..
	e.ReadU16("bwd_start_frame") // ..
	e.ReadU16("bwd_end_frame")   // ..
}

function ReadActionPlayParticles(e)
{
	e.ReadHintStr("particles", "choose")
	e.ReadBool8("particles_flags", ["start_as_free", "particles_constrained", "particles_ignore_parent_rotation", "deferred_stop", "mesh_source", "particles_ignore_parent_scale", "use_parent_color"])
	e.ReadU32("particles_color", "color, u32")
}

function ReadActionPlaySound(e)
{
	e.ReadHintStr("sound", "choose") // // Arktika.1: changed 'sound' to 'choose'
	e.ReadFP32("volume")
	e.ReadU8("sound_filter")
	e.ReadU8("sound_bus")
	e.ReadBool8("flags0", ["instant", "looped", "enable_slowmo", "enable_fx", "check_gasmask", "pad_out"])
	e.ReadU32("ai_sound_type")
	e.ReadFP32("stop_interval")
	e.ReadFP32("startus_intervalus")
	//e.ReadFP32("start_delay") // REMOVED in Exodus !!!
	e.ReadFP32("end_cb_offset")
	e.ReadBool("play_as_music")
}

function ReadActionPlayVibration(e)
{
	/*
	e.ReadFP32("force_feedback_0")
	e.ReadFP32("force_feedback_1")
	e.ReadFP32("step_ff0")
	e.ReadFP32("step_ff1")
	*/
	e.ReadBool("apply_force_feedback_0")
	e.ReadFP32("force_feedback_0")
	e.ReadFP32("step_ff0")
	e.ReadBool("apply_force_feedback_1")
	e.ReadFP32("force_feedback_1")
	e.ReadFP32("step_ff1")
}


function ReadActionSetInterest(e)
{
	e.ReadU8("einterest_mode")
	e.ReadU8("aim_mode")
	e.ReadU16("interest_object", "entity_link, uobject_link")
	ReadTime(e, "interest_duration") // Exodus: changed ReadU32 to ReadTime
	e.ReadFP32("speed_coef")
	e.ReadU8("eyes_speed")
	e.ReadFP32("max_angle")
	e.ReadFP32("clamp_angle")
	e.ReadFP32("look_probability")
	e.ReadU32("ignore_angle_timeout")
	//e.ReadBool("aim_interest"); // REMOVED in Exodus !!!
	e.ReadBool8("flags", ["aim_interest", "force_keep_interest"])         // NEW in Exodus !!!
	e.ReadHintStr("fx_anim", "choose") // ..
	ReadTime(e, "fx_start_min")        // ..
	ReadTime(e, "fx_start_max")        // ..
}

function ReadActionShowHudMenu(e)
{
	e.ReadHintStr("menu_name", "choose")
	e.ReadBool("set_position")
	e.ReadFP32("position_x")
	e.ReadFP32("position_y")
	e.ReadBool("set_scale")
	e.ReadFP32("scale")
	e.ReadBool("game_ui")
}

function ReadLogicAdvRandomex(e)
{
	e.ReadU8("quant")
	var count = e.ReadU8("states_count")
	for(var i = 0; i < count; i++)
	{
		e.ReadBool("on_"+i)
		e.ReadU8("wgt_"+i)
	}		
}

function ReadLogicCounter(e)
{
	e.ReadBool("auto_touch") // NEW in Arktika.1 !!!
	e.ReadS32("value")
}

function ReadLogicRandomChooser(e)
{
	e.ReadU8("quant")
	var count = e.ReadU8("prob_count")
	for(var i = 0; i < count; i++)
		e.ReadFP32("prob"+i)
}

function ReadLogicDelay(e)
{
	ReadTime(e, "min")
	ReadTime(e, "max")
	e.ReadBool8("dflags", ["one_at_a_time", "active"])
}

function ReadTrigger(e)
{
	e.ReadBool("active")
}

function ReadTrigger_ee(e)
{
	ReadTrigger(e)
	// e.ReadBool("player") // REMOVED in Arktika.1
	e.ReadU8("source_filter")
}

function ReadTriggerEntityOrientation(e)
{
	e.ReadFP32("fwd_ang", "angle, fp32")
	e.ReadFP32("bwd_ang", "angle, fp32")
	e.ReadFP32("lft_ang", "angle, fp32")
	e.ReadFP32("rgt_ang", "angle, fp32")
	e.ReadBool("is_dir_cmp_mode")
}

function ReadTriggerHit(e)
{
	ReadTrigger_ee(e)
	e.ReadHint("hit_types", "flags32") // NEW in Arktika.1 !!!
	e.ReadU32("hit_types")
	e.ReadBool("any_hit_type")
	e.ReadIdentifier("weapon_type")
	e.ReadHintStr("bone", "attp_str")
	e.ReadFP32("range_min")
	e.ReadFP32("range_max")
	e.ReadBool("fly_by")
}

function ReadTriggerZone(e)
{
	ReadTrigger(e)
	e.ReadS32("reaction_count")
	e.ReadBool("active_on_load") // NEW in Arktika.1 !!!
	e.ReadU16("zone_link", "entity_link, uobject_link")
}

function ReadTriggerZoneSpecial(e)
{
	ReadTrigger(e)
	e.ReadS32("reaction_count")
	e.ReadBool("active_on_load") // NEW in Arktika.1 !!!
	//e.ReadU16("zone_link", "entity_link, uobject_link")
	e.ReadHint("type_mask", "flags64")
	e.ReadU64("type_mask")
	e.ReadU8("max_refs")
	e.ReadBool8("flags8", ["nested_enter", "nested_leave"])
}

var block_readers = {
	"actions/action_check_slot": function(e)
	{
		e.ReadU8("slot")
	},
	"actions/action_cover_anchor": function(e)
	{
		e.ReadFP32("radius")
	},
	"actions/action_set_interest": ReadActionSetInterest,
	"actions/action_set_interest_ex": ReadActionSetInterest,
	"actions/action_pd_add": function(e)
	{
		e.ReadS32("points")
		e.ReadString("points_comments")
	},
	"actions/action_pd_check": function(e)
	{
		e.ReadS32("points")
	},
	"actions/action_pd_reset": null,
	"actions/action_unreachable_enemy_anchor": function(e)
	{
		e.ReadFP32("radius")
	},
	"actions/activate-deactivate": function(e)
	{
		e.ReadBool("register")
		if(entity_ver >= 53) e.ReadBool("force_dead") // NEW in Exodus !!!
	},
	"actions/actor movement": function(e)
	{
		e.ReadFP32("speed_factor_walk")
		e.ReadFP32("speed_factor_run")
		e.ReadFP32("speed_factor_sprint")
		e.ReadFP32("speed_factor_crouch")
		e.ReadFP32("speed_factor_jump")
		e.ReadFP32("speed_factor_accel")
		e.ReadFP32("speed_factor_brake")
		e.ReadBool("cam_limit")
		e.ReadVec2("cam_limit_pitch")
		e.ReadFP32("mouse_inertion")
		e.ReadBool("invert_mouse")
		e.ReadBool("ignore_cam_rot")
		e.ReadU32("blend_time")
		e.ReadU32("blend_time_out") // NEW in Arktika.1 !!!
	},
	"actions/actor movement ex": function(e) // NEW in Exodus !!!
	{
		e.ReadFP32("modif")
		e.ReadU32("blend")
		e.ReadU32("blend_out")
	},
	"actions/actor movement allowed": function(e)
	{
		e.ReadU8("walk")
		e.ReadU8("run")
		e.ReadU8("sprint")
		e.ReadU8("crouch")
		e.ReadU8("jump")
		e.ReadU32("blend_time")
		e.ReadU32("blend_time_out")
	},
	"actions/add_diary": function(e)
	{
		e.ReadU32("prop_id")
		e.ReadU8("idx")
		e.ReadBool("value")
		e.ReadString("comments")
	},
	"actions/add_postcard": function(e)
	{
		e.ReadU32("prop_id")
		e.ReadU8("idx")
		e.ReadBool("value")
		e.ReadString("comments")
	},
	"actions/ai/arahind female mode": null,
	"actions/ai/arahind jump": function(e)
	{
		e.ReadFP32("_max_height")
		e.ReadBool8("flags0", ["_disable_when_busy", "_find_path_to_jump", "_keep_ceil_on_light"])
		e.ReadFP32("_jump_search_radius")
	},
	"actions/ai/cover escape": function(e)
	{
		e.ReadFP32("escape_min_dist")
		e.ReadFP32("escape_max_dist")
		e.ReadU32("escape_cover_type")
		e.ReadU32("escape_cover_wait_min")
		e.ReadU32("escape_cover_wait_max")
		e.ReadBool("escape_play_cover_path")
		e.ReadFP32("return_min_dist")
		e.ReadFP32("return_max_dist")
		e.ReadU32("return_cover_type")
		e.ReadU32("return_lock_min")
		e.ReadU32("return_lock_max")
		e.ReadU32("angry_on_return_min")
		e.ReadU32("angry_on_return_max")
		e.ReadBool("return_play_cover_path")
		e.ReadU8("movement_type")
		e.ReadU8("arrival_type")
	},
	"actions/ai/cover lock": function(e)
	{
		e.ReadU32("_reuse_delay")
	},
	"actions/ai/enable_melee_attacks": function(e)
	{
		e.ReadHint("_attack_enabled", "flags32")
		e.ReadU32("_attack_enabled")
	},
	"actions/ai/follow": function(e)
	{
		e.ReadU16("leader", "entity_link, uobject_link")
		e.ReadU16("wait_point", "entity_link, uobject_link")
		e.ReadHint("flags", "flags32")
		e.ReadBool32("flags", ["ignore_combat", "should_face_leader", "hold_formation"], [0x1, 0x2, 0x4])
		e.ReadFP32("far_distance_min")
		e.ReadFP32("far_distance_max")
		e.ReadFP32("follow_distance_min")
		e.ReadFP32("follow_distance_max")
		e.ReadFP32("near_distance_min")
		e.ReadFP32("near_distance_max")
		e.ReadU32("near_delay_min")
		e.ReadU32("near_delay_max")
		e.ReadFP32("max_distance")
	},
	"actions/ai/lock_as_enemy": function(e)
	{
		e.ReadU32("lock_time")
	},
	"actions/ai/lock_melee_attack": null,
	"actions/ai/melee_attack_mask": function(e)
	{
		e.ReadHint("mask", "flags32")
		e.ReadU32("mask")
		e.ReadHint("ignore_mask", "flags32")
		e.ReadU32("ignore_mask")
	},
	"actions/ai/monster face enemy": function(e)
	{
		e.ReadBool8("flags0", ["_face_enemy_move", "_face_enemy_stand"])
		e.ReadFP32("_face_enemy_stand_speed")
	},
	"actions/ai/move": function(e)
	{
		e.ReadFP32("base_height")
		e.ReadU32("shape_type")
		e.ReadFP32("delta_height")
		e.ReadU32("surface_type")
		e.ReadBool8("flags", ["rotate_toward_goal"])
		e.ReadFP32("max_radius")
	},
	"actions/ai/state": function(e)
	{
		e.ReadString("body_state")
		e.ReadString("anim_state")
		e.ReadString("movement_type")
		e.ReadString("weapon_state")
		e.ReadString("action")
		e.ReadU16("target", "entity_link, uobject_link")
		e.ReadHint("flags", "flags32")
		e.ReadU32("flags")
		e.ReadFP32("anin_state_approach_speed")
		e.ReadFP32("approaching_accel")
	},
	"actions/ai/static_obstacle": function(e)
	{
		e.ReadFP32("radius") // Exodus: changed 'range' to 'radius'
		e.ReadBool("impassable") // NEW in Exodus !!!
		e.ReadFP32("cct")        // NEW in Exodus !!!
		e.ReadBool("dynamic")    // NEW in Exodus !!!
	},
	"actions/ai/stop_melee_attack": null,
	"actions/ai/sun_lumin_coef": function(e)
	{
		e.ReadFP32("_ai_sun_lumin_coef")
	},
	"actions/alert search enable": null,
	"actions/all_vision_params": function(e)
	{
		// почти как в ReadBaseBrainUnit, но без *_transition_time
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
	},
	"actions/allow attached move": null,
	"actions/animated_gunmode": null,
	"actions/attach": function(e)
	{
		e.ReadHintStr("dbg_model", "choose") // NEW in Arktika.1 !!!
		e.ReadString("dbg_skel") // NEW in Arktika.1 !!!
		//e.ReadU16("owner", "entity_link, uobject_link")
		e.ReadHintStr("bone", "attp_str")
		e.ReadMatrix43("offset", "pose, matrix_43T")
		e.ReadU8("attach_type") // NEW in Exodus !!! ( 0 = Auto Attach, 1 = Offset, 2 = Weapon Trade Offset )
		e.ReadBool8("flags", ["auto_scale", "center_bounding_box"])
	},
	"actions/attach vs": function(e)
	{
		e.ReadString("name")
		//e.ReadBool("disable_qsave") // REMOVED in Exodus !!!
		e.ReadBool8("save_flags", ["disable_qsave", "save_on_nextlevel"]) // NEW in Exodus !!!
		ReadVsRef(e, "vs") 
	},
	"actions/attack_hit_shell": null,
	"actions/body state": function(e) // NEW in Arktika.1 !!!
	{
		e.ReadU8("body_state") // ebody_state_token, смотри PDB
	},
	"actions/camera attach": function(e)
	{
		e.ReadHintStr("dbg_model", "choose")
		e.ReadString("dbg_skel")
		e.ReadHintStr("bone", "attp_str")
		e.ReadMatrix43("offset", "pose, matrix_43T")
		e.ReadU8("attach_type")
		e.ReadBool8("flags", ["auto_scale", "center_bounding_box"])
		
		e.ReadFP32("rotation_coef")
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
		e.ReadBool8("flags", ["use_rotation", "keep_dir", "precise_finish", "exclusive"])
		e.ReadFP32("use_rotation_speed")
	},
	"actions/cap_health": function(e)
	{
		e.ReadFP32("low_health_border")
		e.ReadFP32("high_health_border")
		e.ReadBool("full_protection")
		e.ReadBool("exclude_collision") // NEW in Redux!!
	},
	"actions/chained_humanimal": function(e)
	{
		e.ReadString("leader_group_name")
	},
	"actions/change_icon": function(e)
	{
		e.ReadHintStr("menu_name", "choose")
		e.ReadHintStr("iconstamp_name", "choose")
		e.ReadU8("def_idx")
		var arr = e.ReadArray("icons")
		while(arr.MoreElements())
		{
			var rec = arr.NextElement()
			rec.ReadHintStr("icon", "choose")
		}
	},
	"actions/change_max_speed_vehicle": function(e)
	{
		e.ReadFP32("coefficient")
		e.ReadU32("time")
	},
	"actions/collision_group": function(e)
	{
		e.ReadU8("collisions_group")
		e.ReadU8("setup_type")
	},
	"actions/common_combat": null,
	"actions/console command": function(e)
	{
		e.ReadHintStr("cmd", "choose") // Exodus: changed ReadString to ReadHintStr
	},
	"actions/console echo": function(e)
	{
		e.ReadHintStr("cmd", "choose")
	},
	"actions/cover type": function(e)
	{
		e.ReadU8("_cover_type")
	},
	"actions/cover_combat": function(e)
	{
		//e.ReadBool("make_cover_combat") // REMOVED in Exodus !!!
		e.ReadBool8("flags0", ["make_cover_combat", "_set_cover_dist", "_set_weight_delta"])   // NEW in Exodus !!!
		e.ReadFP32("_cover_min_dist") // ..
		e.ReadFP32("_common_max_dist") // ..
		e.ReadFP32("_weight_delta")   // ..
	},
	"actions/cover_task_params": function(e)
	{
		e.ReadU32("enemy_seen_timeout")
		e.ReadU32("lookout_min")
		e.ReadU32("lookout_max")
		e.ReadU32("aim_while_lookout_timeout")
		e.ReadU32("lookout_cooldown_min")
		e.ReadU32("lookout_cooldown_max")
		//e.ReadU32("lookout_cooldown_min_far") // REMOVED in Exodus !!!
		//e.ReadU32("lookout_cooldown_max_far") // REMOVED in Exodus !!!
		//e.ReadFP32("lookout_cooldown_dist_near") // REMOVED in Exodus !!!
	},
	"actions/destroy": function(e)
	{
		e.ReadBool("report_dead") // NEW in Arktika.1 !!!
	},
	"actions/destroy joint": null,
	"actions/detach": function(e)
	{
		e.ReadU16("owner", "entity_link, uobject_link")
		//e.ReadBool("check_parent") // REMOVED in Exodus !!!
	},
	"actions/detach_all": function(e)
	{
		e.ReadBool("recursively")
	},
	"actions/disable_all_use": function(e)
	{
		e.ReadString("reason")
	},
	"actions/disable_flinch": null,
	"actions/disable_jumpover": null,
	"actions/disable_npc_collision": function(e)
	{
		e.ReadHint("flags", "flags8")
		e.ReadBool8("flags", ["disable_rigid_bodies_collision", "disable_triggers_collision"], "u8") // really must be something like ReadBool16 (with masks), but this should work too
	},
	"actions/disable_sliding": null,
	"actions/dynamite/throw_grenade": null,
	"actions/engine/3d_text": ReadActionEngine3DText,
	"actions/engine/action cull distance": function(e)
	{
		e.ReadFP32("cull_distance")
	},
	"actions/engine/biom clone": function(e)
	{
		e.ReadU32("cover_reuse_delay")
		e.ReadU8("count")
		e.ReadU8("team")
		e.ReadBool8("flags0", ["spectator", "force_cover"])
		e.ReadU32("sources_count")
		e.ReadU8("ignore_mp_class_type")
		e.ReadU32("states_count")
		e.ReadU32("cur_state")
		var count = e.ReadU8("states_count_pre")
		e.ReadU8("max_refs")
		for(var i = 0; i < count; i++)
			e.ReadString("tag"+i);
	},
	"actions/engine/capture_video": function(e)
	{
		e.ReadBool("active")
		e.ReadString("folder_name")
		e.ReadString("seq_name")
	},
	"actions/engine/clone": function(e)
	{
		e.ReadU32("cover_reuse_delay")
		e.ReadU8("count")
		e.ReadString("tag") // NEW in Redux!!
		e.ReadU16("source", "entity_link, uobject_link")
		e.ReadU16("position", "entity_link, uobject_link")
		e.ReadU8("max_saved_cloned") // NEW in Arktika.1 !!! 
	},
	"actions/engine/clone attaches": null,
	"actions/engine/disable_photomode": function(e)
	{
		e.ReadBool("disable_only_fly")
	},
	"actions/engine/disable_save": function(e)
	{
		e.ReadString("disable_save_reason")
	},
	"actions/engine/discharge": function(e)
	{
		e.ReadHintStr("model", "choose") // Arktika.1: changed 'ref_model' to 'choose'
		e.ReadHintStr("coloranim", "choose")
		e.ReadFP32("var_life_time_from")
		e.ReadFP32("var_life_time_to")
		e.ReadFP32("var_scale_from")
		e.ReadFP32("var_scale_to")
	},
	"actions/engine/force env zone": function(e)
	{
		e.ReadHintStr("env_zone_name", "choose")
	},
	"actions/engine/make_ghost": null,
	"actions/engine/measure fps": null,
	"actions/engine/neversleeps": null,
	"actions/engine/outdoor sector": function(e)
	{
		e.ReadHintStr("sector_name", "choose")
	},
	"actions/engine/play video": function(e)
	{
		e.ReadHintStr("video_name", "choose")
	},
	"actions/engine/play_hud_particles": function(e)
	{
		e.ReadHintStr("particles", "choose") // Arktika.1: changed 'particles, str_shared' to 'choose'
		e.ReadU16("hud_particles_type")
		e.ReadBool8("particles_flags", ["particles_constrained", "particles_ignore_parent_rotation", "deferred_stop", "ignore_fov", "persistent", "allow_parent_velocity", "allow_in_vr", "allow_pp"]) 
		e.ReadU32("particles_color", "color, u32") // NEW in Exodus !!
	},
	"actions/engine/portal throughput": function(e)
	{
		e.ReadHintStr("portal_name", "choose")
		e.ReadFP32("throughput")
		e.ReadFP32("transition_time")
		e.ReadBool("geom_culling")
	},
	"actions/engine/presence": function(e)
	{
		e.ReadU16("context_location_id")
		e.ReadU16("context_action_id")
		e.ReadU16("context_string_id") // NEW in Exodus !!
	},
	"actions/engine/quick_save": function(e)
	{
		e.ReadString("filename")
	},
	"actions/engine/replace model": function(e)
	{
		e.ReadHintStr("model", "choose") // Arktika.1: changed 'ref_model' to 'choose'
		//e.ReadBool("preserve") // REMOVED in Exodus !!!
		e.ReadBool("restart_as") // NEW in Exodus !!!
	},
	"actions/engine/replace tpreset": function(e)
	{
		e.ReadHintStr("dbg_model", "choose")  // NEW in Exodus !!!
		e.ReadString("dbg_skel")              // NEW in Exodus !!!
		//e.ReadBool("as_preset")             // REMOVED in Exodus !!!
		e.ReadHintStr("tex_preset", "choose")
	},
	"actions/engine/scale": function(e) // NEW in Redux!!
	{
		e.ReadVec3("scale")
		e.ReadBool8("flags1", ["absolute", "apply_to_children"]) // NEW in Exodus !!!
	},
	"actions/engine/scene_hud": function(e)
	{
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
	},
	"actions/engine/sector visible": function(e)
	{
		e.ReadHintStr("sector_name", "choose")
	},
	"actions/engine/set background music": function(e)
	{
		e.ReadHintStr("back_music", "choose")
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
		e.ReadFP32("volume")
		e.ReadBool("start_at_random") // NEW in Arktika.1 !!!	
	},
	"actions/engine/set env layer": function(e)
	{
		e.ReadHintStr("zone", "choose")
		e.ReadHintStr("layer", "choose")
	},
	"actions/engine/set_texture_aux": function(e) // NEW in Exodus !!!
	{
		e.ReadHintStr("texture_name", "choose")
		e.ReadBool("enabled_x")
		e.ReadBool("enabled_y")
		e.ReadBool("enabled_z")
		e.ReadBool("enabled_w")
		e.ReadU8("aux_index")
		e.ReadBool("restore_defaults")
		e.ReadBool("add_mode")
		e.ReadVec4("target_val", "color, vec4f")
		e.ReadHintStr("color_anim", "choose")
		e.ReadBool("looped")
		e.ReadVec4i("blend_time")
	},
	"actions/engine/set_weather": function(e)
	{
		e.ReadHintStr("preset_name", "choose")
		e.ReadFP32("blend_time")
		e.ReadFP32("falloff")
		e.ReadBool("time_mode")
	},
	"actions/engine/shadowcast": null,
	"actions/engine/show_death_menu": function(e)
	{
		e.ReadString("hint_prefix")
		e.ReadU8("hint_range")
	},
	"actions/engine/show_menu": function(e)
	{
		e.ReadBool("show_only_menu")
		e.ReadHintStr("menu_name", "choose")
	},
	"actions/engine/signal": function(e)
	{
		e.ReadHintStr("signal", "choose")
		e.ReadU8("mode")
	},
	"actions/engine/slowmo": function(e)
	{
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
		e.ReadFP32("value")
		e.ReadFP32("current_value")
		e.ReadFP32("distance")  // NEW in Exodus !!!
		e.ReadBool("active")
		e.ReadBool("registered")
	},
	"actions/engine/text_hud": function(e)
	{
		e.ReadVec2("position")
		e.ReadVec2("size")
		e.ReadVec2("icon_position")
		e.ReadHintStr("text", "choose")
		e.ReadVec4("color", "color, vec4f")
		e.ReadU8("font_size")
		e.ReadU32("appearance_time")
		e.ReadU32("ttl")
		e.ReadU32("falloff_time")
		e.ReadU8("type")
		e.ReadString("group_id_str")
		e.ReadU32("icon_type")
		e.ReadU8("h_alignment")
		e.ReadU8("v_alignment")
		e.ReadU8("team_visibility")
		e.ReadU8("h_alignment_i")
		e.ReadU8("v_alignment_i")
		e.ReadVec4("nick_color", "color, vec4f")
		e.ReadU8("nick_font_size")
		e.ReadU8("nick_position")
		e.ReadU8("nick_space_count")
		e.ReadVec2("icon_size")
		e.ReadBool8("flags", ["instant", "text_depend", "abs_coorsds", "icon_draw_first", "use_nickname", "swap_nick_text", "blink_mode"])
	},
	"actions/engine/texture prestream": function(e)
	{
		e.ReadHintStr("textures", "choose_array, str_shared")
	},
	"actions/engine/volume": function(e)
	{
		e.ReadFP32("volume")
		ReadTime(e, "time") // Exodus: changed ReadU32 to ReadTime
		e.ReadU8("bus")
	},
	"actions/engine/wipe_hud_particles": null,
	"actions/entity/anim param": function(e)
	{
		e.ReadHintStr("dbg_model", "choose")
		e.ReadString("dbg_skel")
		e.ReadHintStr("param", "param_str") // param_str что-то новенькое 
		e.ReadFP32("value")
		ReadTime(e, "time")
		e.ReadU8("smooth_type")
	},
	"actions/entity/hud_collision": null,
	"actions/entity/ignore in pickup": null,
	"actions/entity/interest_info": function(e)
	{
		ReadInterest(e)
	},
	"actions/entity/model_track": function(e)
	{
		e.ReadString("container_name")
	},
	"actions/entity/move_restore": function(e)
	{
		ReadActionMove(e)
		e.ReadMatrix43("movepos", "pose, matrix")
	},
	"actions/entity/override-material": function(e)
	{
		e.ReadHintStr("preset", "choose")
	},
	"actions/entity/add gesture": function(e)
	{
		e.ReadHintStr("dbg_model", "choose") // Arktika.1: changed 'ref_model' to 'choose'
		e.ReadString("dbg_skel")
		e.ReadHintStr("seq_name", "animation_str")
		e.ReadHintStr("bone_part", "part_str") // Arktika.1: changed 'part_id' to 'part_str'
		e.ReadU8("force_looped")
	},
	"actions/entity/flock params": function(e)
	{
		// same as in levelbin.js function ReadFlock
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
	},
	"actions/entity/foliage push": function(e)
	{
		e.ReadFP32("foliage_push_power")
		e.ReadFP32("foliage_push_range")
	},
	"actions/entity/set aux value": function(e)
	{
		e.ReadHintStr("coloranim", "choose") // Arktika.1: changed 'ref_coloranim' to 'choose'
		e.ReadBool("looped")
		e.ReadBool("enabled_x")
		e.ReadBool("enabled_y")
		e.ReadBool("enabled_z")
		e.ReadVec4("color", "color, vec4f")
		e.ReadVec3i("blend_time")
		e.ReadBool("add_mode") // NEW in Arktika.1 !!!
	},
	"actions/entity/set dao value": function(e)
	{
		e.ReadU32("dao_val")
		e.ReadBool8("flags", ["dao_auto", "dao_imm"])
	},
	"actions/entity/set health": function(e)
	{
		e.ReadFP32("health")
	},
	"actions/entity/shadowed": null,
	"actions/entity/show bone": function(e)
	{
		e.ReadHintStr("dbg_model", "ref_model") // Arktika.1: changed 'ref_model' to 'choose'
		e.ReadString("dbg_skel")
		e.ReadHintStr("bone_name", "choose_array, str_shared") // Arktika.1: changed 'bone_str' to 'choose_array, str_shared'
		e.ReadBool("show")		
	},
	"actions/entity/useful for player": function(e)
	{
		e.ReadBool8("flags0", ["flag", "check_attached_loot", "can_dismantle"])
	},
	"actions/entity/wallmark": function(e)
	{
		// like ReadWallmark in levelbin.js
		e.ReadU8("tex_index")
		e.ReadHint("marks", "tex_frame, u16*, u32")
		e.ReadU16Array("marks")
		e.ReadFP32("marks_size")
		e.ReadHint("marks_flags", "flags16")
		e.ReadU16("marks_flags")
		e.ReadVec4("aux_params")
	},
	"actions/entity/waves_emitter": function(e)
	{
		ReadWavesEmitterData(e)
	},
	"actions/explode": function(e)
	{
		e.ReadFP32("amount")
		e.ReadU8("hit_type")
		e.ReadBool("visual_off") // NEW in Arktika.1 !!!
	},
	"actions/ext/play_particles_ex": function(e)
	{
		ReadActionPlayParticles(e)
		e.ReadHintStr("locator", "locator_str")
		e.ReadBool("force_y")
		e.ReadFP32("force_y_val")
		e.ReadMatrix43("offset", "pose, matrix_43T")
	},
	"actions/extinguish": function(e)
	{
		e.ReadVec3("box_size")
		e.ReadBool8("flags", ["extinguish_target", "extinguish_box"])
	},
	"actions/find_cover": function(e)
	{
		// vs::action_goto_target::load_dynamic
		e.ReadFP32("target_range")
		e.ReadFP32("not_going_range")
		e.ReadU8("goto_movement_type")
		e.ReadU8("arrival_type")
		e.ReadBool8("flags", ["do_not_unlock_cover", "need_turn", "exact_rotation", "int_light_damage", "int_heavy_damage"])
		e.ReadFP32("distance_to_cover") // NEW in Exodus !!!
		e.ReadFP32("approach_range")    // NEW in Exodus !!!
		//
		
		e.ReadFP32("min_dist")
		e.ReadFP32("max_dist")
		e.ReadU32("cover_type")
		e.ReadU32("cover_type_id")      // NEW in Exodus !!!
		e.ReadBool("need_direct_path")  // NEW in Exodus !!!
	},
	"actions/force actor state": function(e)
	{
		e.ReadBool("crouch")
	},
	"actions/force_field": function(e)
	{
		e.ReadU32("ff_version")
		
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
	},
	"actions/force_free": null,
	"actions/force_sliding": function(e)
	{
		e.ReadBool("force_trigger")
	},
	"actions/freegun": null,
	"actions/game/set_save_icon": function(e)
	{
		e.ReadU8("save_icon_data_type")
		e.ReadString("save_icon_data_arg")
	},
	"actions/game/set_tablet_preset": function(e) 
	{
		e.ReadU8("target_tablet_preset")
	},
	"actions/game_time/set_time": function(e) // NEW in Exodus !!!
	{
		e.ReadU16("day")
		ReadTime(e, "time")
	},
	"actions/game_time/set_time_speed": function(e) // NEW in Exodus !!!
	{
		e.ReadFP32("target")
		ReadTime(e, "interval")
	},
	"actions/game_time/store_time": null,
	"actions/gasmask hit": ReadActionHit,
	"actions/goto_target": function(e)
	{
		e.ReadFP32("target_range")
		e.ReadFP32("not_going_range")
		e.ReadU8("goto_movement_type")
		e.ReadU8("arrival_type")
		e.ReadBool8("flags", ["do_not_unlock_cover", "need_turn", "exact_rotation", "int_light_damage", "int_heavy_damage"])
		e.ReadFP32("distance_to_cover") // NEW in Arktika.1 !!!
		e.ReadFP32("approach_range") // NEW in Arktika.1 !!!
	},
	"actions/hide mode": function(e)
	{
		e.ReadU8("hide_mode")
	},
	"actions/hint": function(e)
	{
		e.ReadHintStr("menu_name", "choose")
		e.ReadBool("set_position")
		e.ReadFP32("position_x")
		e.ReadFP32("position_y")
		e.ReadBool("set_scale")
		e.ReadFP32("scale")
		e.ReadBool("game_ui")
		
		e.ReadHintStr("hint", "choose")
		e.ReadBool8("flags", ["undisabled", "high_priority"])
		e.ReadHintStr("submit_caption", "choose")
		e.ReadHintStr("submit_text", "choose")
	},
	"actions/hit": ReadActionHit,
	"actions/hud item controller": null,
	"actions/human/active_item": null,
	"actions/human/aim_lag": function(e)
	{
		e.ReadU32("_aim_min_time_lo")
		e.ReadU32("_aim_min_time_hi")
	},
	"actions/human/allow weapon change": function(e)
	{
		e.ReadBool("allow")
	},
	"actions/human/animated_holder_mng": function(e) // NEW in Exodus !!!
	{
		e.ReadHintStr("dbg_model", "choose")
		e.ReadString("dbg_skel")
		e.ReadHintStr("holder_name", "attp_str")
	},
	"actions/human/combat behaviour": function(e)
	{
		e.ReadU32("_behaviour_type")
	},
	"actions/human/combat cooldowns": function(e)
	{
		e.ReadU32("ca_def_cooldown")
		e.ReadU32("ca_def_group_cooldown")
		e.ReadU32("ca_scared_to_cover_cooldown")
		e.ReadU32("ca_scared_to_cover_group_cooldown")
		e.ReadU32("ca_ambush_cover_cooldown")
		e.ReadU32("ca_ambush_cover_group_cooldown")
		e.ReadU32("ca_dodging_cooldown")
		e.ReadU32("ca_dodging_group_cooldown")
		e.ReadU32("ca_somersault_cooldown")
		e.ReadU32("ca_somersault_group_cooldown")
		e.ReadU32("ca_ambush_cooldown")
		e.ReadU32("ca_ambush_group_cooldown")
		e.ReadU32("ca_weapon_change_cooldown")
		e.ReadU32("ca_weapon_change_group_cooldown")
		e.ReadU32("ca_flanking_cooldown")
		e.ReadU32("ca_flanking_group_cooldown")
	},
	"actions/human/combat_type": function(e)
	{
		e.ReadU8("combat_type")
		
		// NEW in Exodus !!!
		e.ReadHint("disabled_actions", "flags32") // scared_to_cover, ambush_cover, ambush, dodging, somersault, weapon_change, flanking
		e.ReadBool32(
			"disabled_actions",
			["scared_to_cover", "ambush_cover", "ambush",    "dodging",   "somersault", "weapon_change", "flanking"],
			[ 0x00000002,        0x00000004,     0x00000020,  0x00000008,  0x00000010,   0x000000C0,      0x00000100],
			"u32"
		)
	},
	"actions/human/enable danger delta": null,
	"actions/human/enable panic": null,
	"actions/human/enable sniper": null,
	"actions/human/enable wounded combat": null,
	"actions/human/enable_surrender": null,
	"actions/human/fire_shooting_target": null,
	"actions/human/fire_while_moving": null,
	"actions/human/melee only": null,
	"actions/human/neutral": function(e)
	{
		e.ReadU32("min_reaction_time")
		e.ReadU32("max_reaction_time")
		e.ReadBool8("neutral_flags", ["check_visible", "check_real_visible"])
	},
	"actions/human/set_fire_target": function(e)
	{
		e.ReadBool("_ai_sound")
	},
	"actions/human/set_speech_name": function(e)
	{
		e.ReadU8("speech_name")
	},
	"actions/human/suppress cover": function(e)
	{
		e.ReadBool8("flags0", ["_force_suppress_cover", "_blind_fire", "_enable_auto_aim"]) // NEW in Exodus !!!
	},
	"actions/human/surrender": function(e)
	{
		e.ReadU8("force_surrender")
	},
	"actions/human/use_shooting_target": null,
	"actions/human/weapon change": function(e)
	{
		e.ReadBool("ranged")
	},
	"actions/luminocity": null,
	"actions/make friend": function(e)
	{
		e.ReadU8("make_friend_type")
	},
	"actions/make immortal": function(e)
	{
		e.ReadBool("check_only")
	},
	"actions/make invul": function(e)
	{
		e.ReadBool("check_only")
		e.ReadBool("vulnerable_to_player") // NEW in Exodus !!!
	},
	"actions/make scripted": function(e)
	{
		e.ReadBool("make_scripted")
	},
	"actions/map_text": function(e)
	{
		e.ReadHintStr("key", "choose")
		e.ReadHintStr("mark_name", "choose")
		e.ReadU8("type")
		e.ReadU8("order")
		e.ReadBool("notify")
		e.ReadString("group")
	},
	"actions/map_text ex": function(e)
	{
		e.ReadU8("preset_id")
		e.ReadHintStr("key", "choose")
		e.ReadHintStr("mark_name", "choose")
		e.ReadU8("type")
		e.ReadU8("order")
		e.ReadBool("notify")
		e.ReadString("group")
	},
	"actions/menu_mode": null,
	"actions/min mental state": function(e)
	{
		e.ReadU8("mental_state")
		e.ReadU8("max_mental_state")
	},
	"actions/monster/action_pathfind_collision": null,
	"actions/monster/aqua_female_state": function(e)
	{
		e.ReadU8("state")
	},
	"actions/monster/aqua_monster_mode_new": function(e)
	{
		e.ReadU8("am_state")
		e.ReadBool("tc")
	},
	"actions/monster/arahind_state": function(e)
	{
		e.ReadU8("state")
		e.ReadU32("state_timer")
	},
	"actions/monster/arm_gunner": null,
	"actions/monster/change_catfish_depth": function(e)
	{
		e.ReadFP32("destination")
		e.ReadU32("duration")
		e.ReadBool("hold_depth_state")
	},
	"actions/monster/dog": function(e)
	{
		e.ReadHint("flags", "flags32")
		e.ReadBool32("flags", ["ignore_combat", "dog_vision_params"], [0x01, 0x04], "u32")
		e.ReadFP32("min_distance")
		e.ReadFP32("forward_max_distance")
		e.ReadFP32("left_max_distance")
		e.ReadFP32("right_max_distance")
		e.ReadFP32("back_max_distance")
		e.ReadFP32("heel_forward_distance")
		e.ReadFP32("heel_right_distance")
		e.ReadFP32("lost_distance")
		e.ReadFP32("smell_distance")
		e.ReadFP32("trail_max_time")
		e.ReadFP32("leader_alert_dist")
		e.ReadFP32("dog_alert_dist")
		e.ReadFP32("footstep_valid_time")
		e.ReadU32("state")
	},
	"actions/monster/group_behaviour_allowed": null,
	"actions/monster/group_behaviour_params": function(e)
	{
		e.ReadU8("_group_model")
		e.ReadFP32("_orbit_radius")
	},
	"actions/monster/set_gunner_type": function(e)
	{
		e.ReadU8("_gunner_type")
		e.ReadU32("_gunner_shot_cooldown")
		e.ReadFP32("_gunner_orbit_radius")
	},
	"actions/monster/set_lurkers_state": function(e)
	{
		e.ReadU8("state")
		e.ReadBool("_force_nolair_attack")
		e.ReadBool("_const_nolair_attack") // NEW in Exodus !!!
	},
	"actions/monster/set_orient_point": null,
	"actions/monster/set_thrower_type": function(e)
	{
		e.ReadU8("_thrower_type")
		e.ReadFP32("_thrower_orbit_radius")
		e.ReadFP32("_thrower_melee_radius")
	},
	"actions/monster/snake_pursue": function(e)
	{
		e.ReadFP32("min_distance")
		e.ReadFP32("max_distance")
		e.ReadFP32("fail_distance")
		e.ReadFP32("max_y_offset")
	},
	"actions/monster/snake_range_attack": function(e)
	{
		e.ReadHintStr("model", "choose")
		e.ReadString("dbg_skel")
		e.ReadHintStr("in_motion", "animation_str")
		e.ReadHintStr("out_motion", "animation_str")
		e.ReadHintStr("attack0_motion", "animation_str")
		e.ReadHintStr("attack1_motion", "animation_str")
		e.ReadHintStr("attack2_motion", "animation_str")
		e.ReadHintStr("attack3_motion", "animation_str")
		e.ReadHintStr("attack4_motion", "animation_str")
		e.ReadHintStr("crit_motion", "animation_str")
		e.ReadHintStr("interrupt_motion", "animation_str")
		e.ReadFP32("min_distance")
		e.ReadFP32("max_distance")
	},
	"actions/monster/snake_reach_out": function(e)
	{
		e.ReadHintStr("model", "choose")
		e.ReadString("dbg_skel")
		e.ReadHintStr("motions", "choose_array, str_shared")
		e.ReadHintStr("locator", "choose")
		e.ReadHintStr("idle_motion", "animation_str")
		e.ReadFP32("min_distance")
		e.ReadFP32("max_distance")
		e.ReadFP32("velocity")
		e.ReadFP32("acceleration")
		e.ReadHint("flags", "flags32")
		e.ReadU32("flags")
		e.ReadString("request")
	},
	"actions/monster/stick_to": function(e)
	{
		e.ReadHintStr("model", "choose")
		e.ReadString("dbg_skel")
		e.ReadHintStr("motions", "choose_array, str_shared")
		e.ReadHintStr("locator", "choose")
		e.ReadFP32("max_test_dist")
		ReadTime(e, "fly_time")
		e.ReadFP32("fly_dist")
		e.ReadFP32("fly_dh")
	},
	"actions/move": ReadActionMove,
	"actions/move2cover": function(e)
	{
		ReadActionMove(e)
		
		e.ReadFP32("min_distance")
		e.ReadFP32("max_distance")
		e.ReadU8("cover_type")
		e.ReadU8("follow_link")
		e.ReadU32("cover_type_id") // NEW in Exodus !!!
		e.ReadBool("strict_lock")
		e.ReadBool("accessibility_check")
		e.ReadBool("lock") // NEW in Exodus !!!
	},
	"actions/movement type": function(e)
	{
		e.ReadU8("min_movement_type")
		e.ReadU8("max_movement_type")
		e.ReadFP32("step_acceleration")
	},
	"actions/net/action_objective_complete": function(e)
	{
		// same as actions/net/net_prop
		e.ReadU32("prop_id")
		e.ReadS32("prop_delta")
		e.ReadString("comments")
	},
	"actions/net/net_prop": function(e)
	{
		e.ReadU32("prop_id")
		e.ReadS32("prop_delta")
		e.ReadString("comments")
	},
	"actions/net/net_prop_bit": function(e)
	{
		// btw same as actions/add_postcard
		e.ReadU32("prop_id")
		e.ReadU8("idx")
		e.ReadBool("value")
		e.ReadString("comments")
	},
	"actions/nonnpc_obstacle": function(e)
	{
		e.ReadFP32("_radius")
		e.ReadU32("_points")
	},
	"actions/no_combat": function(e)
	{
		e.ReadBool("make_no_combat")
	},
	"actions/npc can play motion": null,
	"actions/npc/action feelings": function(e)
	{
		e.ReadBool8("flags", ["vision", "sound"])
	},
	"actions/npc/anim_speed": function(e)
	{
		e.ReadFP32("anim_speed")
	},
	"actions/npc/auto interest group": function(e)
	{
		e.ReadString("group_id")
		e.ReadBool("relax_mode")
	},
	"actions/npc/cancel_vs": null,
	"actions/npc/check_movement": function(e)
	{
		e.ReadFP32("dist")
		e.ReadFP32("angle", "angle, fp32")
		e.ReadBool8("flags0", ["check_npc", "only_obstacles"]) // not sure if it is correct lol
	},
	"actions/npc/choose_priority_target": function(e)
	{
		e.ReadBool8("ch_flags0", ["los", "visible", "visible_strict"])
		
		e.ReadFP32("dist_factor")
		e.ReadFP32("dist_min")
		e.ReadFP32("dist_max")
		
		e.ReadFP32("protected_factor")
		e.ReadFP32("protected_min")
		e.ReadFP32("protected_max")
		
		e.ReadFP32("mental_factor")
		
		e.ReadFP32("health_factor")
		
		e.ReadFP32("attacker_factor")
	},
	"actions/npc/cover group": function(e)
	{
		e.ReadHintStr("cover_group", "choose")
		e.ReadU8("allow")
		e.ReadU8("forbid")
	},
	"actions/npc/damage handle preset": function(e)
	{
		e.ReadU32("_preset_id")
	},
	"actions/npc/dispersion": function(e)
	{
		e.ReadFP32("amount")
	},
	"actions/npc/dispersion_props": function(e)
	{
		e.ReadU32("dispersion_decrease_time")
		e.ReadU32("dispersion_increase_time")
		e.ReadFP32("min_shoot_dispersion_coef")
		e.ReadFP32("max_shoot_dispersion_coef")
	},
	"actions/npc/enable scary": null,
	"actions/npc/flinch_clear": null,
	"actions/npc/forbid_melee_kill": null,
	"actions/npc/force_ragdoll_death": null,
	"actions/npc/free2go": null,
	"actions/npc/gasmask_voice": null,
	"actions/npc/get group leader": null,
	"actions/npc/group": function(e)
	{
		e.ReadString("group_id")
	},
	"actions/npc/group leader": null,
	"actions/npc/groups members": function(e)
	{
		e.ReadString("_group_name")
	},
	"actions/npc/hit_power": function(e)
	{
		e.ReadFP32("power")
	},
	"actions/npc/illuminated_react_level": function(e)
	{
		e.ReadFP32("illuminated_react_level")
	},
	"actions/npc/immunities": function(e)
	{
		[
			"burn", 
			"shock", 
			"collision", 
			"wound", 
			"explosion", 
			"fire_wound", 
			"ignore", 
			"suffocation", 
			"ph_shock",
			"mul_by_mass",
			"radiation",
			"trap",
			"water",
			"suicide",
			"shrapnel",
			"dirt",
			//"max",
			//"scripted",
			"lian_slap",
			"ultrasound",
			"aqua_sputum",
			"bees",
			"poison",
			"pin",
			"spider_sputum",
			"electro_anomaly"
			//"scripted_max",
			//"invalid"
		].forEach(function(hit) { e.ReadFP32(hit); })
	},
	"actions/npc/lock_vs": null,
	"actions/npc/lock_vs_block": null,
	"actions/npc/luminocity correction": function(e)
	{
		e.ReadFP32("_coef")
	},
	"actions/npc/obstacle": function(e)
	{
		e.ReadHintStr("obstacle", "choose")
		e.ReadBool("add_obstacle")
		e.ReadBool("check_objects")
	},
	"actions/npc/register_motion_sensor": null,
	"actions/npc/register_motion_sensor_adv": function(e)
	{
		e.ReadU8("item_type")
	},
	"actions/npc/reload": function(e) // NEW in Redux!!
	{
		e.ReadBool("if_need")
	},
	"actions/npc/remove_from_lvl_mental": null,
	"actions/npc/restrictor_block_ai_vision": function(e)
	{
		e.ReadU8("block_ai_vision")
	},
	"actions/npc/set group leader": null,
	"actions/npc/skip crosshair test": null,
	"actions/npc/skip stick to ai map": null,
	"actions/npc/wounded": null,
	"actions/npc/wounded_params": function(e)
	{
		ReadTime(e, "wounded_time")
		e.ReadFP32("wounded_probability")
	},
	"actions/one shot": function(e)
	{
		e.ReadBool8("flags0", ["ai_sound", "skip_delta", "disable_effects"])
	},
	"actions/open fire": function(e)
	{
		e.ReadU16("target", "entity_link, uobject_link");
		e.ReadU32("min_queue_size");
		e.ReadU32("max_queue_size");
		e.ReadU32("min_queue_interval");
		e.ReadU32("max_queue_interval");	
		//e.ReadBool("instant") // REMOVED in Arktika.1 !!!
		// NEW in Arktika.1 !!!
		e.ReadBool8("flags0", ["instant", "ignore_ammo", "ai_sound", "force_auto_aim"]);
	},
	"actions/p-force": function(e)
	{
		e.ReadFP32("min_amount")
		e.ReadFP32("max_amount")
		e.ReadFP32("max_influence_dst")
		e.ReadString("bone")
		e.ReadVec3("dir")
		//if(version < 32) e.ReadBool8("flags0", ["instant", "in_local_coords", "single_force"])
		e.ReadBool8("flags0", ["instant", "in_local_coords", "single_force", "as_torque"])
	},
	"actions/park_vehicle": null,
	"actions/particles color blend": function(e)
	{
		e.ReadHintStr("coloranim", "choose")
		e.ReadBool("looped")
		e.ReadBool("enabled_r")
		e.ReadBool("enabled_g")
		e.ReadBool("enabled_b")
		e.ReadBool("enabled_a")
		e.ReadVec4("color", "color, vec4f")
		e.ReadVec4i("blend_time")
		e.ReadBool("add_mode")
	},
	"actions/pathfind_check": function(e)
	{
		e.ReadFP32("pos_xz_threshold")
	},
	"actions/pfnn state": function(e)
	{
		e.ReadU8("free_walk_type")
	},
	"actions/physx_vehicle": function(e)
	{
		e.ReadU16("uobj_version")
		e.ReadFP32("max_rpm")
		e.ReadFP32("max_torque")
		e.ReadU32("blend_time")
	},
	"actions/platform_attach": null,
	"actions/platform_deattach": null,
	"actions/play camera-effect": function(e)
	{
		e.ReadHintStr("cameraeffect", "choose") // Arktika.1: changed 'camera_track, str_shared' to 'choose'
		e.ReadFP32("value")
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
		e.ReadFP32("speed") // NEW in Exodus !!!
		e.ReadFP32("inc_delta")
		e.ReadFP32("inc_speed")
		e.ReadFP32("dec_delta")
		e.ReadFP32("dec_speed")
		e.ReadFP32("max_dist")
		e.ReadFP32("min_fade")
		e.ReadBool8("flags0", ["exponential", "directional", "check_visible", "single_instance", "restart_if_single", "stop_all_tracks"])
		e.ReadU16("max_value_point", "entity_link, uobject_link")
		ReadTime(e, "vis_fade_in") // NEW in Arktika.1 !!!
		ReadTime(e, "vis_fade_out") // NEW in Arktika.1 !!!
	},
	"actions/play coloranim": function(e)
	{
		e.ReadHintStr("coloranim", "choose")  // Arktika.1: changed 'ref_coloranim' to 'choose'
		e.ReadBool("looped")
		e.ReadBool("play_from_end")
		e.ReadU8("post_action") // NEW in Exodus !!!
	},
	"actions/play cover path": function(e)
	{
		e.ReadBool("fast_switch")
		e.ReadBool("fast_callback")
		e.ReadBool("take_last_cover")
	},
	"actions/play cover path ex": function(e)
	{
		e.ReadBool("fast_switch")
		e.ReadBool("fast_callback")
		e.ReadBool("take_last_cover")
	},
	"actions/play modifier": function(e)
	{
		e.ReadHintStr("dbg_model", "choose")
		e.ReadString("dbg_skel")
		e.ReadHintStr("attp", "attp_str")
		e.ReadHintStr("modifier", "choose")
		e.ReadBool("global_modifier")
	},
	"actions/play motion": ReadActionPlayMotion,
	"actions/play motion control": ReadActionPlayMotionControl,
	"actions/play motion control ch": function(e)
	{
		ReadActionPlayMotionControl(e)
		e.ReadU16("fwd_frame")
	},
	"actions/play motion ex": function(e)
	{
		ReadActionPlayMotion(e)
		
		e.ReadHintStr("dbg_object_model", "choose") // Arktika.1: changed 'ref_model' to 'choose'
		e.ReadHintStr("object_attp_id", "attp_str") // Arktika.1: changed 'locator_str' to 'attp_str'
		e.ReadBool8("flags0", ["move_to_mode", "precise_dest", "ignore_dest_rotation", "check_movement", "check_movement_move", "p2p_mode", "keep_x_pos", "keep_y_pos"])
		e.ReadBool8("flags1", ["keep_z_pos"]) // NEW in Exodus !!!
		e.ReadFP32("p2p_offset")
		
		e.ReadBool("stick_to_ai_map")	
	},
	"actions/play music theme": function(e)
	{
		e.ReadHintStr("theme_name", "choose")
	},
	"actions/play particles": ReadActionPlayParticles,
	"actions/play patrol path": function(e)
	{
		e.ReadU16("start_point", "entity_link, uobject_link")
		e.ReadBool("start_from_nearest")
		
		// patrol state
		e.ReadString("body_state");
		e.ReadString("anim_state");
		e.ReadString("movement_type");
		e.ReadString("weapon_state");
		e.ReadString("action");
		e.ReadU16("target", "entity_link, uobject_link");
		e.ReadBool32("flags",
			["precise_hit", "exact_rotation", "force_sprint", "disable_strafing"],
			[ 0x01,          0x02,             0x04,           0x08]
		)
		e.ReadFP32("anim_state_approach_speed");
		e.ReadFP32("approaching_accel");
		
		e.ReadBool8("flags0", ["state_from_first", "match_exact_goal", "fire_while_move", "can_move_avoid"]) 
	},
	"actions/play sound": ReadActionPlaySound,
	"actions/play sound ex": function(e)
	{
		ReadActionPlaySound(e)
		
		e.ReadU8("importance")
		e.ReadU32("falloff")
		// ? e.ReadU8("queueing")
		e.ReadU8("flags")
		e.ReadU32("delay")
		e.ReadBool8("new_flags", ["third_person", "auto_interest", "check_alive"]) // NEW in Arktika.1 !!!
		e.ReadU8("_subsequent") // NEW in Arktika.1 !!!
		e.ReadU16Array16("weapon_tags") // NEW in Arktika.1 !!!
	},
	"actions/play sound ex control": function(e)
	{
		e.ReadBool("interrupt_ovewrite")
	},
	"actions/play vibration": ReadActionPlayVibration,
	"actions/play_face_idle": function(e)
	{
		e.ReadString("idle_name")
	},
	"actions/player/active_slot": function(e)
	{
		e.ReadU8("slot")
		e.ReadBool("activate_in_civil")
	},
	"actions/player/camera axis lock": function(e)
	{
		e.ReadBool("remove_z_angle")
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
	},
	"actions/player/camera spring mode": function(e)
	{
		e.ReadBool8("axis", ["axis_x", "axis_y", "axis_z"])
		e.ReadVec3("inertion")
		e.ReadVec3("target_proportion")
	},
	"actions/player/civil mode": function(e)
	{
		e.ReadBool8("flags0", ["instant_in", "instant_out", "high_priority", "keep_holster"])
		var p = e.ReadSection("popups") // NEW in Exodus !!!
		p.ReadU8("weapon_popup")        // ..
		p.ReadU8("device_popup")        // ..
		p.ReadU8("fast_drop_popup")     // ..
		p.ReadU8("hint_popup")          // ..
		var s = e.ReadSection("slots")
		s.ReadU8("backpack_trade")      // NEW in Exodus !!!
		s.ReadU8("weapons")             // ..
		s.ReadU8("fire")                // ..
		s.ReadU8("knife_slot")          // Exodus: changed ReadBool to ReadU8 
		s.ReadU8("secondary_slot")      // ..
		s.ReadU8("primary_slot_1")      // ..
		s.ReadU8("grenade_slot")        // ..
		s.ReadU8("gasmask_slot")        // ..
		s.ReadU8("nightvision_slot")    // ..
		s.ReadU8("map_slot")            // ..
		s.ReadU8("medkit_slot")         // ..
		s.ReadU8("filter_slot")         // ..
		s.ReadU8("grenade_sticky_slot") // .. + renamed
		s.ReadU8("grenade_launcher_slot") // .. + renamed
		s.ReadU8("macheta_slot")          // .. + renamed
		s.ReadU8("charger_slot")          // .. + renamed
		s.ReadU8("grenade_flame_slot")    // ..
		s.ReadU8("claymore_slot")         // .. + renamed
		s.ReadU8("hud_object_slot")       // NEW in Exodus !!!
		//slots.ReadBool("heap")          // REMOVED in Exodus !!!
		s.ReadU8("primary_slot_2")        // Exodus: changed ReadBool to ReadU8
		s.ReadU8("arrow_slot")            // ..
		s.ReadU8("c4_slot")               // .. + renamed
		s.ReadU8("lighter_slot")          // .. + renamed
		s.ReadU8("grenade_flamethrower_slot") // NEW in Exodus !!!
		s.ReadU8("shield_slot")          // Exodus: changed ReadBool to ReadU8 + renamed
		s.ReadU8("arrow_breakable_slot") // NEW in Exodus !!!
		s.ReadU8("arm_device_slot")      // NEW in Exodus !!!
		s.ReadU8("torchlight_slot")      // NEW in Exodus !!!
		s.ReadU8("decoy_slot")           // NEW in Exodus !!!
		s.ReadU8("flare_slot")           // NEW in Exodus !!!
		s.ReadU8("binoculars_slot")      // NEW in Exodus !!!
		s.ReadU8("backpack_slot_1")      // NEW in Exodus !!!
		s.ReadU8("backpack_slot_2")      // NEW in Exodus !!!
		s.ReadU8("tape_slot")            // NEW in Exodus !!!
		//slots.ReadBool("no_slot")      // REMOVED in Exodus !!!
		var a = e.ReadSection("actions")  // NEW in Exodus !!!
		a.ReadU8("npc_qte_attack_action") // ..
		a.ReadU8("melee_kill_action")     // ..
		e.ReadString("request_show")      // ..
		e.ReadString("request_hide")      // ..
	},
	"actions/player/disable_apply_state": null,
	"actions/player/disable_camera_sync": null,
	"actions/player/disable_craft": null,
	"actions/player/doctor": function(e)
	{
		e.ReadU32("boost_time")
	},
	"actions/player/forbid_backpack_slots": null,
	"actions/player/gasmask filter limits": function(e)
	{
		e.ReadFP32("_hit_force")
		e.ReadU32("_max_filter_time")
		e.ReadU32("_slot_max_num")
	},
	"actions/player/give_ammo": function(e)
	{
		e.ReadBool("all")
		var arr = e.ReadArray("ammo_types")
		while(arr.MoreElements())
		{
			var rec = arr.NextElement()
			rec.ReadBool("selected")
			rec.ReadS32("count")
		}
	},
	"actions/player/grenade_timeouts": function(e)
	{
		e.ReadU32("timeout_grenade_friend")
		e.ReadU32("timeout_grenade_enemy")
	},
	"actions/player/head_delta": function(e)
	{
		e.ReadBool("body_rotation_enabled")
	},
	"actions/player/hide subj": function(e)
	{
		e.ReadBool("hands")
		e.ReadBool("knife")
	},
	"actions/player/in_combat": null,
	"actions/player/input_generator": function(e)
	{
		e.ReadU32("action")
	},
	"actions/player/level map mark": function(e)
	{
		e.ReadHintStr("mark_name", "choose")
		e.ReadBool("do_show")
		e.ReadVec2("offset")
	},
	"actions/player/level map mark ex": function(e)
	{
		e.ReadU8("preset_id")
		e.ReadHintStr("mark_name", "choose")
		e.ReadBool("do_show")
		e.ReadVec2("offset")
	},
	"actions/player/lock camera": function(e)
	{
		e.ReadFP32("inertion")
		//e.ReadHintStr("model", "ref_model") // REMOVED in Exodus !!!
		e.ReadHintStr("dbg_model", "choose")  // NEW in Exodus !!!
		e.ReadString("dbg_skel")              // NEW in Exodus !!!
		e.ReadHintStr("bone", "attp_str")     // Exodus: changed 'locator_str' to 'attp_str' 
		e.ReadVec2("limit_yaw", "ang2f")
		e.ReadVec2("soft_limit_yaw", "ang2f")
		e.ReadVec2("limit_pitch", "ang2f")
		e.ReadVec2("soft_limit_pitch", "ang2f")
		e.ReadU32("noinput_return_delay")
		e.ReadFP32("noinput_return_angle", "angle, fp32")
		e.ReadFP32("softness")
		e.ReadFP32("blend")
		e.ReadU32("as_blend_time") // NEW in Exodus !!!
		e.ReadBool8("flags", ["rotate_to_target", "rotate_precisely", "skip_attach_on_deactivate", "enable_soft_limits", "xf_rot_forbid", "use_bone_ang", "return_noinput"])
	},
	"actions/player/luminocity correction": function(e)
	{
		e.ReadFP32("_coef")
	},
	"actions/player/metal detector object": null,
	"actions/player/mount_vehicle": null,
	"actions/player/play dof": function(e)
	{
		e.ReadFP32("near_dof")
		e.ReadFP32("far_dof")
	},
	"actions/player/power_off_control": null,
	"actions/player/purge_ammo": function(e)
	{
		e.ReadIdentifierArray("ammo_types")
		e.ReadBool8("flags0", ["all", "silent"])
		e.ReadFP32("percentage")
	},
	"actions/player/restrictor_activate": null,
	"actions/player/restrictor_obstacle": null,
	"actions/player/set_ammo": function(e)
	{
		e.ReadU32("min")
		e.ReadU32("max")
	},
	"actions/player/set_crosshair_style": function(e)
	{
		e.ReadU8("style")
	},
	"actions/player/show subj": function(e)
	{
		e.ReadBool("hands")
		e.ReadBool("knife")
	},
	"actions/player/spend_ammo": function(e) // not used in original game
	{
		//if(version < 27) e.ReadU8("ammo_type")
		e.ReadIdentifier("ammo_type")
		e.ReadS32("ammo_count")
		e.ReadBool("all")
	},
	"actions/player/torchlight_control": function(e)
	{
		e.ReadFP32("_power_consumption_multiplier")
		e.ReadU8("_device")
	},
	"actions/player/torchlight_disturb_distance": function(e)
	{
		e.ReadFP32("disturb_distance")
	},
	"actions/player/visible hud": null,
	"actions/player/way_point": function(e)
	{
		e.ReadBool("path_visible")
		e.ReadBool("direct")
	},
	"actions/player/weapon make dismantlable": null,
	"actions/player/weapon_down": function(e)
	{
		e.ReadBool("action_type")
	},
	"actions/progress item": function(e)
	{
		ReadActionShowHudMenu(e);
		e.ReadU16("fill_time")
		e.ReadU8("count_for_fill")
		e.ReadFP32("start_percent_fill")
		e.ReadU8("mutiply_plus") // eww, not 'multiply' but 'mutiply', even in original engine...
		e.ReadU8("mutiply_minus")
		e.ReadHintStr("name_stamp", "choose")
		e.ReadU8("idx_fill_def")
	},
	"actions/queue params": function(e) // параметры стрельбы очередями
	{
		e.ReadFP32("close_distance")
		e.ReadU32("close_range_min_queue_size")
		e.ReadU32("close_range_max_queue_size")
		e.ReadU32("close_range_min_queue_interval")
		e.ReadU32("close_range_max_queue_interval")
		e.ReadU32("distant_range_min_queue_size")
		e.ReadU32("distant_range_max_queue_size")
		e.ReadU32("distant_range_min_queue_interval")
		e.ReadU32("distant_range_max_queue_interval")
	},
	"actions/request transition": function(e)
	{
		e.ReadString("request")
		e.ReadU8("state")
	},
	"actions/request_transition": function(e)
	{
		e.ReadString("request")
		e.ReadU8("state")
	},
	"actions/scound_schema": function(e)
	{
		e.ReadHintStr("sound_scheme", "choose")
	},
	"actions/scripted fire": null,
	"actions/set enemy": function(e)
	{
		e.ReadU16("enemy", "entity_link, uobject_link")
		e.ReadU16("point", "entity_link, uobject_link")
		e.ReadU32("threat_valid_time") // NEW in Arktika.1 !!!
		e.ReadBool("threat")
		e.ReadBool("lock")
	},
	"actions/set music scheme": function(e)
	{
		e.ReadHintStr("scheme_name", "choose")
	},
	"actions/set physics props": function(e)
	{
		e.ReadU8("kinematic")
		e.ReadU8("breakable")
		e.ReadU8("allow")
		e.ReadU8("allow_on_detach")
		e.ReadU8("raycast")
		e.ReadU8("raycast_ai") 
		e.ReadU8("clg_from_model")
		e.ReadU8("destroy_on_contact")
		e.ReadU8("semi_breakable") 
		//e.ReadU8("sleeping")        // REMOVED in Exodus !!!
		e.ReadBool("mod_collision_grp")
		e.ReadU8("collision_group")
	},
	"actions/set_name": function(e)
	{
		e.ReadHint("name", "name")
		e.ReadString("name")
	},
	"actions/set_filters_spend": function(e)
	{
		e.ReadBool("indoor_mode")
	},
	"actions/set_narrative": function(e)
	{
		e.ReadU8("narrative_id")
	},
	"actions/set_speech_groups": function(e)
	{
		//e.ReadString("allowed_speech_groups")
		
		e.ReadHintStr("allowed_speech_groups", "choose_array, str_shared")
		e.ReadU8("action")
	},
	"actions/show_diary": null,
	"actions/show_hud_menu": ReadActionShowHudMenu,
	"actions/show_icon": function(e)
	{
		e.ReadU32("icon_color", "color, u32")
		e.ReadHintStr("icon_name", "choose")
		e.ReadU32("position_x")
		e.ReadU32("position_y")
		e.ReadFP32("scale")
		e.ReadFP32("rotate")
		e.ReadBool("flip_h")
		e.ReadBool("flip_v")
		e.ReadU32("fade_in")
		e.ReadU32("fade_out")
	},
	"actions/simple_cct": function(e)
	{
		e.ReadBool("anim_driven")
		e.ReadBool("colliding")
	},
	"actions/slope_settings": function(e)
	{
		e.ReadFP32("bad_angle")
		e.ReadFP32("sliding_angle")
		e.ReadU32("bad_nrm_control_time")
		e.ReadFP32("bad_nrm_control_loss_coef")
	},
	"actions/smell": function(e)
	{
		e.ReadBool8("smell_flags", ["smell_enabled", "smell_player_only"])
		e.ReadFP32("smell_radius")
		e.ReadU32("smell_interval")
		e.ReadU8("smell_threat_type")
	},
	"actions/sound params": function(e)
	{
		e.ReadU8("target_mental_state")
		e.ReadFP32("disturb_threshold")
		e.ReadFP32("light_alert_threshold")
		e.ReadFP32("alert_threshold")
		e.ReadFP32("danger_threshold")
	},
	"actions/species_behavior": function(e)
	{
		e.ReadBool("_use_defaults")
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
	},
	"actions/static_combat": function(e)
	{
		// nearly same as in ReadBaseBrainUnit, except added model/dbg_skel
		e.ReadBool8("static_combat_flags", ["make_static_combat", "force_active_wo_enemy", "play_shot_delta", "aim_disabled", "aim_yaw", "aim_pitch", "not_use_cover", "shoot_while_moving"]);
		e.ReadHintStr("model", "choose");
		e.ReadString("dbg_skel");
		e.ReadHintStr("static_idle", "animation_str");
		e.ReadHintStr("static_attack", "animation_str");
		e.ReadHintStr("static_reload_idle", "animation_str");
		e.ReadHintStr("static_shot", "animation_str"); // in level.bin it's static_shoot, here's static_shot :\
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
		
		e.ReadFP32("min_distance")
		e.ReadFP32("max_distance")
		e.ReadFP32("target_distance")
		e.ReadFP32("attack_angle")
		
	},
	"actions/static_combat_anchor": function(e)
	{
		e.ReadFP32("radius")
		e.ReadFP32("enemy_dist")
	},
	"actions/stop music": function(e)
	{
		e.ReadU32("fade_out")
	},
	"actions/stop_vehicle": function(e)
	{
		e.ReadU32("delta_time")
		e.ReadBool("park_on_stop")
	},
	"actions/take_cover": function(e)
	{
		e.ReadU32("clear_cover_time") // NEW in Arktika.1
		e.ReadBool("_keep_force_cover") // NEW in Arktika.1
	},
	"actions/threat_params": function(e)
	{
		// same as in ReadBaseBrainUnit (levelbin.js)
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
	},
	"actions/toggle": null,
	"actions/treadmil": function(e)
	{
		e.ReadU32("move_speed")
		e.ReadFP32("distance")
		e.ReadU8("Initial_quant")
		e.ReadU8("Random_quant")
	},
	"actions/turn light": function(e)
	{
		e.ReadU8("onoff")
		e.ReadBool("on_or_off")
	},
	"actions/turn torchlight": function(e)
	{
		e.ReadBool8("flags0", ["silent_mode"])
	},
	"actions/unlock_cover": function(e)
	{
		e.ReadU32("reuse_delay")
	},
	"actions/update ik": null,
	"actions/update_once": null,
	"actions/vision params": function(e)
	{
		e.ReadU8("target_mental_state")
		e.ReadFP32("vision_range")
	},
	"actions/voice": function(e)
	{
		e.ReadHintStr("voice", "choose")
	},
	"actions/vs_move": function(e)
	{
		e.ReadHintStr("model", "choose") // Arktika.1: changed 'ref_model' to 'choose'
		e.ReadString("dbg_skel")
		e.ReadHintStr("vs_move_idle", "animation_str")
		e.ReadHintStr("vs_move_fwd", "animation_str")
		e.ReadHintStr("vs_move_left45", "animation_str")
		e.ReadHintStr("vs_move_right45", "animation_str")
		e.ReadHintStr("vs_move_turn_left90", "animation_str")
		e.ReadHintStr("vs_move_turn_right90", "animation_str")
	},
	"actions/water_move": function(e)
	{
		e.ReadVec3("offset")
		e.ReadU32("move_time")
	},
	"actions/water_ripples_mode": function(e)
	{
		e.ReadU8("mode")
		e.ReadVec3("custom_pos")
	},
	"actions/weapon/dispersion": function(e)
	{
		e.ReadFP32("dispersion")
	},
	"actions/weapon/force spinning": null,
	"actions/weapon/laser control": null,
	"actions/weapon/set weapon items": function(e)
	{
		e.ReadHintStr("dbg_upgradable", "choose")
		var arr = e.ReadArray("items")
		while(arr.MoreElements())
		{
			var rec = arr.NextElement()
			rec.ReadHintStr("item", "choose")
		}
		
		e.ReadBool("alternative_ammo")
		e.ReadBool("auto_fix")
	},
	"actions/weapon/set weapon pressure": function(e)
	{
		e.ReadFP32("_percentage")
	},
	"actions/wear binoculars": function(e)
	{
		e.ReadBool8("flags0", ["silent_mode"])
	},
	"actions/wear gasmask": function(e)
	{
		e.ReadBool8("flags0", ["silent_mode"])
	},
	"actions/wear nightvision": function(e)
	{
		//e.ReadBool("instant") // REMOVED in Exodus
		e.ReadBool8("flags0", ["silent_mode"]) // NEW in Exodus !!!
	},
	"actions/wear_suit": function(e)
	{
		e.ReadU32("suit_type")
	},
	"actions/xz rotation mode": function(e)
	{
		e.ReadFP32("blend")
	},
	"actions/zone type" : function(e)
	{
		// e.ReadU8("zone_type") // REMOVED in Arktika.1
		e.ReadHint("type_mask", "flags64") // NEW in Arktika.1 !!!
		e.ReadU64("type_mask") // NEW in Arktika.1 !!!
	},
	"checker/AUX": function(e)
	{
		e.ReadFP32("min")
		e.ReadFP32("max")
		e.ReadU8("value")
	},
	"checker/animation": function(e)
	{
		e.ReadHintStr("dbg_model", "choose")
		e.ReadString("dbg_skel")
		
		e.ReadU8("quant")
		var o = e.ReadArray("outputs", "output_%.2d")
		while(o.MoreElements())
		{
			var rec = o.NextElement();
			rec.ReadHintStr("anim", "choose")
		}
	},
	"checker/demo_mode": null,
	"checker/dlc": function(e)
	{
		e.ReadHintStr("name", "choose")
	},
	"checker/entity/bone inside": function(e)
	{
		e.ReadHintStr("dbg_model", "choose")
		e.ReadString("dbg_skel")
		e.ReadHintStr("bone", "attp_str")
	},
	"checker/entity/check targets count": null,
	"checker/entity/compare_distance": function(e)
	{
		e.ReadFP32("threshold")
	},
	"checker/entity/is_between": null,
	"checker/entity/is_same": function(e)
	{
		e.ReadBool("all")
	},
	"checker/entity/state": null,
	"checker/entity/velocity": function(e)
	{
		e.ReadFP32("min")
		e.ReadFP32("max")
		e.ReadBool("dir_x") // NEW in Exodus !!!
		e.ReadBool("dir_y") // ..
		e.ReadBool("dir_z") // ..
		e.ReadBool("local_space") // ..
	},
	"checker/entity/visible": null,
	"checker/entity/visible4npc": function(e)
	{
		e.ReadBool8("vis_flags0", ["los", "visible", "visible_strict"])
	},
	"checker/game_plus/can_start": null,
	"checker/game_plus/started": null,
	"checker/game_time/check_time": function(e)
	{
		e.ReadS32("dstart")
		e.ReadS32("dend")
		ReadTime(e, "tstart")
		ReadTime(e, "tend")
	},
	"checker/gamepad": null,
	"checker/level": function(e)
	{
		e.ReadHintStr("check_level", "choose")
	},
	"checker/localizable_signs": null,
	"checker/model": function(e)
	{
		// ?
		//e.ReadHintStr("dbg_model", "choose")
		//e.ReadString("dbg_skel")
		
		e.ReadU8("quant")
		var o = e.ReadArray("outputs", "output_%.2d")
		while(o.MoreElements())
		{
			var rec = o.NextElement()
			rec.ReadHintStr("model", "choose")
		}
	},
	"checker/monster/": null,
	"checker/monster/airborne": null,
	"checker/mutations": function(e)
	{
		e.ReadU32("type")
	},
	"checker/npc/enemy": function(e)
	{
		e.ReadIdentifierArray("npc_clsid_vec")
		e.ReadBool("exclude_defenseless")
		e.ReadBool("allow_inactive")
	},
	"checker/npc/has_enemy": function(e)
	{
		// same as checker/npc/pc_type
		e.ReadIdentifierArray("npc_clsid_vec")
		e.ReadBool("exclude_defenseless")
	},
	"checker/npc/is_group_leader": null,
	"checker/npc/movement_interrupt": null,
	"checker/npc/overlap_test": function(e)
	{
		e.ReadHint("clg", "flags32")
		e.ReadU32("clg")
		e.ReadU8("type")
		e.ReadFP32("vert_shift")
		e.ReadFP32("hor_shift")
		e.ReadFP32("test_len")
		e.ReadHintStr("locator", "attp_str")
	},
	"checker/npc/pc_type": function(e)
	{
		//e.ReadU32("npc_type")
		e.ReadIdentifierArray("npc_clsid_vec")
		e.ReadBool("exclude_defenseless")
	},
	"checker/npc/threat_dist": function(e)
	{
		e.ReadFP32("_check_dist")
	},
	"checker/platform": null,
	"checker/player/costume upgrade enabled": function(e)
	{
		e.ReadU8("device")
		e.ReadHintStr("upgrade_id", "choose")
	},
	"checker/player/item active": function(e)
	{
		e.ReadU8("slot")
		e.ReadBool("not_inactive")
	},
	"checker/player/popup allowed": function(e)
	{
		e.ReadU8("popup_type")
	},
	"checker/qte_mode": null,
	"checker/rt_demo": null,
	"checker/weapon/has weapon items": function(e)
	{
		e.ReadHintStr("dbg_upgradable", "choose")
		var items = e.ReadArray("items")
		while(items.MoreElements())
		{
			var rec = items.NextElement()
			rec.ReadHintStr("item", "choose")
		}
	},
	"covers/covers name": function(e)
	{
		e.ReadHintStr("name", "choose")
	},
	"covers/cover_by_eval": function(e)
	{
		e.ReadFP32("min_radius")
		e.ReadFP32("eval_radius")
		e.ReadU32("cover_type")
		e.ReadU32("cover_type_id")
		e.ReadBool("strict_lock")
	},
	"covers/invisible": function(e)
	{
		e.ReadFP32("min_radius")
		e.ReadFP32("eval_radius")
		e.ReadU32("cover_type")
		e.ReadU32("cover_type_id")
		e.ReadBool("strict_lock")
	},
	"covers/ref": function(e)
	{
		e.ReadU16("target", "cover_link, ucover_link")
	},
	"entities/active weapon": function(e)
	{
		e.ReadBool("item_type")
	},
	"entities/entity children": function(e)
	{
		e.ReadBool("top_children") // NEW in Exodus !!
	},
	"entities/entity inv item": function(e)
	{
		e.ReadU8("slot")
	},
	"entities/entity invisible": null,
	"entities/entity labeled": function(e)
	{
		e.ReadString("label")
	},
	"entities/entity level player": function(e)
	{
		e.ReadU16("target", "entity_link, uobject_link")
	},
	"entities/entity name": function(e)
	{
		e.ReadHint("name", "name")
		e.ReadString("name")
	},
	"entities/entity parent": null,
	"entities/entity player": function(e)
	{
		e.ReadU8("team");
	},
	"entities/entity player's hands": null,
	"entities/entity player's torch": null,
	"entities/entity player self": function(e)
	{
		e.ReadU8("team");
	},
	"entities/entity ref": function(e)
	{
		e.ReadU16("target", "entity_link, uobject_link")
	},
	"entities/entity self": null,
	"entities/entity trade weapon": null,
	"entities/filters/enemies filter": null,
	"entities/filters/is alive filter": null,
	"entities/filters/nearest item": null,
	"entities/filters/npc group filter": function(e)
	{
		e.ReadString("_group_name")
	},
	"entities/filters/weapon types filter": function(e)
	{
		e.ReadIdentifierArray("weapon_types_filter")
	},
	"entities/monster_enemy": null,
	"entities/npc_enemy": null,
	"entities/npc_enemy_ex": function(e)
	{
		e.ReadIdentifierArray("npc_type")
		e.ReadBool("include_player")
	},
	"entities/pose weapon locator ref": function(e)
	{
		e.ReadU8("locator")
	},
	"entities/upgrade arm device ref": function(e)
	{
		e.ReadU8("upgrade")
	},
	"fun/text splash": function(e)
	{
		e.ReadBool("active") // NEW in Arktika.1 !!!
		e.ReadString("text")
		e.ReadVec4("color", "color, vec4f")
		e.ReadFP32("max_size")
		e.ReadFP32("speed")
		e.ReadBool("stop")
		e.ReadBool("interrupt")
	},
	"logic/adv/and-2-ext": function(e)
	{
		e.ReadBool("auto_touch")
	},
	"logic/adv/counter ref no touch": function(e)
	{
		ReadLogicCounter(e)
		e.ReadS32("reference")
	},
	"logic/adv/randomex": ReadLogicAdvRandomex,
	"logic/adv/randomex-2": ReadLogicAdvRandomex, 
	"logic/adv/randomex-3": ReadLogicAdvRandomex, 
	"logic/adv/randomex-4": ReadLogicAdvRandomex,
	"logic/adv/randomex-5": ReadLogicAdvRandomex,
	"logic/adv/random_selector_2": function(e)
	{
		e.ReadFP32("probability")
		e.ReadBool("me_called")
		e.ReadBool("other_called")
	},
	"logic/ai/cover/dirrandom": function(e)
	{
		e.ReadHintStr("cover_group", "choose")
	},
	"logic/ai/cover/in direction": function(e)
	{
		e.ReadHintStr("cover_group", "choose")
		e.ReadFP32("fwd_ang", "angle, fp32")
		e.ReadFP32("bwd_ang", "angle, fp32")
		e.ReadFP32("lft_ang", "angle, fp32")
		e.ReadFP32("rgt_ang", "angle, fp32")
		e.ReadU8("orientation")
	},
	"logic/ai/cover/invisible enemygroup": function(e)
	{
		e.ReadHintStr("cover_group", "choose")
		e.ReadFP32("dist_coef")
		e.ReadFP32("player_dist_coef")
		e.ReadFP32("visible_coef")
	},
	"logic/ai/cover/near player": function(e)
	{
		e.ReadHintStr("cover_group", "choose")
		e.ReadFP32("dist_coef")
		e.ReadFP32("dir_coef")
		e.ReadFP32("back_coef")
		e.ReadFP32("fwd_coef")
		e.ReadBool("check_enemy_accessibility")
		e.ReadBool("check_radius_from_asker")
		e.ReadFP32("enemy_dir_coef")
	},
	"logic/ai/cover/nearest": function(e)
	{
		e.ReadHintStr("cover_group", "choose") // NEW in Exodus !!!
		e.ReadFP32("dist_coef")
		e.ReadFP32("dir_coef")
		e.ReadFP32("direct_path_coef")
		e.ReadFP32("max_distance")
	},
	"logic/ai/cover/random": function(e)
	{
		e.ReadHintStr("cover_group", "choose") // NEW in Exodus !!!
	},
	"logic/and-2": function(e)
	{
		e.ReadBool("auto_touch")
	},
	"logic/and-3": function(e)
	{
		e.ReadBool("auto_touch")
	},
	"logic/and-multi": function(e)
	{
		e.ReadU8("quant")
		e.ReadBool("auto_touch")
	},
	"logic/chooser": function(e)
	{
		e.ReadU8("quant")
		e.ReadU8("current_output")
	},
	"logic/counter": ReadLogicCounter,
	"logic/counter ref": function(e)
	{
		ReadLogicCounter(e)
		e.ReadS32("reference")		
	},
	"logic/counter sel": function(e)
	{
		ReadLogicCounter(e)
		e.ReadU8("quant")
	},
	"logic/delay": ReadLogicDelay,
	"logic/delay ex": function(e)
	{
		ReadLogicDelay(e)
		e.ReadU32("_mul_val")
		e.ReadU32("_add_val")
	},
	"logic/delay multi": function(e)
	{
		ReadLogicDelay(e)
		e.ReadU8("min_times")
		e.ReadU8("max_times")
	},
	"logic/exposed_locker": function(e)
	{
		e.ReadBool8("flags", ["locked", "auto_lock", "next_frame", "two_frames"])
		e.ReadString("exp_name")
	},
	"logic/hub": null,
	"logic/locker" : function(e)
	{
		e.ReadBool8("flags", ["locked", "auto_lock", "next_frame", "two_frames"])
	},
	"logic/locker_base" : function(e)
	{
		e.ReadBool8("flags", ["locked", "auto_lock", "next_frame", "two_frames"])
	},
	"logic/or-2": function(e)
	{
		e.ReadBool("auto_touch")
	},
	"logic/or-3": function(e)
	{
		e.ReadBool("auto_touch")
	},
	"logic/or-multi": function(e)
	{
		e.ReadU8("quant")
		e.ReadBool("auto_touch")
	},
	"logic/random": function(e)
	{
		e.ReadU8("quant")
	},
	"logic/random-2": function(e)
	{
		e.ReadU8("quant")
	},
	"logic/random-3": function(e)
	{
		e.ReadU8("quant")
	},
	"logic/random-4": function(e)
	{
		e.ReadU8("quant")
	},
	"logic/random-5": function(e)
	{
		e.ReadU8("quant")
	},
	"logic/random-chooser": ReadLogicRandomChooser,
	"logic/random-chooser-3": ReadLogicRandomChooser,
	"logic/random-chooser-4": ReadLogicRandomChooser,
	"logic/select-2": function(e)
	{
		e.ReadU8("quant")
		e.ReadBool("same_selected_allowed") // NEW in Redux!!
	},
	"logic/select-3": function(e)
	{
		e.ReadU8("quant")
		e.ReadBool("same_selected_allowed") // NEW in Redux!!
	},
	"logic/select-4": function(e)
	{
		e.ReadU8("quant")
		e.ReadBool("same_selected_allowed") // NEW in Redux!!
	},
	"logic/select-5": function(e)
	{
		e.ReadU8("quant")
		e.ReadBool("same_selected_allowed") // NEW in Redux!!
	},
	"logic/select_param": function(e)
	{
		e.ReadU8("quant")
		e.ReadBool("same_selected_allowed") // NEW in Redux!!
	},
	"logic/switch-2": function(e)
	{
		e.ReadU8("quant")
		e.ReadU8("type")
	},
	"logic/switch-3": function(e)
	{
		e.ReadU8("quant")
		e.ReadU8("type")
	},
	"logic/switch-4": function(e)
	{
		e.ReadU8("quant")
		e.ReadU8("type")
	},
	"logic/switch-5": function(e)
	{
		e.ReadU8("quant")
		e.ReadU8("type")
	},
	"logic/switch_param": function(e)
	{
		e.ReadU8("quant")
		e.ReadU8("type")
	},
	"logic/switchboard": function(e)
	{
		e.ReadU8("quant")
		var count = e.ReadU8("states_count")
		for(var i = 0; i < count; i++)
			e.ReadBool("on_"+i)
	},
	"trade/dismantle weapon": null,
	"trade/purge slot": function(e)
	{
		e.ReadU8("slot")
		e.ReadU8("counter")
		//e.ReadBool("unload_magazine") // REMOVED in Exodus !!
		//e.ReadBool("attach_entity") // REMOVED in Exodus !!
		e.ReadBool8("flags", ["unload_magazine", "attach_entity", "active_weapon"]) // NEW in Exodus !!!
	},
	"trade/take item": function(e)
	{
		e.ReadBool8("tradeflags8", ["vs_use", "creating"])
	},
	"trade/trade trigger": function(e)
	{
		e.ReadU8("trade_type") // < 7
		e.ReadU32("current_object")
		e.ReadU32("objects_count_pre")
		e.ReadU32("trade_preset")
		e.ReadU8("trade_mode")
		e.ReadBool8("flags1", ["can_sell_attaches", "need_attach", "_is_exchanging_enabled", "_is_deffered_crafting", "_destroy_item_on_sell", "_is_attaches_trade_enabled", "charity_attach_mode"])
		e.ReadBool8("flags2", ["allow_items_sell", "trade_tiers", "tiers_for_tiers"])
		e.ReadU32("objects_count")
		e.ReadFP32("cam_track_accrue")
		e.ReadFP32("cam_track_falloff")
	},
	"trigger/ammo spend": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("min_cnt")
		e.ReadU32("max_cnt")
	},
	"trigger/did install upgrade": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("device")
		e.ReadHintStr("upgrade_id", "choose")
	},
	"trigger/has ammo": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("with_loaded")
		e.ReadBool("as_percent")
		e.ReadIdentifierArray("ammo_types")
		var count = e.ReadU32("state_count")
		for(var i = 0; i < count; i++)
			e.ReadU32("state_" + (i+1) + "_ammo_count");
	},
	"trigger/has fast drop": function(e) // NEW in Exodus !!!
	{
		ReadTrigger(e)
		e.ReadBool("with_loaded")
		e.ReadBool("as_percent")
		e.ReadU8("fast_drop_type")
		e.ReadU32("fast_drop_count")
	},
	"trigger/hearing": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadFP32("sound_power")
		e.ReadFP32("dist")
		e.ReadU32("ai_sound_type")
	},
	"trigger/pick up ammo": function(e)
	{
		ReadTrigger(e)
		e.ReadU8Array('ammo')
	},
	"trigger/pick up fastdrop": function(e)
	{
		ReadTrigger(e)
		e.ReadU8Array("slot")
	},
	"trigger/pick up upgrade": function(e)
	{
		ReadTrigger(e)
		e.ReadHintStr("upgrade_id", "choose")
	},
	"trigger/pick up weapon": function(e)
	{
		ReadTrigger(e)
		e.ReadIdentifier("weapon_type")
		e.ReadBool("only_undiscovered")
	},
	"trigger/vision": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadFP32("vision_value")
	},
	"trigger/ragdoll": ReadTrigger,
	"triggers/active item": function(e)
	{
		ReadTrigger(e)
		e.ReadIdentifier("weapon_type")
	},
	"triggers/active_state": ReadTrigger,
	"triggers/aftercloned": function(e)
	{
		ReadTrigger(e)
		e.ReadString("tag")
	},
	"triggers/am i enemy": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadIdentifierArray("npc_type")
		e.ReadFP32("safe_range")
		e.ReadBool("exclude_defenseless")
	},
	"triggers/ammo absent": function(e)
	{
		ReadTrigger(e)
		e.ReadU16("min_ammo_count")
		e.ReadU8("loaded")
	},
	"triggers/anim event": function(e)
	{
		ReadTrigger(e)
		e.ReadU16("entity", "entity_link, uobject_link")
		e.ReadString("event")
	},
	"triggers/anim param": function(e)
	{
		ReadTrigger(e)
		e.ReadHintStr("dbg_model", "choose")
		e.ReadString("dbg_skel")
		e.ReadHintStr("param", "param_str") // param_str что-то новенькое 
		e.ReadFP32("val0")
		e.ReadFP32("val1")
		e.ReadU8("accept")
	},
	"triggers/anim signal": function(e)
	{
		ReadTrigger(e)
		e.ReadString("signal")
	},
	"triggers/any player": function(e)
	{
		ReadTriggerZone(e)
		e.ReadU8("_friend")
		e.ReadU8("_team")
	},
	"triggers/any_input": ReadTrigger,
	"triggers/arahind state": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("_fake_light_multiplier")
		e.ReadBool("_force_light")
	},
	"triggers/attach-detach": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("need_child_check")
	},
	"triggers/auto_save_info": ReadTrigger,
	"triggers/binoculars": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("binoculars_state")
	},
	"triggers/body_posture": null,
	"triggers/camera-track event": function(e)
	{
		ReadTrigger(e)
		e.ReadString("event")
	},
	"triggers/close_range weapon": ReadTrigger,
	"triggers/collision": function(e)
	{
		ReadTrigger(e)
		//e.ReadBool("super_static") // REMOVED in Exodus !!
		e.ReadU32("collisions_group")
		//e.ReadBool("only_arrows") // REMOVED in Exodus !!!
		e.ReadFP32("min_impulse") // NEW in Exodus !!!
	},
	"triggers/credits_finished": ReadTrigger,
	"triggers/danger_activity": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("activity")
	},
	"triggers/debug_input": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("key")
		e.ReadU32("modifier1")
		e.ReadU32("modifier2")
		e.ReadFP32("in_threshold") // NEW in Arktika.1 !!!
		e.ReadFP32("out_threshold") // NEW in Arktika.1 !!!
		e.ReadBool("network")
	},
	"triggers/die": function(e)
	{
		ReadTrigger_ee(e)
		//e.ReadBool("from_fire") // REMOVED in Exodus !!
		e.ReadU8("mode") // NEW in Exodus !!
		e.ReadU8("type") // ..
	},
	"triggers/dog_smell": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("_check_on_activate")
	},
	"triggers/engine/signal": function(e)
	{
		ReadTrigger(e)
		e.ReadHintStr("signal", "choose")
	},
	"triggers/entity/health threshold": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("threshold")
		e.ReadFP32("threshold_step")
		e.ReadBool("positive")
	},
	"triggers/entity/health threshold2": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("threshold")
		e.ReadFP32("threshold_step")
		e.ReadBool("positive")
	},
	"triggers/entity/orientation": ReadTriggerEntityOrientation,
	"triggers/game/menu dpad trigger": function(e)
	{
		function read_dpad(n)
		{
			e.ReadHintStr("dpad_" + n + "_icon", "choose")
			e.ReadBool("dpad_" + n + "_hold")
			e.ReadU32("dpad_" + n + "_hold_time")
		}
		
		ReadTrigger(e)
		e.ReadBool("allow_input")
		e.ReadBool("reset_state_on_activate")
		e.ReadU32("delay_resend_key")
		e.ReadHintStr("menu_name", "choose")
		e.ReadU8("current_dir")
		e.ReadU8("key_choice");
		
		["left", "right", "top", "bottom"].forEach(read_dpad);
	},
	"triggers/game/menu trigger": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("current_object")
		var cnt = e.ReadU32("objects_count_pre")
		for(var i = 1; i <= cnt; i++) {
			e.ReadString("object_" + i + "_name")
			e.ReadU16("object_" + i + "_link", "entity_link, uobject_link")
			e.ReadString("entity_" + i + "_name")
			e.ReadU16("entity_" + i + "_link", "entity_link, uobject_link")
		}
		e.ReadU32("objects_count")
		e.ReadBool("ignore_select_input")
	},
	"triggers/game/take_cover": ReadTrigger,
	"triggers/game/weapon_state": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("any_weapon_state") // NEW in Exodus !!!
		e.ReadU8("weapon_state")
		e.ReadBool("any_aim_state")
		var s = e.ReadSection("aim_states")
		s.ReadHint("aim_state", "flags8")
		s.ReadBool8("aim_state", ["aim_none", "aim_in", "aim_idle", "aim_out"], "u8")
	},
	"triggers/gamepad": ReadTrigger,
	"triggers/gamepad_custom": ReadTrigger,
	"triggers/gasmask": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("trigger_on_wear")
	},
	"triggers/game_difficulty": function(e)
	{
		e.ReadU32("difficulty_id")
	},
	"triggers/game_difficulty_change": ReadTrigger,
	"triggers/game_time/time_event": function(e) // NEW in Exodus !!!
	{
		ReadTrigger(e)
		e.ReadS32("day")
		ReadTime(e, "time")
		e.ReadU8("tl_mode")
	},
	"triggers/gameloaded": ReadTrigger,
	"triggers/generic": ReadTriggerZone,
	"triggers/generic-no-player": ReadTriggerZone,
	"triggers/generic_special": ReadTriggerZoneSpecial,
	"triggers/has_target": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("tid")
		e.ReadFP32("distance")
		e.ReadFP32("threshold")
		e.ReadFP32("aimap_threshold")
		e.ReadU32("time_threshold")
		e.ReadU8("_friend")
		e.ReadIdentifierArray("npc_type")
		e.ReadBool8("flags8", ["ai_map_check", "busy_check", "use_frustum"])
		e.ReadString("ex_prop")
	},
	"triggers/hit": ReadTriggerHit,
	"triggers/hit ref": ReadTriggerHit,
	"triggers/illuminated": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("dist")
	},
	"triggers/illumination":function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("illumination")
	},
	"triggers/input": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("action")
		e.ReadBool8("flags0", ["allow_input", "network"])
	},
	"triggers/interest": function(e)
	{
		ReadTrigger(e)
		//e.ReadU16("entity", "entity_link, uobject_link") // REMOVED in Exodus !!!
		e.ReadHintStr("bone", "attp_str") // Exodus: changed 'locator_str' to 'attp_str'
		//e.ReadU16("interest", "entity_link, uobject_link") // REMOVED in Exodus !!!
		e.ReadFP32("distance")
		e.ReadFP32("fov", "angle, fp32")
		e.ReadU32("time")
		e.ReadBool("reload")
		e.ReadBool("keep_watching") // NEW in Exodus !!!
	},
	"triggers/is fire": function(e)
	{
		var weapons = [
			"revolver",
			"uboynicheg", 
			"duplet", 
			"tihar", "helsing",
			"ubludok",
			"AK 74",
			"2012",
			"VSV", 
			"macheta", 
			"dagger",
			"medved", 
			"flamethrower",
			"ventil",
			"ashot",
			"padonag",
			"gatling",
			"saiga",
			"tumak",
			"abzaz_slot",
			"aksu_slot",
			"rpk_slot",
		]
		
		if(entity_ver >= 53)
			weapons.push(		
				"ak sammy slot",
				"kolya slot",
				"vyhlop"
			);
			
		ReadTrigger(e)
		for(var i = 0; i < weapons.length; i++)
		{
			var r = e.ReadSection(weapons[i])
			r.ReadU8("primary_"+weapons[i])   // Exodus: changed ReadBool to ReadU8
			r.ReadU8("secondary_"+weapons[i]) // Exodus: changed ReadBool to ReadU8
		}
		
		//e.ReadBool("enabled_dynamite")  // REMOVED in Arktika.1 !!!
		//e.ReadBool("enabled_launcher")  // REMOVED in Arktika.1 !!!
		//e.ReadBool("enabled_shotgun")   // REMOVED in Arktika.1 !!!
		
		// NEW in Arktika.1 !!!
		e.ReadBool8("flags", ["dynamite", "sticky_dynamite", "flame_grenade", "claymore", "knife", "launcher", "shotgun", "melee"])
	},
	"triggers/is milestone": ReadTrigger,
	"triggers/is player busy": ReadTrigger,
	"triggers/lamp_die": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("type_mask")
	},
	"triggers/level_mental": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("mental_border")
	},
	"triggers/loading_screen": function(e)
	{
		ReadTrigger(e)
		e.ReadBool8("flags", ["enable_on_save_load"])
	},
	"triggers/medkit": ReadTrigger,
	"triggers/menu activated": function(e)
	{
		ReadTrigger(e)
		e.ReadHintStr("menu_name", "choose")
	},
	"triggers/menu event": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("menu_event")
	},
	"triggers/net/check_prop": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("prop_id")
		e.ReadS32("prop_delta")
	},
	"triggers/net/check_prop_bit": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("prop_id")
		e.ReadU8("idx")
	},
	"triggers/nightvision": function(e)
	{
		ReadTrigger(e)
		//if (version < 53) 
		//	e.ReadBool("trigger_on_wear")
		//else
				e.ReadU8("trigger_state")
	},
	"triggers/nightvision_added": ReadTrigger,
	"triggers/npc": function(e)
	{
		ReadTriggerZone(e)
		e.ReadIdentifierArray("npc_type")
		e.ReadBool("_check_on_activate")
	},
	"triggers/npc enemy": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadIdentifierArray("npc_type")
	},
	"triggers/npc enemy is close": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadFP32("distance")
		e.ReadFP32("distance_far")
		e.ReadIdentifier("npc_type") // identifier probably
	},
	"triggers/npc mental state": function(e)
	{
		ReadTrigger(e) // Exodus: changed ReadTrigger_ee to ReadTrigger
		e.ReadU8("min_mental_state")
		e.ReadU8("max_mental_state")
	},
	"triggers/npc threat": ReadTrigger,
	"triggers/npc/threat_orientation": ReadTriggerEntityOrientation,
	"triggers/npc_die_from": function(e)
	{
		ReadTrigger(e)
		e.ReadHint("hit_types", "flags32") // NEW in Exodus !!!
		e.ReadU32("hit_types", "u32")      // NEW in Exodus !!!
		e.ReadBool("any_hit_type")         // NEW in Exodus !!!
		e.ReadIdentifier("weapon_type")
		e.ReadHintStr("bone", "attp_str")  // NEW in Exodus !!!
		e.ReadIdentifier("npc_type")
		e.ReadU8("friend")
		e.ReadU8("mp_class_type")
		e.ReadFP32("threshold_min")
		e.ReadFP32("threshold_max")
	},
	"triggers/npc_in_cover": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("cover_type")
	},
	"triggers/object": function(e)
	{
		ReadTriggerZone(e)
		e.ReadU16("object", "entity_link, uobject_link")
	},
	"triggers/object_is_close": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("distance")
	},
	"triggers/open backpack": ReadTrigger,
	"triggers/out of filters": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("border_time")
		e.ReadBool("filter")
	},
	"triggers/pass cover": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("once")
		e.ReadU8("cover_type")
	},
	"triggers/pathfind enemy": ReadTrigger,
	"triggers/player": function(e)
	{
		ReadTriggerZone(e)
		e.ReadBool("check_on_activate")
	},
	"triggers/player die": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("bullet_kill")
		e.ReadIdentifier("npc_type")
	},
	"triggers/player in combat": ReadTrigger,
	"triggers/player jumpon": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("upper")
		e.ReadBool("in_air")
	},
	"triggers/player jumpover": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("upper")
	},
	"triggers/player map": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("mode")
	},
	"triggers/player on ai map": ReadTrigger,
	"triggers/player sliding": ReadTrigger,
	"triggers/player's state": function(e)
	{
		ReadTrigger(e)
		e.ReadBool8("state", ["sprinting", "running", "walking", "jumping", "crouching", "fwd_moving", "falling"]);
	},
	"triggers/player_special": function(e)
	{
		ReadTriggerZoneSpecial(e)
		e.ReadIdentifier("npc_type")
	},
	"triggers/session countdown": function(e)
	{
		ReadTrigger(e)
		ReadTime(e, "total_time")
		ReadTime(e, "warning_time")
		ReadTime(e, "death_time")
	},
	"triggers/smell": ReadTrigger_ee,
	"triggers/start_script": ReadTrigger,
	"triggers/startgame": function(e)
	{
		ReadTrigger(e)
		e.ReadBool8("flags", ["disable_when_ctrl_f5"])
	},
	"triggers/stopvideo": ReadTrigger,
	"triggers/torch": ReadTrigger,
	"triggers/use": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadFP32("usage_distance")
		e.ReadHintStr("use_action", "choose")
		//e.ReadVec2("use_offset") // REMOVED in Exodus !!!
		e.ReadFP32("blink_distance")
		e.ReadBool8("flags8", ["blink", "check_need_end", "ignore_busy"])
		e.ReadU8("user_team")
		e.ReadBool("in_reloading")
		e.ReadU8Array("mp_classes")
		e.ReadBool("trigger_only") // NEW in Arktika.1 !!!
		ReadTime(e, "hold_count") // Exodus: changed ReadU32 to ReadTime
	},
	"triggers/use multiimage": function(e)
	{
		//
		ReadTrigger_ee(e)
		e.ReadFP32("usage_distance")
		//e.ReadHintStr("use_action", "choose")
		//e.ReadVec2("use_offset") // REMOVED in Exodus !!!
		e.ReadFP32("blink_distance")
		e.ReadBool8("flags8", ["blink", "check_need_end", "ignore_busy"])
		e.ReadU8("user_team")
		e.ReadBool("in_reloading")
		e.ReadU8Array("mp_classes")
		e.ReadBool("trigger_only") // NEW in Arktika.1 !!!
		ReadTime(e, "hold_count") // Exodus: changed ReadU32 to ReadTime
		//
		
		e.ReadU8("active_image")
		e.ReadU8("count_images")
		var arr = e.ReadArray("images")
		while(arr.MoreElements())
		{
			var rec = arr.NextElement()
			rec.ReadHintStr("use_action", "choose")
		}
	},
	"triggers/vehicles/check_player": function(e)
	{
		ReadTrigger(e)
		e.ReadHint("vehicle_types", "flags16")
		e.ReadU16("vehicle_types")
	},
	"triggers/vehicles/accel": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("input_type")
		e.ReadBool8("flags0", ["allow_input"])
		e.ReadU32("direction")
	},
	"triggers/vehicles/hit": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("hit_value")
		e.ReadHint("hit_direction", "flags16")
		e.ReadU16("hit_direction")
	},
	"triggers/vehicles/position": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("max_deviation", "angle, fp32")
		e.ReadBool("inverse")
	},
	"triggers/vehicles/speed": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("speed_value")
		e.ReadBool8("flags0", ["abs_values", "check_on_activate", "drifting"])
	},
	"triggers/vehicles/state": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("state_type")
	},
	"triggers/vehicles/suspension": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("suspension_value")
		e.ReadBool("use_average")
		e.ReadU8("wheel")
	},
	"triggers/velocity": function(e)
	{
		ReadTrigger(e)
		e.ReadU16("entity", "entity_link, uobject_link")
		e.ReadFP32("velocity")
		e.ReadBool("dir_x") // NEW in Exodus !!!
		e.ReadBool("dir_y") // ..
		e.ReadBool("dir_z") // ..
		e.ReadBool("local_space") // ..
	},
	"triggers/visibility": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("fov", "angle, fp32")
	},
	"triggers/watchman group": ReadTrigger,
	"triggers/weapon taking": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("equal_weapon")
	},
	"triggers/weapon_active_state": ReadTrigger,
	"triggers/web_burn": ReadTrigger_ee,
	"triggers/xbox adaptive controller": ReadTrigger,
	"triggers/zombie": ReadTrigger_ee,
	"trolley/kulemet mode": function(e)
	{
		e.ReadVec2("limit_yaw")
		e.ReadVec2("limit_pitch")
		e.ReadFP32("limit_factor")
		e.ReadFP32("speed_in")       // NEW in Exodus !!!   
		e.ReadFP32("speed_out")      // NEW in Exodus !!!
		e.ReadBool("deactivate_on_use")
		e.ReadU16("npc", "entity_link, uobject_link")
	},
	"trolley/kulemet one shot": null,
	"trolley/lock on target": function(e)
	{
		e.ReadBool("open_fire")
		e.ReadBool("keep_direction") // NEW in Exodus !!!
		e.ReadBool("ai_sound")       // NEW in Exodus !!!
	},
	
	// how to deal with this shit properly? game doesn't support there blocks, but they're used a lot in editor
	"subscript/script" : null,
	"subscript/script_block_input": null,
	"subscript/script_block_output": null,
	"subscript/script_block_reference": null,
	"subscript/script_cover_reference": null,
}