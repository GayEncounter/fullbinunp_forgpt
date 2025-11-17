// interface
this.ReadVssVer6 = ReadVssVer6
this.ReadBlock = ReadBlock

// implementation
var already_printed = new Object // not implemented classes
var vs_clsid = module("vs_clsid")

var entity_ver;

function ReadVssVer6(e, entity_version)
{
	entity_ver = entity_version;

	var v = e.TryReadArray("vss_ver_6") || e.ReadArray("vss_ver_7")
	
	for(var i = 0; v.More(); i++)
	{
		var rec = v.ReadSection(RecStr("rec_", i, 4), false)
		ReadVS(rec)
	}
}

function ReadVS(e)
{
	var groups = e.ReadArray("groups") // it's even used ???
	
	var blocks = e.ReadSection("blocks")
	
	blocks.ReadU16("version")
	blocks.ReadU32("block_count")
	blocks.ReadHint("array with no key", "array")
	blocks.ReadU32("count")

	for(var i = 0; blocks.More(); i++)
	{
		var block = blocks.ReadSection(RecStr("block_", i, 4), false)
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

function ReadTime(e, name)
{
	e.ReadHint(name, "time")
	e.ReadU32(name)
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
			ReadBlock(b);
		}
	}
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
	e.ReadHintStr("bone", "choose")
	e.ReadHintStr("font", "choose")
	e.ReadU8("flags0", "bool8")
	e.ReadString("group_id")
}

function ReadActionEngineClone(e)
{
	e.ReadU32("cover_reuse_delay")
	e.ReadU8("count")
	e.ReadString("tag") // NEW in Redux!!
	e.ReadU16("source", "entity_link, uobject_link")
	e.ReadU16("position", "entity_link, uobject_link")
	e.ReadU8("max_saved_cloned") // NEW in Arktika.1 !!! 
}

function ReadActionEngineSignal(e)
{
	e.ReadHintStr("signal", "choose")
	e.ReadU8("mode")
}

function ReadActionHit(e)
{
	//e.ReadU16("initiator", "entity_link, uobject_link"); // REMOVED in Arktika.1
	e.ReadFP32("amount")
	e.ReadHintStr("bone", "choose_array, str_shared") // Arktika.1: changed 'bone_str' to 'choose_array, str_shared'
	e.ReadU8("hit_type")
	//e.ReadBool("forbid_ai") // REMOVED in Arktika.1
	e.ReadU8("flags", "bool8") // NEW in Arktika.1 !!!
}

function ReadActionMove(e)
{
	e.ReadU8("flags", "bool8")
	e.ReadU8("ex_flags", "bool8")
	e.ReadU8("smooth_type")
	e.ReadVec3("offset")
	e.ReadVec3("offset_angle", "ang3f")
	ReadTime(e, "interp_time")
	ReadTime(e, "delay_min")
	ReadTime(e, "delay_max")
	e.ReadHintStr("object_attp_id", "attp_str")
}

function ReadActionPlayParticles(e)
{
	e.ReadHintStr("particles", "choose")
	e.ReadU8("particles_flags", "bool8")
	e.ReadU32("particles_color", "color, u32")
}

function ReadActionPlayMotion(e)
{
	e.ReadHintStr("dbg_model", "choose")  // Arktika.1: changed 'ref_model' to 'choose'
	e.ReadString("dbg_skel")
	e.ReadHintStr("animation", "animation_str")
	e.ReadHintStr("bone_part", "part_str") // Arktika.1: changed 'part_id' to 'part_str'
	e.ReadFP32("blend")
	e.ReadFP32("speed")
	e.ReadU8("force_looped")
	e.ReadU8("flags", "bool8") // overlay, inverted, speed_replace
}

function ReadActionPlayMotionControl(e)
{
	e.ReadHintStr("dbg_model", "choose")  // Arktika.1: changed 'ref_model' to 'choose'
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
	e.ReadU8("flags", "bool8")
	e.ReadU16("fwd_start_frame") // NEW in Arktika.1 !!!
	e.ReadU16("fwd_end_frame")   // ..
	e.ReadU16("bwd_start_frame") // ..
	e.ReadU16("bwd_end_frame")   // ..
}

function ReadActionPlaySound(e)
{
	e.ReadHintStr("sound", "choose") // // Arktika.1: changed 'sound' to 'choose'
	e.ReadFP32("volume")
	e.ReadU8("sound_filter")
	e.ReadU8("sound_bus")
	e.ReadU8("flags0", "bool8")
	e.ReadU32("ai_sound_type")
	e.ReadFP32("stop_interval")
	e.ReadFP32("startus_intervalus")
	e.ReadFP32("start_delay") // NEW in Arktika.1 !!!
	e.ReadFP32("end_cb_offset") // NEW in Arktika.1 !!!
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
	e.ReadU32("interest_duration")
	e.ReadFP32("speed_coef")
	e.ReadU8("eyes_speed")
	e.ReadFP32("max_angle")
	e.ReadFP32("clamp_angle")
	e.ReadFP32("look_probability")
	e.ReadU32("ignore_angle_timeout")
	e.ReadBool("aim_interest"); // NEW in Arktika.1
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
	e.ReadU8("dflags")
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
	e.ReadU8("flags8", "bool8") // nested_enter, nested_leave
}

var block_readers = {
	"actions/action_set_interest_ex": ReadActionSetInterest,
	"actions/activate-deactivate": function(e)
	{
		e.ReadBool("register")
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
	"actions/ai/enable_melee_attacks": function(e)
	{
		var cnt = e.ReadU32("melee_attacks");
		for(var i = 0; i < cnt; i++)
		{
			e.ReadHint("melee_attacks_"+i, "flags32")
			e.ReadU32("melee_attacks_"+i)
		}
	},
	"actions/ai/follow": function(e) // not used in game, but engine supports it
	{
		e.ReadU16("leader", "entity_link, uobject_link")
		e.ReadU16("wait_point", "entity_link, uobject_link")
		e.ReadHint("flags", "flags32")
		e.ReadU32("flags")
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
	"actions/ai/lead": function(e) // not used in game, but engine supports it
	{
		e.ReadU16("follower", "entity_link, uobject_link")
		e.ReadU16("goal_point", "entity_link, uobject_link")
		e.ReadU16("next_point", "entity_link, uobject_link")
		e.ReadU16("wait_point", "entity_link, uobject_link")
		e.ReadHint("flags", "flags32")
		e.ReadU32("flags")
		e.ReadFP32("wait_distance_min")
		e.ReadFP32("wait_distance_max")
		e.ReadFP32("lead_distance_min")
		e.ReadFP32("lead_distance_max")
		e.ReadFP32("retrieve_distance_min")
		e.ReadFP32("retrieve_distance_max")
		e.ReadFP32("success_distance")
		e.ReadFP32("max_distance")
	},
	"actions/ai/monster face enemy": function(e)
	{
		e.ReadU8("flags0", "bool8")
		e.ReadFP32("_face_enemy_stand_speed")
	},
	"actions/ai/state": function(e)
	{
		// patrol state
		e.ReadString("body_state")
		e.ReadString("anim_state")
		e.ReadString("movement_type")
		e.ReadString("weapon_state")
		e.ReadString("action")
		e.ReadU16("target", "entity_link, uobject_link")
		e.ReadHint("flags", "flags32")
		e.ReadU32("flags")
		e.ReadFP32("anim_state_approach_speed")
		e.ReadFP32("approaching_accel")
	},
	"actions/allow_vr_vision": null,
	"actions/alt_ammo_mode": null,
	"actions/attach": function(e)
	{
		e.ReadHintStr("dbg_model", "choose") // NEW in Arktika.1 !!!
		e.ReadString("dbg_skel") // NEW in Arktika.1 !!!
		//e.ReadU16("owner", "entity_link, uobject_link")
		e.ReadHintStr("bone", "attp_str")
		e.ReadMatrix43("offset", "pose, matrix_43T")
		e.ReadU8("flags", "bool8")
	},
	"actions/attach to HUD": function(e)  // NEW in Arktika.1 !!!
	{
		e.ReadString("attp")
		e.ReadMatrix43("offset", "pose, matrix_43T")
		e.ReadU8("vr_only")
		e.ReadBool("auto_offset")
		e.ReadBool("camera_ignore_rot")
	},
	"actions/attach vs": function(e)
	{
		e.ReadString("name")
		e.ReadBool("disable_qsave") // NEW in Arktika.1 !!!
		ReadVsRef(e, "vs") 
	},
	"actions/attach_vr": function(e) // NEW in Arktika.1 !!!
	{
		e.ReadMatrix43("offset", "pose, matrix_43T");
		e.ReadU8("attp_vr")
		e.ReadU8("tracker_id")
		e.ReadBool("auto_offset")
	},
	"actions/attack_hit_shell": null,
	"actions/block_hand": function(e) // NEW in Arktika.1 !!!
	{
		e.ReadBool("left")
		e.ReadBool("right")
	},
	"actions/block touch input": null,
	"actions/body state": function(e) // NEW in Arktika.1 !!!
	{
		e.ReadU8("body_state") // ebody_state_token, смотри PDB
	},
	"actions/cap_health": function(e)
	{
		e.ReadFP32("low_health_border")
		e.ReadFP32("high_health_border")
		e.ReadBool("full_protection")
		e.ReadBool("exclude_collision") // NEW in Redux!!
	},
	"actions/citadel/complete_objective": function(e)
	{
		e.ReadU32("level")
		e.ReadU16("phase")
		e.ReadBool("save_profile")
		e.ReadU8("objective")
	},
	"actions/citadel/main menu": function(e)
	{
		e.ReadU32("level")
		e.ReadU16("phase")
		e.ReadU8("menu_flags", "bool8")
	},
	"actions/citadel/perks_activate": function(e)
	{
		e.ReadU8("_perk_idx")
	},
	"actions/citadel/perks_small": null,
	"actions/citadel/perks_unlock": function(e)
	{
		e.ReadBool("_challenge_mode")
		e.ReadU8("_perk_idx")
	},
	"actions/citadel/select_level": function(e)
	{
		e.ReadU32("level")
		e.ReadU16("phase")
		e.ReadBool("save_profile")
	},
	"actions/citadel/stats": function(e)
	{
		e.ReadU32("stat")
		e.ReadS32("val")
	},
	"actions/citadel/store": null,
	"actions/citadel/store purchase": function(e)
	{
		e.ReadS32("upgrade")
	},
	"actions/citadel/unlock_level": function(e)
	{
		e.ReadU32("level")
		e.ReadU16("phase")
		e.ReadBool("save_profile")
	},
	"actions/citadel/unlock_vr_mission": function(e)
	{
		e.ReadU8("mission_id")
	},
	"actions/collision_group": function(e)
	{
		e.ReadU8("collisions_group")
	},
	"actions/connect_vcamera": null,
	"actions/console command": function(e)
	{
		e.ReadString("cmd")
	},
	"actions/cover_combat": function(e)
	{
		e.ReadBool("make_cover_combat")
	},
	"actions/cover_task_params": function(e)
	{
		e.ReadU32("enemy_seen_timeout")
		e.ReadU32("lookout_min")
		e.ReadU32("lookout_max")
		e.ReadU32("aim_while_lookout_timeout")
		e.ReadU32("lookout_cooldown_min")
		e.ReadU32("lookout_cooldown_max")
		e.ReadU32("lookout_cooldown_min_far")
		e.ReadU32("lookout_cooldown_max_far")
		e.ReadFP32("lookout_cooldown_dist_near")
	},
	"actions/destroy": function(e)
	{
		e.ReadBool("report_dead") // NEW in Arktika.1 !!!
	},
	"actions/destroy joint": null,
	"actions/detach": function(e)
	{
		e.ReadU16("owner", "entity_link, uobject_link")
		e.ReadBool("check_parent")
	},
	"actions/disable_all_use": function(e)
	{
		e.ReadString("reason")
	},
	"actions/disable_npc_collision": null,
	"actions/disable_teleport": null,
	"actions/engine/3d_arcade_score": function(e)
	{
		ReadActionEngine3DText(e)
		e.ReadBool("total")
		e.ReadBool("high_score")
		e.ReadU32("level")
		e.ReadU8("stage")
	},
	"actions/engine/3d_citadel_money_text": ReadActionEngine3DText,
	"actions/engine/3d_text": ReadActionEngine3DText,
	"actions/engine/3d_user_name_text": ReadActionEngine3DText,
	"actions/engine/auramode": function(e)
	{
		e.ReadBool("hud_mode") // NEW in Arktika.1 !!!
	},
	"actions/engine/clone": ReadActionEngineClone,
	"actions/engine/counter control": function(e) // NEW in Arktika.1 !!!
	{
		ReadTime(e, "time")
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
	"actions/engine/make_ghost": null,
	"actions/engine/measure fps": null,
	"actions/engine/play_hud_particles": function(e)
	{
		e.ReadHintStr("particles", "choose") // Arktika.1: changed 'particles, str_shared' to 'choose'
		e.ReadU16("hud_particles_type")
		e.ReadU8("particles_flags", "bool8") // particles_constrained, allow_in_vr, allow_parent_velocity, persistent, ignore_fov, deferred_stop, particles_ignore_parent_rotation
	},
	"actions/engine/quick_load": function(e)
	{
		e.ReadString("filename")
	},
	"actions/engine/quick_save": function(e)
	{
		e.ReadString("filename")
	},
	"actions/engine/replace model": function(e)
	{
		e.ReadHintStr("model", "choose") // Arktika.1: changed 'ref_model' to 'choose'
		e.ReadBool("preserve") // NEW in Arktika.1 !!!	
	},
	"actions/engine/replace tpreset": function(e)
	{
		e.ReadBool("as_preset")
		e.ReadHintStr("tex_preset", "choose")
	},
	"actions/engine/restore tpreset": function(e)
	{
		e.ReadBool("restore_all")
	},
	"actions/engine/scale": function(e) // NEW in Redux!!
	{
		e.ReadVec3("scale")
	},
	"actions/engine/set background music": function(e)
	{
		e.ReadHintStr("back_music", "choose")
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
		e.ReadFP32("volume")
		e.ReadBool("start_at_random") // NEW in Arktika.1 !!!	
	},
	"actions/engine/set_menu_label": function(e)
	{
		e.ReadHintStr("menu_name", "choose")
		e.ReadString("label_name")
		e.ReadHintStr("text", "choose")
		e.ReadS32("idx")
	},
	"actions/engine/set_weather": function(e)
	{
		e.ReadHintStr("preset_name", "choose")
		e.ReadFP32("blend_time")
		e.ReadFP32("falloff")
		e.ReadBool("time_mode")
	},
	"actions/engine/shadowcast": null,
	"actions/engine/show_menu": function(e)
	{
		e.ReadBool("show_only_menu")
		e.ReadHintStr("menu_name", "choose")
	},
	"actions/engine/signal": ReadActionEngineSignal,
	"actions/engine/slowmo": function(e)
	{
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
		e.ReadFP32("value")
		e.ReadFP32("current_value")
		e.ReadBool("active")
		e.ReadBool("registred")
	},
	"actions/engine/texture prestream": function(e)
	{
		e.ReadHintStr("textures", "choose_array, str_shared")
	},
	"actions/engine/volume": function(e)
	{
		e.ReadFP32("volume")
		e.ReadU32("time")
		e.ReadU8("bus")
	},
	"actions/engine/vr_menu_input": function(e)
	{
		e.ReadHintStr("menu_name", "choose")
		e.ReadBool("left_hand")
		e.ReadHintStr("bone_name", "attp_str")
	},
	"actions/entity/action_aux_prop": function(e)
	{
		e.ReadU32("prop0")
		e.ReadU32("prop1")
	},
	"actions/entity/add gesture": function(e)
	{
		e.ReadHintStr("dbg_model", "choose") // Arktika.1: changed 'ref_model' to 'choose'
		e.ReadString("dbg_skel")
		e.ReadHintStr("seq_name", "animation_str")
		e.ReadHintStr("bone_part", "part_str") // Arktika.1: changed 'part_id' to 'part_str'
		e.ReadU8("force_looped")
	},
	"actions/entity/blink": function(e)
	{
		e.ReadBool("blink_prevent")
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
		e.ReadU8("flags", "bool8") // dao_auto, dao_imm
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
	"actions/force_field": function(e)
	{
		e.ReadU32("ff_vision")
		
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
	},
	"actions/freegun": null,
	"actions/game/update_play_stats": null,
	"actions/goto_target": function(e)
	{
		e.ReadFP32("target_range")
		e.ReadFP32("not_going_range")
		e.ReadU8("goto_movement_type")
		e.ReadU8("arrival_type")
		e.ReadU8("flags", "bool8") // movement_type, int_heavy_damage, int_light_damage, exact_rotation, need_turn, do_not_unlock_cover
		e.ReadFP32("distance_to_cover") // NEW in Arktika.1 !!!
		e.ReadFP32("approach_range") // NEW in Arktika.1 !!!
	},
	"actions/hit": ReadActionHit,
	"actions/holster_weapon": function(e)
	{
		e.ReadU8("slot")
	},
	"actions/human/combat_type": function(e)
	{
		e.ReadU8("combat_type")
	},
	"actions/human/suppress cover": null,
	"actions/make friend": function(e)
	{
		e.ReadU8("make_friend_type")
	},
	"actions/make immortal": function(e)
	{
		e.ReadBool("check_only")
	},
	"actions/make scripted": function(e)
	{
		e.ReadBool("make_scripted")
	},
	"actions/min mental state": function(e)
	{
		e.ReadU8("mental_state")
		e.ReadU8("max_mental_state")
	},
	"actions/move": ReadActionMove,
	"actions/move project": function(e)
	{
		e.ReadU16("step")
	},
	"actions/movement type": function(e)
	{
		e.ReadU8("min_movement_type")
		e.ReadU8("max_movement_type")
		e.ReadFP32("step_acceleration")
	},
	"actions/net/net_prop": function(e)
	{
		e.ReadU32("prop_id")
		e.ReadS32("prop_delta")
		e.ReadString("comments")
	},
	"actions/net/respawn": function(e) // not used in game, but engine supports it
	{
		ReadActionEngineClone(e)
		e.ReadU8("team")
		e.ReadU8("flags0", "bool8")
	},
	"actions/new virtual grab": function(e)
	{
		e.ReadFP32("index")
		e.ReadFP32("thumb")
		e.ReadFP32("palm")
		e.ReadBool("lego")
		e.ReadHintStr("hand_bone", "attp_str")
		e.ReadFP32("min_delta")
		e.ReadFP32("stop_time")
		e.ReadBool("concentric")
	},
	"actions/no_combat": function(e)
	{
		e.ReadBool("make_no_combat")
	},
	"actions/npc/action feelings": function(e)
	{
		e.ReadU8("flags", "bool8")
	},
	"actions/npc/anim_speed": function(e)
	{
		e.ReadFP32("anim_speed")
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
	"actions/npc/dispersion_props": function(e)
	{
		e.ReadU32("dispersion_decrease_time")
		e.ReadU32("dispersion_increase_time")
		e.ReadFP32("min_shoot_dispersion_coef")
		e.ReadFP32("max_shoot_dispersion_coef")
	},
	"actions/npc/group": function(e)
	{
		e.ReadString("group_id")
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
			//"scripted_max",
			//"invalid"
		].forEach(function(hit) { e.ReadFP32(hit); })
	},
	"actions/npc/obstacle": function(e)
	{
		e.ReadHintStr("obstacle", "choose")
		e.ReadBool("add_obstacle")
		e.ReadBool("check_objects")
	},
	"actions/npc/reload": function(e) // NEW in Redux!!
	{
		e.ReadBool("if_need")
	},
	"actions/npc/restrictor_block_ai_vision": null,
	"actions/one shot": function(e)
	{
		e.ReadU8("flags0", "bool8") // ai_sound, disable_effects, skip_delta
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
		e.ReadU8("flags0", "bool8"); // instant, ignore_ammo, ai_sound
	},
	"actions/p-force": function(e)
	{
		e.ReadFP32("min_amount")
		e.ReadFP32("max_amount")
		e.ReadFP32("max_influence_dst")
		e.ReadString("bone")
		e.ReadVec3("dir")
		e.ReadU8("flags0", "bool8")
	},
	"actions/particles color": function(e)
	{
		e.ReadVec4("color", "color, vec4f")
	},
	"actions/play camera-effect": function(e)
	{
		e.ReadHintStr("cameraeffect", "choose") // Arktika.1: changed 'camera_track, str_shared' to 'choose'
		e.ReadFP32("value")
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
		e.ReadFP32("inc_delta")
		e.ReadFP32("inc_speed")
		e.ReadFP32("dec_delta")
		e.ReadFP32("dec_speed")
		e.ReadFP32("max_dist")
		e.ReadFP32("min_fade")
		e.ReadU8("flags0", "bool8")
		e.ReadU16("max_value_point", "entity_link, uobject_link")
		ReadTime(e, "vis_fade_in") // NEW in Arktika.1 !!!
		ReadTime(e, "vis_fade_out") // NEW in Arktika.1 !!!
	},
	"actions/play coloranim": function(e)
	{
		e.ReadHintStr("coloranim", "choose")  // Arktika.1: changed 'ref_coloranim' to 'choose'
		e.ReadBool("looped")
		e.ReadBool("play_from_end") // NEW in Arktika.1 !!!
	},
	"actions/play cover path": function(e)
	{
		e.ReadBool("fast_switch")
		e.ReadBool("fast_callback")
		e.ReadBool("take_last_cover")
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
		e.ReadU8("flags0", "bool8")
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
		e.ReadHint("flags", "flags32");
		e.ReadU32("flags");
		e.ReadFP32("anim_state_approach_speed");
		e.ReadFP32("approaching_accel");
		
		e.ReadU8("flags0", "bool8") // match_exact_goal, fire_while_move
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
		e.ReadU8("new_flags", "bool8") // NEW in Arktika.1 !!!
		e.ReadU8("_subsequent") // NEW in Arktika.1 !!!
		e.ReadU16Array16("weapon_tags") // NEW in Arktika.1 !!!
	},
	"actions/play vibration": ReadActionPlayVibration,
	"actions/play vibration VR": function(e)
	{
		ReadActionPlayVibration(e)
		
		e.ReadU8("frequency_0")
		e.ReadU8("frequency_1")
	},
	"actions/player/arcade_score": function(e)
	{
		e.ReadU32("val")
	},
	"actions/player/arcade_score_commit": function(e)
	{
		e.ReadU32("level")
		e.ReadBool("force")
	},
	"actions/player/citadel_money": function(e)
	{
		e.ReadS32("val")
	},
	"actions/player/disable_craft": null,
	"actions/player/doctor": function(e)
	{
		e.ReadU32("boost_time")
	},
	"actions/player/mount_vehicle": null,
	"actions/player/restrictor_obstacle": null,
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
	"actions/scary_face": null,
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
		e.ReadU8("allow")           // NEW in Arktika.1
		e.ReadU8("allow_on_detach") // ..
		e.ReadU8("raycast")         // ..
		e.ReadU8("raycast_ai")      // ..
		e.ReadU8("clg_from_model")  // ..
		e.ReadU8("destroy_on_contact") // ..
		e.ReadU8("semi_breakable")  // ..
		e.ReadU8("sleeping")        // ..
		e.ReadBool("mod_collision_grp") // ..
		e.ReadU8("collision_group") // ..
	},
	"actions/set_mech_offset": function(e)
	{
		e.ReadFP32("offset")
	},
	"actions/set_name": function(e)
	{
		e.ReadString("name")
	},
	"actions/set_speech_groups": function(e)
	{
		e.ReadString("allowed_speech_groups")
	},
	"actions/show_hitmark": function(e)
	{
		e.ReadBool("_hit_attach")
		e.ReadBool("_hit_friend")
		e.ReadFP32("_hit_power")
	},
	"actions/static_combat": function(e)
	{
		e.ReadU8("static_combat_flags", "bool8")
		e.ReadHintStr("model", "choose") // Arktika.1: changed 'ref_model' to 'choose'
		e.ReadString("dbg_skel")
		e.ReadHintStr("static_idle", "animation_str")
		e.ReadHintStr("static_attack", "animation_str")
		e.ReadHintStr("static_reload_idle", "animation_str")
		e.ReadHintStr("static_shoot", "animation_str");      // NEW in Arktika.1 !!!
		e.ReadHintStr("static_turn180l", "animation_str");   // ..
		e.ReadHintStr("static_turn90l", "animation_str");    // ..
		e.ReadHintStr("static_turn0", "animation_str");      // ..
		e.ReadHintStr("static_turn90r", "animation_str");    // ..
		e.ReadHintStr("static_turn180r", "animation_str");   // ..
		e.ReadHintStr("static_turn_idle", "animation_str");  // ..
		e.ReadHintStr("static_sit_idle", "animation_str");   // ..
		e.ReadHintStr("static_stand_idle", "animation_str"); // ..
		e.ReadHintStr("static_in", "animation_str");         // ..
		e.ReadHintStr("static_out", "animation_str");        // ..
		e.ReadFP32("min_distance")                           // ..
		e.ReadFP32("max_distance")                           // ..
		e.ReadFP32("target_distance")                        // ..
		e.ReadFP32("attack_angle")                           // ..
	},
	"actions/static_combat_anchor": function(e)
	{
		e.ReadFP32("radius")
		e.ReadFP32("enemy_dist")
	},
	"actions/take_cover": function(e)
	{
		e.ReadU32("clear_cover_time") // NEW in Arktika.1
		e.ReadBool("_keep_force_cover") // NEW in Arktika.1
	},
	"actions/teleport_control": function(e)
	{
		e.ReadU8("mode")
		e.ReadBool("restart_cooldown")
	},
	"actions/toggle": null,
	"actions/turn light": function(e)
	{
		e.ReadU8("onoff")
		e.ReadBool("on_or_off")
	},
	"actions/turret_control": function(e)
	{
		e.ReadBool("open_and_rotate")
		e.ReadFP32("rotation_speed")
		e.ReadFP32("speed_step")
	},
	"actions/update ik": null,
	"actions/virtual boundries": function(e)
	{
		e.ReadVec3("aux_color")
	},
	"actions/virtual grab": function(e)
	{
		e.ReadFP32("index")
		e.ReadFP32("thumb")
		e.ReadFP32("palm")
		e.ReadBool("lego")
		
		var m = e.ReadSection("mech")
		m.ReadBool("set_mechanism_params")
		m.ReadHintStr("mech_bone", "attp_str")
		
		m.ReadHintStr("hand_bone", "attp_str")
		m.ReadBool("move")
		m.ReadU8("axis")
		m.ReadBool("ignore_limits")
		m.ReadFP32("limit_min")
		m.ReadFP32("limit_max")
		m.ReadFP32("limit_trig_norm")
		m.ReadFP32("min_delta")
		m.ReadFP32("stop_time")
		m.ReadFP32("vel_dampen")
		m.ReadFP32("vel_multi")
		m.ReadBool("concentric")
	},
	"actions/voice": function(e)
	{
		e.ReadHintStr("voice", "choose")
	},
	"actions/vs_move": function(e) // NEW in Arktika.1 !!!
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
	"actions/weapon modular/power mode": null,
	"actions/weapon/dispersion": function(e)
	{
		e.ReadFP32("dispersion")
	},
	"actions/weapon/pump": function(e)
	{
		e.ReadFP32("_time")
	},
	"actions/weapon/rocket_launch": function(e)
	{
		e.ReadFP32("power")
		e.ReadU8("flags", "bool8") // fake, rotate_to_fwd
	},
	"actions/weapon/safelock": null,
	"actions/weapon/weapon_lock_target": null,
	"actions/wear_suit": function(e)
	{
		e.ReadU32("suit_type")
	},
	"actions/zone type" : function(e)
	{
		// e.ReadU8("zone_type") // REMOVED in Arktika.1
		e.ReadHint("type_mask", "flags64") // NEW in Arktika.1 !!!
		e.ReadU64("type_mask") // NEW in Arktika.1 !!!
	},
	"checker/entity/bone inside": function(e)
	{
		e.ReadHintStr("dbg_model", "choose")  // Arktika.1: changed 'ref_model' to 'choose'
		e.ReadString("dbg_skel")
		e.ReadHintStr("bone", "attp_str")
		
	},
	"checker/entity/is_same": function(e)
	{
		e.ReadBool("all")
	},
	"checker/entity/last_in_zone": null,
	"checker/entity/velocity": function(e)
	{
		e.ReadFP32("min")
		e.ReadFP32("max")
	},
	"checker/platform": null,
	"checker/previous_map": null,
	"covers/ref": function(e)
	{
		e.ReadU16("target", "cover_link, ucover_link")
	},
	"debug/simulate_start": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("unk") // engine doesn't read this
	},
	"entities/entity children": null,
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
		e.ReadString("name")
	},
	"entities/npc_enemy_ex": function(e) // NEW in Arktika.1 !!!
	{
		e.ReadU32Array("npc_type", "identifier_array")
		e.ReadBool("include_player")
	},
	"entities/entity parent": null,
	"entities/entity player": function(e)
	{
		e.ReadU8("team");
	},
	"entities/entity player's torch": null,
	"entities/entity ref": function(e)
	{
		e.ReadU16("target", "entity_link, uobject_link")
	},
	"entities/entity self": null,
	"entities/filters/nearest item": null,
	"entities/holstered_weapon": function(e)
	{
		e.ReadU8("holstered")
		e.ReadBool("right_slot")
		e.ReadBool("allow_in_hand")
	},
	"entities/nearest_enemies": function(e)
	{
		e.ReadU8("count")
		e.ReadFP32("size")
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
	"logic/hub": null,
	"logic/locker": function(e)
	{
		e.ReadU8("flags", "bool8")
	},
	"logic/locker_base": function(e)
	{
		e.ReadU8("flags", "bool8")
	},
	"logic/or-2": function(e)
	{
		e.ReadBool("auto_touch")
	},
	"logic/or-3": function(e)
	{
		e.ReadBool("auto_touch")
	},
	"logic/random": function(e)
	{
		e.ReadU8("quant")
	},
	"logic/random-chooser": ReadLogicRandomChooser,
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
	"logic/switchboard": function(e)
	{
		e.ReadU8("quant")
		var count = e.ReadU8("states_count")
		for(var i = 0; i < count; i++)
			e.ReadBool("on_"+i)
	},
	"logic/switch_param": function(e)
	{
		e.ReadU8("quant")
		e.ReadU8("type")
	},
	"trade/take item": function(e)
	{
		e.ReadU8('tradeflags8', 'bool8') // creating, force_activation, vs_use
	},
	"trigger/vision": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadFP32("vision_value")
	},
	"triggers/aftercloned": function(e)
	{
		ReadTrigger(e)
		e.ReadString("tag")
	},
	"triggers/ammo absent": function(e)
	{
		ReadTrigger(e)
		e.ReadU16("min_ammo_count")
		e.ReadBool("loaded")
	},
	"triggers/anim event": function(e)
	{
		ReadTrigger(e)
		e.ReadU16("entity", "entity_link, uobject_link")
		e.ReadString("event")
	},
	"triggers/attach-detach": ReadTrigger,
	"triggers/block_explosion": ReadTrigger,
	"triggers/citadel/arktika mainmenu": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("level")
	},
	"triggers/citadel/level_selected": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("level")
		e.ReadU16("phase")
	},
	"triggers/citadel/perks": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("_challenge_mode")
		e.ReadU8("_perk_idx")
	},
	"triggers/citadel/teleport": ReadTrigger,
	"triggers/citadel/vr_mission_trigger": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("mission_id")
	},
	"triggers/citadel_purchase": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("_unlock_unpgrade")
		e.ReadU8("_unlock_upgrade_idx")
	},
	"triggers/collision": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("super_static") // NEW in Arktika.1 !!!
		e.ReadU32("collisions_group")
		e.ReadBool("only_arrows") // NEW in Arktika.1 !!!
	},
	"triggers/curve_shot": ReadTrigger,
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
		e.ReadBool("from_fire")
	},
	"triggers/engine/graphics_changed": ReadTrigger,
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
	"triggers/entity/orientation": function(e)
	{
		e.ReadFP32("fwd_ang", "angle, fp32")
		e.ReadFP32("bwd_ang", "angle, fp32")
		e.ReadFP32("lft_ang", "angle, fp32")
		e.ReadFP32("rgt_ang", "angle, fp32")
		e.ReadBool("is_dir_cmp_mode")
	},
	"triggers/game/weapon_hit": ReadTrigger,
	"triggers/game/weapon_state": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("weapon_state")
	},
	"triggers/game_difficulty": function(e)
	{
		e.ReadU32("difficulty_id")
	},
	"triggers/gameloaded": ReadTrigger,
	"triggers/generic_special": ReadTriggerZoneSpecial,
	"triggers/grenade_is_close": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadFP32("distance")
	},
	"triggers/hand_orientation": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("hand")
		e.ReadFP32("fwd_ang", "angle, fp32")
		e.ReadFP32("bwd_ang", "angle, fp32")
		e.ReadFP32("lft_ang", "angle, fp32")
		e.ReadFP32("rgt_ang", "angle, fp32")		
	},
	"triggers/has_target": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("tid")
		e.ReadFP32("distance")
		e.ReadFP32("threshold")
		e.ReadFP32("aimap_threshold")
		e.ReadU32("time_threshold")
		e.ReadU8("_friend")
		e.ReadU32Array("npc_type", "identifier_array")
		e.ReadU8("flags8", "bool8") // ai_map_check, busy_check, 
		e.ReadString("ex_prop")
	},
	"triggers/helpers/counter trigger": ReadTrigger,
	"triggers/hit": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadHint("hit_types", "flags32") // NEW in Arktika.1 !!!
		e.ReadU32("hit_types")
		e.ReadBool("any_hit_type")
		e.ReadU32("weapon_type")
		e.ReadHintStr("bone", "attp_str")
		e.ReadFP32("range_min")
		e.ReadFP32("range_max")
		e.ReadBool("fly_by")
	},
	"triggers/input": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("action")
		e.ReadU8("flags0", "bool8") // allow_input, network, check_on_activate, signal_on_activate
	},
	"triggers/is fire": function(e)
	{
		var weapons = [
			"revolver", //"revolver silencer", 
			"uboynicheg", "duplet", 
			"tihar", "helsing",
			"ubludok", //"ubludok silencer",
			"AK 74", //"AK 74 silencer",
			"2012", //"2012 silencer",
			"VSV", 
			"macheta", "dagger",
			"medved", 
			"flamethrower",
			"ventil",
			"ashot", //"ashot silencer",
			"padonag", //"padonag silencer",
			"gatling",
			"saiga",
			"tumak",
			"abzaz_slot",
			"aksu_slot",
			//"aksu_silencer_slot",
			"rpk_slot",
			//"rpk_silencer_slot"
		]
			
		ReadTrigger(e)
		for(var i = 0; i < weapons.length; i++)
		{
			var r = e.ReadSection(weapons[i])
			r.ReadBool("primary_"+weapons[i])
			r.ReadBool("secondary_"+weapons[i])
		}
		
		//e.ReadBool("enabled_dynamite")  // REMOVED in Arktika.1 !!!
		//e.ReadBool("enabled_launcher")  // REMOVED in Arktika.1 !!!
		//e.ReadBool("enabled_shotgun")   // REMOVED in Arktika.1 !!!
		
		// NEW in Arktika.1 !!!
		e.ReadU8("flags", "bool8") // dynamite, sticky_dynamite, flame_grenade, claymore, knife, launcher, shotgun
	},
	"triggers/magnetic_holster": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("slot")
	},
	"triggers/mech_vr_pull": ReadTrigger,
	"triggers/menu button": function(e)
	{
		ReadTrigger(e)
		e.ReadHintStr("menu_name", "choose")
		e.ReadHintStr("button_name", "choose")
	},
	"triggers/menu_option_changed": function(e)
	{
		ReadTrigger(e)
		e.ReadHintStr("menu_name", "choose")
		e.ReadHintStr("control_name", "choose")
	},
	"triggers/mic threshold": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("threshold")
	},
	"triggers/modular_customize": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("attach_type")
	},
	"triggers/net/check_prop": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("prop_id")
		e.ReadS32("prop_delta")
	},
	"triggers/npc": function(e)
	{
		ReadTriggerZone(e)
		e.ReadU32Array("npc_type", "identifier_array")
		e.ReadBool("_check_on_activate")
	},
	"triggers/npc enemy": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadU32Array("npc_type", "identifier_array") // NEW in Arktika.1 !!!
	},
	"triggers/npc enemy is close": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadFP32("distance")
		e.ReadFP32("distance_far")
		e.ReadU32("npc_type")
	},
	"triggers/npc mental state": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadU8("min_mental_state")
		e.ReadU8("max_mental_state")
	},
	"triggers/npc_die_from": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("weapon_type")
		e.ReadU32("npc_type")
		e.ReadU8("friend")
		e.ReadU8("mp_class_type")
		e.ReadFP32("threshold_min")
		e.ReadFP32("threshold_max")
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
		e.ReadBool("only_targets")
	},
	"triggers/pass cover": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("once")
		e.ReadU8("cover_type")
	},
	"triggers/phys_bottom_reached": ReadTrigger,
	"triggers/player": function(e)
	{
		ReadTriggerZone(e)
		e.ReadBool("check_on_activate")
	},
	"triggers/player_vr_gesture": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("hand")
		e.ReadU8("dir")
		e.ReadFP32("gest_power")
		e.ReadFP32("gest_curvature")
	},
	"triggers/startgame": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("flags", "bool8")
	},
	"triggers/stick": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("right")
		e.ReadU16("sectors_count")
		e.ReadFP32("threshold")
	},
	"triggers/store_selection": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("flags", "bool8") // _ignore_upgrades, _selected_upgrade
		e.ReadU8("_selected_upgrade_idx")
	},
	"triggers/turret": ReadTrigger,
	"triggers/use": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadFP32("usage_distance")
		e.ReadHintStr("use_action", "choose")
		e.ReadVec2("use_offset")
		e.ReadFP32("blink_distance")
		e.ReadU8("flags8", "bool8") // blink, check_need_end, hold_user
		e.ReadU8("user_team")
		e.ReadBool("in_reloading")
		e.ReadU8Array("mp_classes")
		e.ReadBool("trigger_only") // NEW in Arktika.1 !!!
		e.ReadU32("hold_count") // NEW in Arktika.1 !!!
	},
	"triggers/velocity": function(e)
	{
		ReadTrigger(e)
		e.ReadU16("entity", "entity_link, uobject_link")
		e.ReadFP32("velocity")
	},
	"triggers/wall_collision": function(e)
	{
		ReadTrigger(e)
		e.ReadU8("device_type")
	},
	"triggers/zombie": ReadTrigger_ee
}