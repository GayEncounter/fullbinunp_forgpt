
// implementation
function ReadVssVer6(e)
{
	var v = e.TryReadArray("vss_ver_6") || e.ReadArray("vss_ver_7")
	
	for(var i = 0; v.More(); i++)
	{
		var rec = v.ReadSection(RecStr("rec_", i, 4), false)
		ReadVS(rec)
	}
}

var already_printed = new Object // not implemented classes

function ReadVS(e)
{

	var groups = e.ReadArray("groups") // it's even used ???
	
	var blocks = e.ReadSection("blocks")
	
	blocks.ReadU32("block_count")
	blocks.ReadHint("array with no key", "array")
	blocks.ReadU32("count")
		
	for(var i = 0; blocks.More(); i++)
	{
		var block = blocks.ReadSection(RecStr("block_", i, 4), false)
		
		var clsid = block.ReadString("clsid")
		block.ReadU16("posx")
		block.ReadU16("posy")
		
		// print(clsid)
		
		var r = block_readers[clsid]
	
		if(r)
		{
			r(block)
		}
		else
			if(r !== null && !already_printed[clsid])
			{
				print("not implemented block class ", clsid)
				already_printed[clsid] = true
			}

		if(block.More())
			print("block " + clsid + " data left")
	}
	
	var link_count = e.ReadU32("link_count")
	
	for(var j = 0; j < link_count; j++)
		e.ReadVec4S16(j);
}

function ReadActionPlayMotion(e)
{
	e.ReadHintStr("dbg_model", "ref_model")
	e.ReadString("dbg_skel")
	e.ReadFP32("initial_offset")
	e.ReadHintStr("animation", "animation_str")
	e.ReadHintStr("bone_part", "part_id")
	e.ReadU8("force_looped")
	e.ReadBool("overlay")
}

function ReadTrigger(e)
{
	e.ReadBool("active")
}

function ReadTrigger_ee(e)
{
	ReadTrigger(e)
	
	e.ReadU16("entity", "entity_link, uobject_link")
	e.ReadU16("initiator", "entity_link, uobject_link")
	e.ReadBool("player")
}

function ReadTriggerZone(e)
{
	ReadTrigger(e)
	e.ReadS32("reaction_count")
	e.ReadU16("zone_link", "entity_link, uobject_link")
}

function ReadTrolleyDriverMode(e)
{
	e.ReadVec2("limit_yaw")
	e.ReadVec2("limit_pitch")
	e.ReadFP32("limit_factor")
	e.ReadBool("deactivate_on_use")
	e.ReadU16("npc", "entity_link, uobject_link")
	e.ReadFP32("fwd_accel")
	e.ReadFP32("bwd_accel")
	e.ReadFP32("brakes_accel")
	e.ReadFP32("slope_accel")
	e.ReadFP32("friction_accel")
	e.ReadFP32("speed_min")
	e.ReadFP32("speed_max")
	e.ReadFP32("pump_power")
	e.ReadFP32("pump_fade")
	e.ReadFP32("pump_npc_speed")
	e.ReadFP32("pump_npc_accel")
	e.ReadFP32("tuk_tuk_len")
	e.ReadFP32("tuk_tuk_my_len")
}

var block_readers = {
	"actions/action_check_slot": function(e) // in original game only exists in commons_vs
	{
		e.ReadU8("slot")
	},
	"actions/action_data_add": function(e)
	{
		e.ReadString("private_data")
	},
	"actions/action_data_check": function(e)
	{
		e.ReadString("private_data")
	},
	"actions/action_data_remove": function(e)
	{
		e.ReadString("private_data")
	},
	"actions/action_dialog": function(e)
	{
		e.ReadString("dialog_id")
		e.ReadBool("esc_use")
		e.ReadBool("esc_use_activate")
		e.ReadString("confirm_string")
		e.ReadString("cancel_string")
		e.ReadBool("show_gold")
	},
	"actions/action_move2camera": null,
	"actions/action_pd_add": function(e)
	{
		e.ReadS32("points")
		e.ReadString("points_comments")
	},
	"actions/action_pd_check": function(e)
	{
		e.ReadS32("points")
	},
	"actions/action_set_interest": function(e)
	{
		e.ReadU8("einterest_mode")
		e.ReadU8("aim_mode")
		e.ReadU16("interest_object", "entity_link, uobject_link")
		e.ReadU32("interest_duration")
		e.ReadFP32("speed_coef")
		e.ReadFP32("max_angle")
		e.ReadFP32("look_probability")
	},
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
		e.ReadBool("cam_limit")
		e.ReadVec2("cam_limit_pitch")
		e.ReadFP32("mouse_inertion")
		e.ReadBool("invert_mouse")
		e.ReadBool("ignore_cam_rot")
	},
	"actions/ai/cover escape": function(e)
	{
		e.ReadFP32("escape_min_dist")
		e.ReadFP32("escape_max_dist")
		e.ReadU32("escape_cover_type")
		e.ReadU32("escape_cover_wait_min")
		e.ReadU32("escape_cover_wait_max")
		e.ReadFP32("return_min_dist")
		e.ReadFP32("return_max_dist")
		e.ReadU32("return_cover_type")
		e.ReadU32("return_lock_min")
		e.ReadU32("return_lock_max")
		e.ReadU32("angry_on_return_min")
		e.ReadU32("angry_on_return_max")
		e.ReadU8("movement_type")
		e.ReadU8("arrival_type")
	},
	"actions/ai/follow": function(e)
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
	"actions/ai/lead": function(e)
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
		e.ReadFP32("anim_state_approach_speed")
	},
	"actions/ai/stop_melee_attack": null,
	"actions/alert search enable": null,
	"actions/attach": function(e)
	{
		e.ReadU16("owner", "entity_link, uobject_link")
		e.ReadHintStr("bone", "locator_str")
		e.ReadMatrix44("offset", "pose, matrix")
		e.ReadBool("auto_offset")
	},
	"actions/attraction": function(e)
	{
		e.ReadFP32("min_distance")
		e.ReadFP32("max_distance")
		e.ReadFP32("min_power")
		e.ReadFP32("max_power")
		e.ReadFP32("near_power")
		e.ReadFP32("far_power")
		e.ReadFP32("start_time")
		e.ReadFP32("finish_time")
		e.ReadFP32("start_modif")
		e.ReadFP32("finish_modif")
		e.ReadFP32("early_modif")
		e.ReadFP32("later_modif")
		e.ReadString("satellite_bone")
	},
	"actions/camera attach": function(e)
	{
		e.ReadU16("owner", "entity_link, uobject_link")
		e.ReadHintStr("bone", "locator_str")
		e.ReadMatrix44("offset", "pose, matrix")
		e.ReadBool("auto_offset")
		
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
		e.ReadBool("use_rotation")
		e.ReadBool("keep_rot")
		e.ReadFP32("use_rotation_speed")
		e.ReadBool("precise_finish")
		e.ReadBool("exclusive")
	},
	"actions/change ground holder": function(e)
	{
		e.ReadString("ground_holder")
	},
	"actions/collision_group": function(e)
	{
		e.ReadU8("collisions_group")
	},
	"actions/console command": function(e)
	{
		e.ReadString("cmd")
	},
	"actions/cover_combat": function(e)
	{
		e.ReadBool("make_cover_combat")
	},
	"actions/destroy": null,
	"actions/detach": function(e)
	{
		e.ReadU16("owner", "entity_link, uobject_link")
	},
	"actions/detach_all": function(e)
	{
		e.ReadBool("recursively")
	},
	"actions/engine/3d_text": function(e)
	{
		e.ReadVec3("offset")
		e.ReadFP32("rotation")
		e.ReadString("key")
		e.ReadFP32("size")
		e.ReadU8("h_alignment")
		e.ReadU8("v_alignment")
		e.ReadVec4("color", "color, vec4f")
		e.ReadHintStr("bone", "bone_str")
	},
	"actions/engine/achievement": function(e)
	{
		e.ReadString("name")
		e.ReadS32("border")
	},
	"actions/engine/clone": function(e)
	{
		e.ReadU16("source", "entity_link, uobject_link")
		e.ReadU16("position", "entity_link, uobject_link")
	},
	"actions/engine/disable_save": null,
	"actions/engine/discharge": function(e)
	{
		e.ReadHintStr("model", "ref_model")
		e.ReadHintStr("coloranim", "choose")
		e.ReadFP32("var_life_time_from")
		e.ReadFP32("var_life_time_to")
		e.ReadFP32("var_scale_from")
		e.ReadFP32("var_scale_to")
	},
	"actions/engine/dlc": function(e)
	{
		e.ReadU32("offer_idx")
	},
	"actions/engine/end_cut_scene": null,
	"actions/engine/lightning": function(e)
	{
		e.ReadHintStr("model", "ref_model")
		e.ReadHintStr("coloranim", "choose")
		e.ReadFP32("var_life_time_from")
		e.ReadFP32("var_life_time_to")
		e.ReadFP32("var_altitude_from", "angle, fp32")
		e.ReadFP32("var_altitude_to", "angle, fp32")
		e.ReadFP32("var_longitude_from", "angle, fp32")
		e.ReadFP32("var_longitude_to", "angle, fp32")
		e.ReadFP32("var_scale_from")
		e.ReadFP32("var_scale_to")
		e.ReadFP32("ground_height")
		e.ReadFP32("tilt", "angle, fp32")
		e.ReadBool("adjust_size")
	},
	"actions/engine/play video": function(e)
	{
		e.ReadHintStr("video_name", "choose")
	},
	"actions/engine/portal throughput": function(e)
	{
		e.ReadHintStr("portal_name", "choose")
		e.ReadFP32("throughput")
	},
	"actions/engine/presence": function(e)
	{
		e.ReadU16("context_location_id")
		e.ReadU16("context_action_id")
	},
	"actions/engine/property": function(e) // in original game only exists in commons_vs
	{
		e.ReadString("name")
		e.ReadS32("value")
	},
	"actions/engine/quick_save": function(e)
	{
		e.ReadString("filename")
	},
	"actions/engine/set background music": function(e)
	{
		e.ReadHintStr("back_music", "choose")
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
		e.ReadFP32("volume")
	},
	"actions/engine/set env cubemap": function(e)
	{
		e.ReadHintStr("cubemap", "choose")
	},
	"actions/engine/set env layer": function(e)
	{
		e.ReadString("zone")
		e.ReadString("layer")
	},
	"actions/engine/set environment": function(e)
	{
		e.ReadHintStr("env_name", "choose")
		e.ReadFP32("blend_time")
	},
	"actions/engine/signal": function(e)
	{
		e.ReadBool("localus")
		e.ReadString("signal")
	},
	"actions/engine/slowmo": function(e)
	{
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
		e.ReadFP32("value")
		e.ReadFP32("current_value")
		e.ReadBool("active")
		e.ReadBool("registered")
	},
	"actions/engine/start_cut_scene": null,
	"actions/engine/volume": function(e)
	{
		e.ReadFP32("volume")
		e.ReadU32("time")
	},
	"actions/entity/add gesture": function(e)
	{
		e.ReadHintStr("dbg_model", "ref_model")
		e.ReadString("dbg_skel")
		e.ReadHintStr("seq_name", "animation_str")
	},
	"actions/entity/move_restore": function(e)
	{
		e.ReadBool("keep_move")
		e.ReadMatrix44("savedpos")
		e.ReadU8("saved")
		e.ReadMatrix44("movepos")
	},
	"actions/entity/useful for player": function(e)
	{
		e.ReadBool("flag")
	},
	"actions/explode": function(e)
	{
		e.ReadFP32("amount")
		e.ReadU8("hit_type")
	},
	"actions/ext/play_particles_ex": function(e)
	{
		e.ReadHintStr("particles", "particles, str_shared")
		e.ReadHintStr("locator", "locator_str")
		e.ReadMatrix44("offset", "pose, matrix")
	},
	"actions/find_cover": function(e)
	{
		e.ReadFP32("target_range")
		e.ReadU8("movement_type")
		e.ReadU8("arrival_type")
		e.ReadBool("do_not_unlock_cover")
		e.ReadFP32("min_dist")
		e.ReadFP32("max_dist")
		e.ReadU32("cover_type")
	},
	"actions/force actor state": function(e)
	{
		e.ReadBool("crouch")
	},
	"actions/freegun": null,
	"actions/goto_target": function(e)
	{
		e.ReadFP32("target_range")
		e.ReadU8("movement_type")
		e.ReadU8("arrival_type")
		e.ReadBool("do_not_unlock_cover")
	},
	"actions/hint": function(e)
	{
		e.ReadString("hint")
		e.ReadBool("show_background")
		e.ReadBool("undisabled")
		e.ReadBool("auto_size")
		e.ReadFP32("x")
		e.ReadFP32("y")
		e.ReadFP32("width")
		e.ReadFP32("height")
	},
	"actions/hit": function(e)
	{
		e.ReadU16("initiator", "entity_link, uobject_link")
		e.ReadFP32("amount")
		e.ReadHintStr("bone", "bone_str")
		e.ReadU8("hit_type")
	},
	"actions/link": function(e)
	{
		e.ReadHintStr("destination_bone", "bone_str")
		e.ReadHintStr("source_bone", "bone_str")
	},
	"actions/luminocity": null,
	"actions/make friend": function(e)
	{
		e.ReadU8("make_friend_type")
	},
	"actions/make immortal": null,
	"actions/make scripted": function(e)
	{
		e.ReadBool("make_scripted")
	},
	"actions/map_text": function(e)
	{
		e.ReadString("key")
		e.ReadFP32("size")
		e.ReadFP32("rotation")
		e.ReadVec4("rect")
		e.ReadU8("line_spacing")
		e.ReadVec4("color", "color, vec4f")
	},
	"actions/milestone": function(e)
	{
		e.ReadString("milestone")
	},
	"actions/min mental state": function(e)
	{
		e.ReadU8("mental_state")
		e.ReadU8("max_mental_state")
	},
	"actions/monster/set_librarian_aggro": function(e)
	{
		e.ReadFP32("aggro_time")
		e.ReadBool("force")
		e.ReadBool("do_not_increase")
		e.ReadBool("do_not_decrease")
		e.ReadBool("only_add_aggro")
	},
	"actions/monster/set_librarian_scripted_ex": null,
	"actions/monster/set_lurkers_state": function(e)
	{
		e.ReadU8("state")
	},
	"actions/move": function(e)
	{
		e.ReadU16("dest", "entity_link, uobject_link")
		e.ReadBool("keep_move")
	},
	"actions/movement type": function(e)
	{
		e.ReadU8("min_movement_type")
		e.ReadU8("max_movement_type")
	},
	"actions/no_combat": function(e)
	{
		e.ReadBool("make_no_combat")
	},
	"actions/npc can play motion": null,
	"actions/npc/action feelings": function(e)
	{
		e.ReadBool("vision")
	},
	"actions/npc/biomass_attack": null,
	"actions/npc/cover group": function(e)
	{
		e.ReadHintStr("cover_group", "choose")
		e.ReadU8("allow")
		e.ReadU8("forbid")
	},
	"actions/npc/dark_make_angry": null,
	"actions/npc/dispersion": function(e)
	{
		e.ReadFP32("amount")
	},
	"actions/npc/group": function(e)
	{
		e.ReadString("group_id")
	},
	"actions/npc/obstacle": function(e)
	{
		e.ReadHintStr("obstacle", "choose")
		e.ReadBool("add_obstacle")
	},
	"actions/one shot": function(e)
	{
		e.ReadBool("ai_sound")
	},
	"actions/open fire": function(e)
	{
		e.ReadU16("target", "entity_link, uobject_link")
		e.ReadU32("min_queue_size")
		e.ReadU32("max_queue_size")
		e.ReadU32("min_queue_interval")
		e.ReadU32("max_queue_interval")
		e.ReadBool("instant")
	},
	"actions/p-force": function(e)
	{
		e.ReadFP32("min_amount")
		e.ReadFP32("max_amount")
		e.ReadFP32("max_influence_dst")
		e.ReadBool("instant")
		e.ReadString("bone")
		e.ReadVec3("dir")
		e.ReadBool("in_local_coords")
		e.ReadBool("single_force")
	},
	"actions/play animation": ReadActionPlayMotion,
	"actions/play camera-effect": function(e)
	{
		e.ReadHintStr("cameraeffect", "camera_track, str_shared")
		e.ReadFP32("value")
		e.ReadFP32("inc_delta")
		e.ReadFP32("inc_speed")
		e.ReadFP32("dec_delta")
		e.ReadFP32("dec_speed")
		e.ReadFP32("max_dist")
		e.ReadBool("directional")
		e.ReadBool("check_visible")
		e.ReadU16("max_value_point", "entity_link, uobject_link")
		e.ReadBool("single_instance")
	},
	"actions/play coloranim": function(e)
	{
		e.ReadHintStr("coloranim", "ref_coloranim")
	},
	"actions/play cover path": function(e)
	{
		e.ReadBool("fast_switch")
	},
	"actions/play modifier": function(e)
	{
		e.ReadHintStr("modifier", "particles_modifier, str_shared")
		e.ReadU16("source", "entity_link, uobject_link")
	},
	"actions/play motion": ReadActionPlayMotion,
	"actions/play motion control": function(e)
	{
		e.ReadHintStr("dbg_model", "ref_model")
		e.ReadString("dbg_skel")
		e.ReadHintStr("anim_fwd", "animation_str")
		e.ReadHintStr("anim_bwd", "animation_str")
		e.ReadHintStr("bone_part", "part_id")
		e.ReadFP32("accel_fwd")
		e.ReadFP32("accel_bwd")
		e.ReadFP32("accel_pause")
	},
	"actions/play motion ex": function(e)
	{
		ReadActionPlayMotion(e)
		
		e.ReadHintStr("dbg_object_model", "ref_model")
		e.ReadHintStr("object_attp_id", "locator_str")
		e.ReadBool("move_to_mode")
		e.ReadBool("precise_dest")
		e.ReadBool("ignore_dest_rotation")
		e.ReadBool("check_movement")
		e.ReadBool("check_movement_move")
		e.ReadBool("p2p_mode")
		e.ReadFP32("p2p_offset")
	},
	"actions/play particles": function(e)
	{
		e.ReadHintStr("particles", "particles, str_shared")
		e.ReadBool("start_as_free")
	},
	"actions/play path": function(e)
	{
		e.ReadU16("start_point", "entity_link, uobject_link")
		e.ReadBool("start_from_nearest")
	},
	"actions/play patrol path": function(e)
	{
		e.ReadU16("start_point", "entity_link, uobject_link")
		e.ReadBool("start_from_nearest")
		e.ReadString("body_state")
		e.ReadString("anim_state")
		e.ReadString("movement_type")
		e.ReadString("weapon_state")
		e.ReadString("action")
		e.ReadU16("target", "entity_link, uobject_link")
		e.ReadHint("flags", "flags32")
		e.ReadU32("flags")
		e.ReadFP32("anim_state_approach_speed")
		e.ReadBool("state_from_first")
		e.ReadBool("match_exact_goal")
	},
	"actions/play sound": function(e)
	{
		e.ReadHintStr("sound", "sound")
		e.ReadFP32("volume")
		e.ReadBool("instant")
		e.ReadU32("ai_sound_type")
		e.ReadFP32("stop_interval")
		e.ReadFP32("startus_intervalus")
		e.ReadBool("looped")
		e.ReadBool("enable_slowmo")
		e.ReadBool("enable_fx")
		e.ReadBool("play_as_music")
	},
	"actions/play vibration": function(e)
	{
		e.ReadFP32("force_feedback_0")
		e.ReadFP32("force_feedback_1")
		e.ReadFP32("step_ff0")
		e.ReadFP32("step_ff1")
	},
	"actions/player/civil mode": function(e)
	{
		e.ReadBool("instant")
		e.ReadBool("allow_inventory_ui")
		
		var slots = e.ReadSection("slots");
		
		[
			"knife_slot",
			"pistol_slot",
			"smg_slot",
			"rifle_slot",
			"grenade_slot",
			"gasmask_slot",
			"nightvision_slot",
			"map_slot",
			"medkit_slot",
			"filter_slot",
			"grenade_sticky",
			"grenade_flash",
			"macheta",
			"charger"
		].forEach(function(each) { slots.ReadBool(each); })
	},
	"actions/player/doctor": function(e)
	{
		e.ReadU32("boost_time")
	},
	"actions/player/effect switcher": null,
	"actions/player/friend_mode": null,
	"actions/player/hide hands": null,
	"actions/player/hide subj": function(e)
	{
		e.ReadBool("hands")
		e.ReadBool("knife")
	},
	"actions/player/lock camera": function(e)
	{
		e.ReadVec2("limit_yaw")
		e.ReadVec2("limit_pitch")
		e.ReadFP32("inertion")
		e.ReadHintStr("model", "ref_model")
		e.ReadHintStr("bone", "locator_str")
		e.ReadBool("ignore_rotation")
		e.ReadBool("centering_epsilon_accuracy")
	},
	"actions/player/random controls": null,
	"actions/player/restrictor_obstacle": null,
	"actions/player/show hands": null,
	"actions/player/show subj": function(e)
	{
		e.ReadBool("hands")
		e.ReadBool("knife")
	},
	"actions/player/unlimited ammo": null,
	"actions/player/way_point": null,
	"actions/savepos": function(e)
	{
		e.ReadBool("keep_move")
		e.ReadMatrix44("savedpos", "pose, matrix")
		e.ReadBool("saved")
	},
	"actions/set enemy": function(e)
	{
		e.ReadU16("enemy", "entity_link, uobject_link")
		e.ReadU16("point", "entity_link, uobject_link")
		e.ReadBool("threat")
		e.ReadBool("lock")
	},
	"actions/set physics props": function(e)
	{
		e.ReadU8("kinematic")
		e.ReadU8("breakable")
	},
	"actions/set_filters_spend": function(e)
	{
		e.ReadBool("indoor_mode")
	},
	"actions/set_speech_groups": function(e)
	{
		e.ReadString("allowed_speech_groups")
	},
	"actions/set_turn_options": function(e)
	{
		e.ReadBool("can_turn_body")
		e.ReadFP32("max_heading", "angle, fp32")
		e.ReadFP32("max_pitch", "angle, fp32")
	},
	"actions/static_combat": function(e)
	{
		e.ReadBool("make_static_combat")
		e.ReadBool("force_active_wo_enemy")
		e.ReadHintStr("model", "ref_model")
		e.ReadString("dbg_skel")
		e.ReadHintStr("static_idle", "animation_str")
		e.ReadHintStr("static_attack", "animation_str")
		e.ReadHintStr("static_reload_idle", "animation_str")
	},
	"actions/take_cover": null,
	"actions/turn ladder": function(e)
	{
		e.ReadBool("enabled")
	},
	"actions/turn light": function(e)
	{
		e.ReadU8("onoff")
		e.ReadBool("on_or_off")
	},
	"actions/voice": function(e)
	{
		e.ReadU32("voice_id")
	},
	"actions/wear gasmask": function(e)
	{
		e.ReadBool("silent_mode")
	},
	"actions/wear nightvision": function(e)
	{
		e.ReadBool("instant")
	},
	"actions/wear_suit": function(e)
	{
		e.ReadU32("suit_type")
	},
	"checker/platform": null,
	"covers/best_cover_ref": null,
	"covers/ref": function(e)
	{
		e.ReadU16("target", "cover_link, ucover_link")
	},
	"entities/entity labeled" : function(e) // in original game only exists in commons_vs
	{
		e.ReadString("label")
	},
	"entities/entity parent": null,
	"entities/entity player": null,
	"entities/entity player's hands": null,
	"entities/entity player's knife": null,
	"entities/entity player's torch": null,
	"entities/entity ref": function(e)
	{	
		e.ReadU16("target", "entity_link, uobject_link")
	},
	"entities/entity self": null,
	"entities/npc_enemy": null,
	"entities/player_enemy": null,
	"fun/set boss": null,
	"fun/text splash": function(e)
	{
		e.ReadString("text")
		e.ReadVec4("color", "color, vec4f")
		e.ReadFP32("max_size")
		e.ReadFP32("speed")
		e.ReadBool("stop")
		e.ReadBool("interrupt")
	},
	"logic/adv/randomex-2": function(e)
	{
		e.ReadBool("on_0")
		e.ReadBool("on_1")
	},
	"logic/adv/randomex-3": function(e)
	{
		e.ReadBool("on_0")
		e.ReadBool("on_1")
		e.ReadBool("on_2")
	},
	"logic/adv/randomex-4": function(e)
	{
		e.ReadBool("on_0")
		e.ReadBool("on_1")
		e.ReadBool("on_2")
		e.ReadBool("on_3")
	},
	"logic/adv/randomex-5": function(e)
	{
		e.ReadBool("on_0")
		e.ReadBool("on_1")
		e.ReadBool("on_2")
		e.ReadBool("on_3")
		e.ReadBool("on_4")
	},
	"logic/ai/cover/near player": function(e)
	{
		e.ReadFP32("dist_coef")
		e.ReadFP32("dir_coef")
		e.ReadFP32("back_coef")
		e.ReadFP32("fwd_coef")
		e.ReadBool("check_enemy_accessibility")
	},
	"logic/ai/cover/nearest": function(e)
	{
		e.ReadFP32("dist_coef")
		e.ReadFP32("dir_coef")
		e.ReadFP32("direct_path_coef")
	},
	"logic/and-2": function(e)
	{
		e.ReadBool("b1")
		e.ReadBool("b2")
	},
	"logic/and-3": function(e)
	{
		e.ReadBool("b1")
		e.ReadBool("b2")
		e.ReadBool("b3")
	},
	"logic/counter": function(e)
	{
		e.ReadS32("value")
		e.ReadS32("current_value")
	},
	"logic/counter ref": function(e)
	{
		e.ReadS32("value")
		e.ReadS32("current_value")
		e.ReadS32("reference")
	},
	"logic/delay": function(e)
	{
		e.ReadU32("min")
		e.ReadU32("max")
		e.ReadBool("one_at_a_time")
		e.ReadBool("active")
	},
	"logic/locker": function(e)
	{
		e.ReadBool("locked")
		e.ReadBool("auto_lock")
		e.ReadBool("next_frame")
	},
	"logic/or-2": function(e)
	{
		e.ReadBool("b1")
		e.ReadBool("b2")
	},
	"logic/or-3": function(e)
	{
		e.ReadBool("b1")
		e.ReadBool("b2")
		e.ReadBool("b3")
	},
	"logic/random-2": null,
	"logic/random-3": null,
	"logic/random-4": null,
	"logic/random-5": null,
	"logic/random-chooser-2": function(e)
	{
		e.ReadFP32("prob0")
		e.ReadFP32("prob1")
	},
	"logic/random-chooser-3": function(e)
	{
		e.ReadFP32("prob0")
		e.ReadFP32("prob1")
		e.ReadFP32("prob2")
	},
	"logic/random-chooser-4": function(e)
	{
		e.ReadFP32("prob0")
		e.ReadFP32("prob1")
		e.ReadFP32("prob2")
		e.ReadFP32("prob3")
	},
	"logic/random-chooser-5": function(e)
	{
		e.ReadFP32("prob0")
		e.ReadFP32("prob1")
		e.ReadFP32("prob2")
		e.ReadFP32("prob3")
		e.ReadFP32("prob4")
	},
	"trade/get player gold": function(e)
	{
		e.ReadU32("cost")
		e.ReadBool("get_all_gold")
	},
	"trade/purge slot": function(e)
	{
		e.ReadU8("slot")
		e.ReadU8("counter")
		e.ReadBool("unload_magazine")
	},
	"trade/take item": function(e)
	{
		e.ReadBool("force_activation")
	},
	"trade/test my weapon": null,
	"trade/trade trigger": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("current_object")
		
		var count = e.ReadU32("objects_count_pre")
		for(var i = 1; i <= count; i++)
		{
			e.ReadString("object_" + i + "_descr")
			e.ReadString("object_" + i + "_name")
			e.ReadU16("object_" + i + "_link", "entity_link, uobject_link")
			e.ReadBool("object_" + i + "_check_dlc")
			e.ReadU32("object_" + i + "_offer_dlc")
		}
		
		e.ReadU32("objects_count")
	},
	"trigger/vision": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadFP32("vision_value")
	},
	"triggers/aftercloned": ReadTrigger,
	"triggers/ameba zombie": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadBool("suicide_signal")
	},
	"triggers/ameba_die": ReadTrigger_ee,
	"triggers/ammo absent": function(e) // in original game only exists in commons_vs
	{
		ReadTrigger(e)
		e.ReadU32("min_ammo_count")
	},
	"triggers/anim event": function(e)
	{
		ReadTrigger(e)
		e.ReadU16("entity", "entity_link, uobject_link")
		e.ReadString("event")
	},
	"triggers/auto interest": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadU16("object_of_interest", "entity_link, uobject_link")
		e.ReadS32("int_reaction_count")
	},
	"triggers/camera-track event": function(e)
	{
		ReadTrigger(e)
		e.ReadString("event")
	},
	"triggers/charger": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("min")
		e.ReadFP32("max")
	},
	"triggers/die": ReadTrigger_ee,
	"triggers/engine/signal": function(e)
	{
		ReadTrigger(e)
		e.ReadString("signal")
	},
	"triggers/entity/health threshold": function(e)
	{
		ReadTrigger(e)
		e.ReadFP32("threshold")
		e.ReadFP32("threshold_step")
	},
	"triggers/entity/move": function(e)
	{
		e.ReadFP32("range")
	},
	"triggers/entity/orientation": function(e)
	{
		e.ReadFP32("fwd_ang", "angle, fp32")
		e.ReadFP32("bwd_ang", "angle, fp32")
		e.ReadFP32("lft_ang", "angle, fp32")
		e.ReadFP32("rgt_ang", "angle, fp32")
	},
	"triggers/game/main_menu": function(e)
	{
		ReadTrigger(e)
		
		e.ReadU16("new_game_entity", "entity_link, uobject_link")
		e.ReadU16("continue_game_entity", "entity_link, uobject_link")
		e.ReadU16("load_game_entity", "entity_link, uobject_link")
		e.ReadU16("options_entity", "entity_link, uobject_link")
		e.ReadU16("credits_entity", "entity_link, uobject_link")
		e.ReadU16("exit_entity", "entity_link, uobject_link")
	},
	"triggers/game_difficulty": function(e)
	{
		e.ReadU32("difficulty_id")
	},
	"triggers/gasmask": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("trigger_on_wear")
	},
	"triggers/generic": ReadTriggerZone,
	"triggers/generic-no-player": ReadTriggerZone,
	"triggers/hit": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadU8("hit_type")
		e.ReadBool("any_hittype")
		e.ReadFP32("range_min")
		e.ReadFP32("range_max")
	},
	"triggers/input": function(e)
	{
		ReadTrigger(e)
		e.ReadU32("action")
		e.ReadBool("allow_input")
	},
	"triggers/interest": function(e)
	{
		ReadTrigger(e)
		e.ReadU16("entity", "entity_link, uobject_link")
		e.ReadHintStr("bone", "locator_str")
		e.ReadU16("interest", "entity_link, uobject_link")		
		e.ReadFP32("distance")
		e.ReadFP32("fov", "angle, fp32")
		e.ReadU32("time")
		e.ReadBool("reload")		
	},
	"triggers/is fire": function(e)
	{
		var weapons = [
			"revolver", "revolver silencer", 
			"uboynicheg", "duplet", 
			"tihar", "helsing",
			"ubludok", "ubludok silencer",
			"AK 74", "2012",
			"VSV", 
			"macheta", "dagger"
		]
			
		ReadTrigger(e)
		
		var r = e.ReadSection("weapons")
		for(var i = 0; i < weapons.length; i++)
			r.ReadBool(weapons[i])
	},
	"triggers/low ammo": function(e) // in original game only exists in commons_vs
	{
		ReadTrigger(e)
		e.ReadBool("once")
	},
	"triggers/money": function(e) // in original game only exists in commons_vs
	{
		ReadTrigger(e)
		
		e.ReadU32("bill_money")
		e.ReadU32("spend_money")
		e.ReadU32("current_bill_money")
		e.ReadU32("current_spend_money")
		e.ReadBool("bill_signalled")
		e.ReadBool("spend_signalled")
	},
	"triggers/mouse interest": function(e)
	{
		ReadTrigger(e)
		
		e.ReadU16("entity", "entity_link, uobject_link")
		e.ReadU32("interest_time")
	},
	"triggers/nextlevel": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("trigger_on_next_level")
	},
	"triggers/nightvision_added": ReadTrigger, // in original game only exists in commons_vs
	"triggers/npc": ReadTriggerZone,
	"triggers/npc enemy": ReadTrigger_ee,
	"triggers/npc enemy is close": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadFP32("distance")
		e.ReadFP32("distance_far")
	},
	"triggers/npc mental state": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadU8("min_mental_state")
		e.ReadU8("max_mental_state")
	},
	"triggers/npc threat": ReadTrigger,
	"triggers/npc_die_from": function(e) // in original game only exists in commons_vs
	{
		ReadTrigger(e)
		e.ReadString("weapon_type")
		e.ReadString("npc_type")
	},
	"triggers/npc_in_cover": ReadTrigger,
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
	"triggers/on use": ReadTrigger_ee,
	"triggers/out of filters": ReadTrigger,
	"triggers/pass cover": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("once")
	},
	"triggers/pathfind enemy": ReadTrigger,
	"triggers/player": ReadTriggerZone,
	"triggers/player die": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("bullet_kill")
		e.ReadString("npc_type")
	},
	"triggers/player_special": function(e) // in original game only exists in commons_vs
	{
		ReadTrigger(e)
		e.ReadS32("reaction_count")
		e.ReadU8("zone_type")
	},
	"triggers/playvideo": ReadTrigger,
	"triggers/pneumo": ReadTrigger, // in original game only exists in commons_vs
	"triggers/start_script": ReadTrigger,
	"triggers/startgame": function(e)
	{
		ReadTrigger(e)
		e.ReadBool("trigger_on_game_load")
	},
	"triggers/stop_script": ReadTrigger,
	"triggers/stopvideo": ReadTrigger,
	"triggers/torch": ReadTrigger,
	"triggers/use": function(e)
	{
		ReadTrigger_ee(e)
		e.ReadFP32("usage_distance")
		e.ReadHintStr("use_action", "choose")
		e.ReadVec2("use_offset")
		e.ReadBool("blink")
		e.ReadFP32("blink_distance")
	},
	"triggers/velocity": function(e)
	{
		ReadTrigger(e)
		e.ReadU16("entity", "entity_link, uobject_link")
		e.ReadFP32("velocity")
	},
	"triggers/zombie": ReadTrigger_ee,
	"trolley/add accel": function(e)
	{
		e.ReadFP32("accel")
		e.ReadFP32("duration")
		e.ReadFP32("target_speed")
		e.ReadBool("speed_prop")
	},
	"trolley/drezina goto target": function(e)
	{
		e.ReadFP32("accel")
		e.ReadFP32("decel")
		e.ReadFP32("speed_min")
		e.ReadFP32("speed_max")
		e.ReadFP32("dist")
		e.ReadBool("smooth_approach")
	},
	"trolley/drezina params": function(e)
	{
		e.ReadFP32("fwd_accel")
		e.ReadFP32("bwd_accel")
		e.ReadFP32("brakes_accel")
		e.ReadFP32("slope_accel")
		e.ReadFP32("friction_accel")
		e.ReadFP32("speed_min")
		e.ReadFP32("speed_max")
		e.ReadFP32("pump_power")
		e.ReadFP32("pump_fade")
		e.ReadFP32("pump_npc_speed")
		e.ReadFP32("pump_npc_accel")
	},
	"trolley/driver mode hand": ReadTrolleyDriverMode,
	"trolley/driver mode moto": ReadTrolleyDriverMode,
	"trolley/kulemet mode": function(e)
	{
		e.ReadVec2("limit_yaw")
		e.ReadVec2("limit_pitch")
		e.ReadFP32("limit_factor")
		e.ReadBool("deactivate_on_use")
		e.ReadU16("npc", "entity_link, uobject_link")
	},
	"trolley/kulemet one shot": null,
	"trolley/lock on target": function(e)
	{
		e.ReadBool("open_fire")
	},
	"trolley/start drezina": function(e)
	{
		e.ReadHintStr("dbg_model", "ref_model")
		e.ReadString("dbg_skel")
		e.ReadHintStr("animation", "animation_str")
		e.ReadFP32("speed")
		e.ReadFP32("initial_offset")
		e.ReadFP32("slope_threshold")
		e.ReadFP32("slope_min_time")
		e.ReadFP32("slope_cooldown")
	},
	"trolley/station stand mode": null,
	"trolley/stop drezina hand": function(e)
	{
		e.ReadFP32("tormoznoy_put")
	}
}

// interface
if(this)
{
	// called as module, it's actually interface
	this.ReadVssVer6 = ReadVssVer6
}
else
{
	// called directly, it's entrypoint
	ReadVS(reader.ReadSection("script"))
}