
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
	
	blocks.ReadU16("version") // should we rely on this value when dealing with different spawn versions in this file instead of using entity_ver ? 
	blocks.ReadU32("block_count")
	blocks.ReadHint("array with no key", "array")
	blocks.ReadU32("count")
	
	for(var i = 0; blocks.More(); i++)
	{
		var block = blocks.ReadSection(RecStr("block_", i, 4), false)
		
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
			if(r !== null && !already_printed[clsid])
			{
				print("not implemented block class ", clsid)
				already_printed[clsid] = true
			}
	}
	
	var link_count = e.ReadU32("link_count")
	
	for(var j = 0; j < link_count; j++)
		e.ReadVec4S16(j);
}

function ReadActionEngineSignal(e)
{
	e.ReadHintStr("signal", "choose")
	// old version
	//e.ReadBool("localus")
	//e.ReadBool("relatives")
	e.ReadU8("mode")
}

function ReadActionHit(e)
{
	e.ReadU16("initiator", "entity_link, uobject_link");
	e.ReadFP32("amount")
	e.ReadHintStr("bone", "bone_str")
	e.ReadU8("hit_type")
	if(entity_ver >= ENTITY_VER_29)
		e.ReadBool("forbid_ai")
}

function ReadActionPlayParticles(e)
{
	e.ReadHintStr("particles", "particles, str_shared")
	e.ReadU8("particles_flags", "bool8")
}

function ReadActionPlayMotion(e)
{
	e.ReadHintStr("dbg_model", "ref_model")
	e.ReadString("dbg_skel")
	e.ReadHintStr("animation", "animation_str")
	e.ReadHintStr("bone_part", "part_id")
	e.ReadFP32("blend")
	e.ReadFP32("speed")
	e.ReadU8("force_looped")
	e.ReadU8("flags", "bool8") // overlay, inverted, speed_replace
}

function ReadActionPlayMotionControl(e)
{
	e.ReadHintStr("dbg_model", "ref_model")
	e.ReadString("dbg_skel")
	e.ReadHintStr("anim_fwd", "animation_str")
	e.ReadHintStr("anim_bwd", "animation_str")		
	e.ReadHintStr("bone_part", "part_id")
	e.ReadFP32("accel_fwd")
	e.ReadFP32("accel_bwd")
	e.ReadFP32("accel_pause")
	e.ReadFP32("spd_coef")
	e.ReadFP32("spd_coef_acc")
	e.ReadFP32("spd_coef_decc")
	if(entity_ver >= ENTITY_VER_29)
		e.ReadU8("flags", "bool8")
}

function ReadActionPlaySound(e)
{
	e.ReadHintStr("sound", "sound")
	e.ReadFP32("volume")
	e.ReadU8("sound_filter")
	e.ReadU8("sound_bus")
	e.ReadU8("flags0", "bool8")
	e.ReadU32("ai_sound_type")
	e.ReadFP32("stop_interval")
	e.ReadFP32("startus_intervalus")
	e.ReadBool("play_as_music")
}

function ReadActionSetInterest(e)
{
	e.ReadU8("einterest_mode")
	e.ReadU8("aim_mode")
	e.ReadU16("interest_object", "entity_link, uobject_link")
	e.ReadU32("interest_duration")
	e.ReadFP32("speed_coef")
	e.ReadFP32("max_angle")
	e.ReadFP32("clamp_angle")
	e.ReadFP32("look_probability")
	e.ReadU32("ignore_angle_timeout")
}

function ReadActionTextHud(e)
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
	e.ReadU8("flags", "bool8") // icon_draw_first, instant, text_depend, abb_coorsds, swap_nicktext, blink_mode, use_nickname (order ?)
}

function ReadLogicCounter(e)
{
	e.ReadS32("value")
	e.ReadS32("current_value")
}

function ReadTrigger(e)
{
	e.ReadBool("active")
}

var block_readers = {
	"actions/action_check_slot": function(e)
	{
		e.ReadU8("slot")
	},
	"actions/action_dialog": function(e) // build 2012-12-03
	{
		e.ReadString("dialog_id")
		e.ReadHintStr("confirm_string", "choose")
		e.ReadHintStr("cancel_string", "choose")
		e.ReadU8("flags0", "bool8") // esc_use, esc_use_activate, show_gold, dont_get_input
	},
	"actions/action_move2camera": null,
	"actions/action_set_interest": function(e)
	{
		ReadActionSetInterest(e)
	},
	"actions/action_set_interest_ex": function(e)
	{
		ReadActionSetInterest(e)
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
		e.ReadFP32("speed_factor_accel")
		e.ReadFP32("speed_factor_brake")
		e.ReadBool("cam_limit")
		e.ReadVec2("cam_limit_pitch")
		e.ReadFP32("mouse_inertion")
		e.ReadBool("invert_mouse")
		e.ReadBool("ignore_cam_rot")
		e.ReadU32("blend_time")
	},
	"actions/ai/cover combat": function(e)
	{
		e.ReadBool("make_cover_combat")
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
	"actions/ai/lock_melee_attack": null,
	"actions/ai/monster face enemy": function(e)
	{
		e.ReadU8("flags0", "bool8")
		e.ReadFP32("_face_enemy_stand_speed")
	},
	"actions/ai/static_obstacle": function(e)
	{
		e.ReadFP32("range")
	},
	"actions/ai/stop_melee_attack": null,
	"actions/alert search enable": null,
	"actions/anim_movement": null,
	"actions/attach": function(e)
	{
		e.ReadU16("owner", "entity_link, uobject_link")
		e.ReadHintStr("bone", "locator_str")
		e.ReadMatrix43("offset", "pose, matrix")
		e.ReadBool("auto_offset")
	},
	"actions/attach vs": function(e)
	{
		e.ReadString("name")
		e.ReadHintStr("vs", "vs_ref, str_shared")
	},
	"actions/body state": function(e) // build 2012-10-19
	{
		e.ReadU8("body_state")
	},
	"actions/break_ice": null,
	"actions/camera attach": function(e)
	{
		e.ReadU16("owner", "entity_link, uobject_link")
		e.ReadHintStr("bone", "locator_str")
		e.ReadMatrix43("offset", "pose, matrix")
		e.ReadBool("auto_offset")
		
		e.ReadFP32("rotation_coef")
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
		e.ReadBool("use_rotation")
		e.ReadBool("keep_rot")
		e.ReadFP32("use_rotation_speed")
		e.ReadBool("precise_finish")
		e.ReadBool("exclusive")
	},
	"actions/cap_health": function(e)
	{
		e.ReadFP32("low_health_border")
		e.ReadFP32("high_health_border")
		e.ReadBool("full_protection")
	},
	"actions/collision_group": function(e)
	{
		e.ReadU8("collisions_group")
	},
	"actions/common_combat": null,
	"actions/console command": function(e)
	{
		e.ReadString("cmd")
	},
	"actions/console echo": function(e)
	{
		e.ReadString("cmd")
	},
	"actions/cover link": function(e) // build 2012-10-19
	{
		e.ReadS32("_cost")
	},
	"actions/cover type": function(e) // build 2012-10-19
	{
		e.ReadU8("_cover_type")
	},
	"actions/cover_combat": function(e)
	{
		e.ReadBool("make_cover_combat")
	},
	"actions/cover_task_params": function(e) // build 2012-10-19
	{
		e.ReadU32("enemy_seen_timeout")
		e.ReadU32("lookout_min")
		e.ReadU32("lookout_max")
		e.ReadU32("aim_while_lookout_timeout")
		e.ReadU32("lookout_cooldown_min")
		e.ReadU32("lookout_cooldown_max")
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
	"actions/disable_all_use": null,
	"actions/dynamite/throw": null,
	"actions/engine/3d_text": function(e)
	{
		e.ReadVec3("offset")
		e.ReadFP32("rotation")
		e.ReadHintStr("key", "choose")
		e.ReadFP32("size")
		e.ReadU8("h_alignment")
		e.ReadU8("v_alignment")
		e.ReadVec4("color", "color, vec4f")
		e.ReadHintStr("bone", "bone_str")
		e.ReadHintStr("font", "choose")
		e.ReadU8("flags0", "bool8")
	},
	"actions/engine/auramode": null,
	"actions/engine/clone": function(e)
	{
		e.ReadU32("cover_reuse_delay")
		e.ReadU8("count")
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
		e.ReadS32("name")
	},
	"actions/engine/game news": null,
	"actions/engine/leaderboard": function(e)
	{
		e.ReadU32("property")
		e.ReadU32("value")
	},
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
	"actions/engine/make_ghost": null,
	"actions/engine/measure fps": null,
	"actions/engine/net-signal": function(e)
	{
		ReadActionEngineSignal(e)
	},
	"actions/engine/play video": function(e)
	{
		e.ReadHintStr("video_name", "choose")
	},
	"actions/engine/play_hud_particles": function(e)
	{
		e.ReadHintStr("particles", "particles, str_shared")
		e.ReadU16("hud_particles_type")
		e.ReadU8("particles_flags", "bool8") // particles_constrained, particles_ignore_parent_rotation, deferred_stop
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
		e.ReadHintStr("model", "ref_model")
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
		e.ReadVec4("factor", "color, vec4f")
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
	"actions/engine/shadowcast": null,
	"actions/engine/show_menu": function(e)
	{
		e.ReadBool("show_only_menu")
		e.ReadString("menu_name")
	},
	"actions/engine/steam_store": null,
	"actions/engine/stop_hud_particles": function(e)
	{
		e.ReadU8("hud_particles_type")
		e.ReadBool("deferred_stop")
	},
	"actions/engine/signal": function(e)
	{
		ReadActionEngineSignal(e)
	},
	"actions/engine/slowmo": function(e)
	{
		e.ReadFP32("accrue")
		e.ReadFP32("falloff")
		e.ReadFP32("value")
		e.ReadFP32("current_value")
		e.ReadBool("active")
		e.ReadBool("registred")
	},
	"actions/engine/text_hud": ReadActionTextHud, // build 2012-10-19
	"actions/engine/text_hud_ind": ReadActionTextHud,
	"actions/engine/texture prestream": function(e)
	{
		e.ReadHintStr("textures", "choose_array, str_shared")
	},
	"actions/engine/view distance": function(e)
	{
		e.ReadFP32("distance")
		e.ReadU32("time")
	},
	"actions/engine/volume": function(e)
	{
		e.ReadFP32("volume")
		e.ReadU32("time")
		e.ReadU8("bus")
	},
	"actions/engine/weapon_price": function(e)
	{
		e.ReadVec3("offset")
		e.ReadFP32("rotation")
		e.ReadHintStr("key", "choose")
		e.ReadFP32("size")
		e.ReadU8("h_alignment")
		e.ReadU8("v_alignment")
		e.ReadVec4("color", "color, vec4f")
		e.ReadHintStr("bone", "bone_str")
		e.ReadHintStr("font", "choose")
		e.ReadU8("flags0", "bool8")
		e.ReadU16("entity_link", "entity_link, uobject_link")
	},
	"actions/engine/wipe_hud_particles": null,
	"actions/entity/add gesture": function(e)
	{
		e.ReadHintStr("dbg_model", "ref_model")
		e.ReadString("dbg_skel")
		e.ReadHintStr("seq_name", "animation_str")
		e.ReadHintStr("bone_part", "part_id")
		e.ReadU8("force_looped")
	},
	"actions/entity/blink": function(e)
	{
		if(entity_ver >= ENTITY_VER_28b)
			e.ReadBool("blink_prevent")
	},
	"actions/entity/move_restore": function(e)
	{
		e.ReadU8("flags", "bool8")
		e.ReadVec3("offset")
		e.ReadMatrix43("movepos", "pose, matrix")
	},
	"actions/entity/override-material": function(e)
	{
		e.ReadHintStr("preset", "choose")
	},
	"actions/entity/set dao value": function(e)
	{
		e.ReadU32("dao_val")
		e.ReadU8("flags", "bool8")
	},
	"actions/entity/shadowed": null,
	"actions/entity/show bone": function(e)
	{
		e.ReadHintStr("dbg_model", "ref_model")
		e.ReadString("dbg_skel")
		e.ReadHintStr("bone_name", "bone_str")
		e.ReadBool("show")		
	},
	"actions/entity/useful for player": function(e)
	{
		e.ReadU8("flags0", "bool8");
	},
	"actions/explode": function(e)
	{
		e.ReadFP32("amount")
		e.ReadU8("hit_type")
	},
	"actions/ext/play_particles_ex": function(e)
	{
		ReadActionPlayParticles(e)
		e.ReadHintStr("locator", "locator_str")
		if(entity_ver >= ENTITY_VER_29)
		{
			e.ReadBool("force_y")
			e.ReadFP32("force_y_val")
		}
		e.ReadMatrix43("offset", "pose, matrix_43T")
	},
	"actions/find_cover": function(e)
	{
		e.ReadFP32("target_range")
		e.ReadFP32("not_going_range")
		e.ReadU8("goto_movement_type")
		e.ReadU8("arrival_type")
		e.ReadU8("flags", "bool8")
		e.ReadFP32("min_dist")
		e.ReadFP32("max_dist")
		e.ReadU32("cover_type")
	},
	"actions/force actor state": function(e)
	{
		e.ReadBool("crouch")
	},
	"actions/force_field": function(e)
	{
		e.ReadU8("type")
		for(var i = 0; i < 9; i++)
			e.ReadFP32("param_"+i)
		e.ReadU8("flags0", "bool8")
		e.ReadVec3("rotation_power")
		e.ReadU8("anim_type")
	},
	"actions/force_free": null,
	"actions/freegun": null,
	"actions/gasmask hit": function(e)
	{
		ReadActionHit(e)
	},
	"actions/gasmask_spend_speed": function(e)
	{
		e.ReadFP32("speed")
		e.ReadU32("blend")
	},
	"actions/goto_target": function(e)
	{
		e.ReadFP32("target_range")
		e.ReadFP32("not_going_range")
		e.ReadU8("goto_movement_type")
		e.ReadU8("arrival_type")
		e.ReadU8("flags", "bool8")
	},
	"actions/hint": function(e)
	{
		e.ReadHintStr("hint", "choose")
		e.ReadU8("flags0", "bool8")
		e.ReadFP32("x")
		e.ReadFP32("y")
		e.ReadFP32("width")
		e.ReadFP32("height")
	},
	"actions/hit": function(e)
	{
		ReadActionHit(e)
	},
	"actions/howl": null, // build 2012-12-03
	"actions/human/property": function(e)
	{
		e.ReadString("name")
		e.ReadS32("value")
	},
	"actions/long idle": null, // build 2012-12-03
	"actions/luminocity": null,
	"actions/make friend": function(e)
	{
		e.ReadU8("make_friend_type")
	},
	"actions/make immortal": function(e)
	{
		if(entity_ver >= ENTITY_VER_29)
			e.ReadBool("check_only")
	},
	"actions/make invul": function(e)
	{
		if(entity_ver >= ENTITY_VER_29)
			e.ReadBool("check_only")
	},
	"actions/make scripted": function(e)
	{
		e.ReadBool("make_scripted")
	},
	"actions/map_text": function(e)
	{
		if(entity_ver >= ENTITY_VER_29)
		{
			e.ReadHintStr("key", "choose")
			e.ReadU8("type")
			e.ReadU8("order")
			e.ReadBool("notify")
		}
		else
		{
			e.ReadHintStr("key", "choose")
			e.ReadU32("size")
			e.ReadU32("rotation")
			if(entity_ver >= ENTITY_VER_28b)
				e.ReadVec4i("rect")
			else
				e.ReadVec4("rect")
			e.ReadU8("line_spacing")
			e.ReadVec4("color", "color, vec4f")
			e.ReadBool("notify")
		}
	},
	"actions/min mental state": function(e)
	{
		e.ReadU8("mental_state")
		e.ReadU8("max_mental_state")
	},
	"actions/monster/action_pathfind_collision": null,
	"actions/monster/aqua_female_sputum": null, // build 2012-12-03
	"actions/monster/aqua_female_state": function(e)
	{
		e.ReadU8("state")
	},
	"actions/monster/aqua_monster_mode": function(e)
	{
		e.ReadBool("set_water")
		e.ReadBool("water")
		e.ReadBool("set_tired")
		e.ReadBool("tired")
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
	"actions/monster/demon_ragdoll": null, // build 2012-10-19
	"actions/monster/group_behaviour_allowed": null,
	"actions/monster/group_behaviour_params": function(e)
	{
		e.ReadU8("_group_model")
		e.ReadFP32("_orbit_radius")
	},
	"actions/monster/monster_passive": function(e)
	{
		e.ReadU8("mode")
	},
	"actions/monster/nosalis_female_sleep": null,
	"actions/monster/nosalis_female_anchor": function(e) // build 2012-10-19
	{
		e.ReadFP32("radius")
	},
	"actions/monster/nosalis_female_state": function(e)
	{
		e.ReadU8("_state")
	},
	"actions/monster/set_lurkers_state": function(e)
	{
		e.ReadU8("state")
		e.ReadBool("_force_nolair_attack")
	},
	"actions/monster/watchman_get_reinforcements": null,
	"actions/move": function(e)
	{
		e.ReadU8("flags", "bool8")
		e.ReadVec3("offset")
	},
	"actions/move2cover": function(e)
	{
		e.ReadU8("flags", "bool8")
		e.ReadVec3("offset")
		
		e.ReadFP32("min_distance")
		e.ReadFP32("max_distance")
		e.ReadU8("cover_type")
		e.ReadU8("follow_link")
		e.ReadBool("strict_lock")
		e.ReadBool("accessibility_check")		
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
	"actions/net/net_prop_bit": function(e)
	{
		e.ReadU32("prop_id")
		e.ReadU8("idx")
		e.ReadBool("value")
		e.ReadString("comments")
	},
	"actions/net/set_class": function(e)
	{
		e.ReadU32("_class")
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
	"actions/npc/check_movement": function(e)
	{
		e.ReadFP32("dist")
		e.ReadFP32("angle", "angle, fp32")
		e.ReadBool("check_npc")
	},
	"actions/npc/cover group": function(e)
	{
		e.ReadHintStr("cover_group", "choose")
		e.ReadU8("allow")
		e.ReadU8("forbid")
	},
	"actions/npc/dispersion": function(e) // build 2012-12-03
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
	"actions/npc/group": function(e) // build 2012-10-19
	{
		e.ReadString("group_id")
	},
	"actions/npc/flinch_clear": null,
	"actions/npc/forbid_melee_kill": null,
	"actions/npc/free2go": null,
	"actions/npc/gasmask_voice": null,
	"actions/npc/hit_power": function(e)
	{
		e.ReadFP32("power")
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
			"suicide"
		].forEach(function(hit) { e.ReadFP32(hit); })
		
		if(entity_ver >= ENTITY_VER_29)
		{
			[
				"lian_slap",
				"ultrasound",
				"aqua_sputum"
			].forEach(function(hit) { e.ReadFP32(hit); })
		}
	},
	"actions/npc/lock_vs": null,
	"actions/npc/obstacle": function(e)
	{
		e.ReadHintStr("obstacle", "choose")
		e.ReadBool("add_obstacle")
		e.ReadBool("check_objects")
	},
	"actions/npc/set_hide_cover": null,
	"actions/npc/wounded": null,
	"actions/one shot": function(e)
	{
		e.ReadU8("flags0", "bool8")
	},
	"actions/open fire": function(e)
	{
		e.ReadU16("target", "entity_link, uobject_link");
		e.ReadU32("min_queue_size");
		e.ReadU32("max_queue_size");
		e.ReadU32("min_queue_interval");
		e.ReadU32("max_queue_interval");	
		e.ReadBool("instant");
	},	
	"actions/particles color": function(e)
	{
		e.ReadVec4("color", "color, vec4f")
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
	"actions/platform_attach": null,
	"actions/platform_deattach": null,
	"actions/play camera-effect": function(e)
	{
		e.ReadHintStr("cameraeffect", "camera_track, str_shared")
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
	},
	"actions/play color cube": function(e)
	{
		e.ReadHintStr("color_cube", "choose")
		e.ReadU32("blend_time")
		e.ReadBool("reset")
	},
	"actions/play coloranim": function(e)
	{
		e.ReadHintStr("coloranim", "ref_coloranim")
		e.ReadBool("looped")
	},
	"actions/play cover path": function(e)
	{
		e.ReadBool("fast_switch")
		e.ReadBool("fast_callback")
	},
	"actions/play cover path ex": function(e) // build 2012-10-19
	{
		e.ReadBool("fast_switch")
		e.ReadBool("fast_callback")
	},
	"actions/play modifier": function(e)
	{
		e.ReadHintStr("dbg_model", "ref_model")
		e.ReadString("dbg_skel")
		e.ReadHintStr("attp", "locator_str")
		e.ReadHintStr("modifier", "particles_modifier, str_shared")
		if(entity_ver == ENTITY_VER_28a)
			e.ReadU16("source", "entity_link, uobject_link")
	},
	"actions/play motion": function(e)
	{
		ReadActionPlayMotion(e)
	},
	"actions/play motion control" : function(e)
	{
		ReadActionPlayMotionControl(e)
	},
	"actions/play motion control ch": function(e)
	{
		ReadActionPlayMotionControl(e)
		
		e.ReadU16("fwd_frame")
	},
	"actions/play motion ex": function(e)
	{
		ReadActionPlayMotion(e)
		
		e.ReadHintStr("dbg_object_model", "ref_model")
		e.ReadHintStr("object_attp_id", "locator_str")
		e.ReadU8("flags0", "bool8")
		e.ReadFP32("p2p_offset")
		
		e.ReadBool("stick_to_ai_map")	
	},
	"actions/play music theme": function(e)
	{
		e.ReadHintStr("theme_name", "choose")
	},
	"actions/play particles": function(e)
	{
		ReadActionPlayParticles(e)
	},
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
	"actions/play sound": function(e)
	{
		ReadActionPlaySound(e)
	},
	"actions/play sound ex": function(e)
	{
		ReadActionPlaySound(e)
		
		e.ReadU8("importance")
		e.ReadU32("falloff")
		//e.ReadU8("queueing")
		e.ReadU8("flags")
		e.ReadU32("delay")
	},
	"actions/play sound ex control": function(e)
	{
		e.ReadBool("interrupt_ovewrite")
	},
	"actions/player/active_slot": function(e)
	{
		e.ReadU8("slot")
	},
	"actions/player/civil mode": function(e)
	{
		e.ReadU8("flags0", "bool8") // instant, allow_inventory_ui, torchlight
		var slots = e.ReadSection("slots")
		slots.ReadBool("knife_slot")
		slots.ReadBool("secondary_slot")
		slots.ReadBool("primary_slot_1")
		slots.ReadBool("grenade_slot")
		slots.ReadBool("gasmask_slot")
		slots.ReadBool("nightvision_slot")
		slots.ReadBool("map_slot")
		slots.ReadBool("medkit_slot")
		slots.ReadBool("filter_slot")
		slots.ReadBool("grenade_sticky")
		slots.ReadBool("launcher_time")
		slots.ReadBool("macheta")
		slots.ReadBool("charger")
		slots.ReadBool("grenade_flame_slot")
		slots.ReadBool("claymore")
		slots.ReadBool("heap")
		slots.ReadBool("primary_slot_2")
		slots.ReadBool("arrow_slot")
		slots.ReadBool("c4")
		slots.ReadBool("lighter")
		slots.ReadBool("shield")
		slots.ReadBool("no_slot")
	},
	"actions/player/doctor": function(e)
	{
		e.ReadU32("boost_time")
	},
	"actions/player/friend_mode": null,
	"actions/player/friend_mode_target": null,
	"actions/player/hide hands": null,
	"actions/player/hide subj": function(e)
	{
		e.ReadBool("hands")
		e.ReadBool("knife")
	},
	"actions/player/lock camera": function(e)
	{
		e.ReadFP32("inertion")
		e.ReadHintStr("model", "ref_model")
		e.ReadHintStr("bone", "locator_str")
		// if version < 7
		//e.ReadVec2("limit_yaw")
		//e.ReadVec2("limit_pitch")
		//e.ReadBool("ignore_rotation")
		//e.ReadBool("centering_epsilon_accuracy")
		e.ReadVec2("limit_yaw", "ang2f")
		e.ReadVec2("soft_limit_yaw", "ang2f")
		e.ReadVec2("limit_pitch", "ang2f")
		e.ReadVec2("soft_limit_pitch", "ang2f")
		e.ReadU32("noinput_return_delay")
		e.ReadFP32("noinput_return_angle", "angle, fp32")
		e.ReadFP32("softness")
		e.ReadFP32("blend")
		e.ReadU8("flags", "bool8") // rotate_to_target, rotate_precisely, skip_attach_on_deactivate, enable_soft_limits
	},
	"actions/player/luminocity correction": function(e)
	{
		e.ReadFP32("_coef")
	},
	"actions/player/purge_ammo": function(e)
	{
		if(entity_ver < ENTITY_VER_28b) // if script_version < 20
			e.ReadU32Array16("ammo_types", "u32_array")
		else
			e.ReadU8Array("ammo_types")
		e.ReadBool("all")
	},
	"actions/player/restrictor_obstacle": null,
	"actions/player/show crosshair": function(e)
	{
		e.ReadFP32("dispersion")
	},
	"actions/player/show hands": null,
	"actions/player/show subj": function(e)
	{
		e.ReadBool("hands")
		e.ReadBool("knife")
	},
	"actions/player/unlimited ammo": null, // build 2012-10-19
	"actions/player/way_point": null,
	"actions/scound_schema": function(e)
	{
		e.ReadHintStr("sound_scheme", "choose")
	},
	"actions/set enemy": function(e)
	{
		e.ReadU16("enemy", "entity_link, uobject_link")
		e.ReadU16("point", "entity_link, uobject_link")
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
	},
	"actions/set_filters_spend": function(e)
	{
		e.ReadBool("indoor_mode")
	},
	"actions/set_speech_groups": function(e)
	{
		e.ReadString("allowed_speech_groups")
	},
	"actions/show_diary": null,
	"actions/show_hitmark": function(e)
	{
		e.ReadBool("_hit_attach")
		e.ReadBool("_hit_friend")
		e.ReadFP32("_hit_power")
	},
	"actions/sound params": function(e)
	{
		e.ReadU8("target_mental_state")
		e.ReadFP32("disturb_threshold")
		e.ReadFP32("light_alert_threshold")
		e.ReadFP32("aleft_threshold")
		e.ReadFP32("danger_threshold")
	},
	"actions/static_combat": function(e)
	{
		e.ReadU8("static_combat_flags", "bool8")
		e.ReadHintStr("model", "ref_model")
		e.ReadString("dbg_skel")
		e.ReadHintStr("static_idle", "animation_str")
		e.ReadHintStr("static_attack", "animation_str")
		e.ReadHintStr("static_reload_idle", "animation_str")
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
	"actions/stretchy_man": null,
	"actions/take_cover": null,
	"actions/toggle": null,
	"actions/turn ladder": function(e)
	{
		e.ReadBool("enabled")
	},
	"actions/turn light": function(e)
	{
		e.ReadU8("onoff")
		e.ReadBool("on_or_off")
	},
	"actions/update ik": null,
	"actions/vision params": function(e)
	{
		e.ReadU8("target_mental_state")
		e.ReadFP32("vision_range")
	},
	"actions/water_blend": function(e)
	{
		e.ReadU32("blend_time")
	},
	"actions/water_move": function(e)
	{
		e.ReadVec3("offset")
		e.ReadU32("move_time")
	},
	"actions/wear gasmask": function(e)
	{
		e.ReadU8("flags0", "bool8")
	},
	"actions/wear_suit": function(e)
	{
		e.ReadU32("suit_type")
	},
	"actions/zone type": function(e)
	{
		e.ReadU8("zone_type")
	},
	"checker/entity/state": null,
	"checker/gamepad": null,
	"checker/npc/enemy": function(e)
	{
		e.ReadU32("npc_type")
	},
	"checker/platform": null,
	"checker/thq_connect": null,
	"covers/ref": function(e)
	{
		e.ReadU16("target", "cover_link, ucover_link")
	},
	"entities/entity kulemet hands": null,
	"entities/entity labeled": function(e)
	{
		e.ReadString("label")
	},
	"entities/entity level player": function(e)
	{
		e.ReadU16("entity", "entity_link, uobject_link")
	},
	"entities/entity name": function(e)
	{
		e.ReadString("name")
	},
	"entities/entity parent": null,
	"entities/entity player": function(e)
	{
		e.ReadU8("team");
	},
	"entities/entity player self": function(e)
	{
		e.ReadU8("team");
	},
	"entities/entity player's hands": null,
	"entities/entity player's knife": null,
	"entities/entity player's torch": null,
	"entities/entity ref": function(e)
	{
		e.ReadU16("target", "entity_link, uobject_link")
	},
	"entities/entity self": null,
	"entities/entity user": function(e) // build 2012-12-03
	{
		e.ReadU8("team")
	},
	"entities/monster_enemy": null,
	"entities/player_enemy": null,
	"fun/text splash": function(e)
	{
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
		e.ReadS32("value")
		e.ReadS32("current_value")
		e.ReadS32("reference")
	},
	"logic/adv/randomex": function(e)
	{
		e.ReadU8("quant")
		var count = e.ReadU8("states_count")
		for(var i = 0; i < count; i++)
		{
			e.ReadBool("on_"+i)
			e.ReadU8("wgt_"+i)
		}
	},
	"logic/adv/randomex-3": function(e)
	{
		e.ReadU8("quant")
		var count = e.ReadU8("states_count")
		for(var i = 0; i < count; i++)
		{
			e.ReadBool("on_"+i)
			e.ReadU8("wgt_"+i)
		}		
	}, 
	"logic/adv/randomex-4": function(e)
	{
		e.ReadU8("quant")
		var count = e.ReadU8("states_count")
		for(var i = 0; i < count; i++)
		{
			e.ReadBool("on_"+i)
			e.ReadU8("wgt_"+i)
		}		
	},
	"logic/adv/randomex-5": function(e)
	{
		e.ReadU8("quant")
		var count = e.ReadU8("states_count")
		for(var i = 0; i < count; i++)
		{
			e.ReadBool("on_"+i)
			e.ReadU8("wgt_"+i)
		}		
	},
	"logic/ai/cover/escape": null,
	"logic/ai/cover/near player": function(e)
	{
		e.ReadFP32("dist_coef")
		e.ReadFP32("dir_coef")
		e.ReadFP32("back_coef")
		e.ReadFP32("fwd_coef")
		e.ReadBool("check_enemy_accessibility")
		e.ReadFP32("enemy_dir_coef")
	},
	"logic/ai/cover/nearest": function(e)
	{
		e.ReadFP32("dist_coef")
		e.ReadFP32("dir_coef")
		e.ReadFP32("direct_path_coef")
		e.ReadFP32("max_distance")
	},
	"logic/ai/cover/random": null,
	"logic/and-2": function(e)
	{
		e.ReadBool("auto_touch")
	},
	"logic/and-3": function(e)
	{
		e.ReadBool("auto_touch")
	},
	"logic/counter": function(e)
	{
		ReadLogicCounter(e)
	},
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
	"logic/delay": function(e)
	{
		//e.ReadHint("min", "time")
		e.ReadU32("min")
		//e.ReadHint("max", "time")
		e.ReadU32("max")
		e.ReadU8("dflags", "bool8")
	},
	"logic/exposed_locker": function(e)
	{
		e.ReadU8("flags", "bool8")
		e.ReadString("exp_name")
	},
	"logic/locker": function(e)
	{
		e.ReadU8("flags", "bool8")
	},
	"logic/locker_base": function(e)
	{
		e.ReadU8("flags", "bool8")
	},
	"logic/or-2": function(e) // build 2012-12-03
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
	"logic/random-chooser": function(e)
	{
		e.ReadU8("quant")
		var count = e.ReadU8("prop_count")
		for(var i = 0; i < count; i++)
			e.ReadFP32("prop"+i)
	},
	"logic/random-chooser-2": function(e)
	{
		e.ReadU8("quant")
		var count = e.ReadU8("prob_count")
		for(var i = 0; i < count; i++)
			e.ReadFP32("prob"+i)	
	},
	"logic/random-chooser-3": function(e)
	{
		e.ReadU8("quant")
		var count = e.ReadU8("prob_count")
		for(var i = 0; i < count; i++)
			e.ReadFP32("prob"+i)	
	},
	"logic/select-2": function(e)
	{
		e.ReadU8("quant")
	},
	"logic/select_param": function(e)
	{
		e.ReadU8("quant")
	},
	"logic/select-3": function(e)
	{
		e.ReadU8("quant")
	},
	"logic/select-4": function(e)
	{
		e.ReadU8("quant")
	},
	"logic/select-5": function(e)
	{
		e.ReadU8("quant")
	},
	"logic/switch_param": function(e)
	{
		e.ReadU8("quant")
		e.ReadU8("type")
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
		e.ReadBool("attach_entity")
	},
	"trade/take item": function(e)
	{
		e.ReadBool("force_activation")
		e.ReadBool("creating")
	},
	"trade/test my weapon": null, // build 2012-10-19
	"trade/trade trigger": function(e)
	{
		if(entity_ver >= ENTITY_VER_29)
		{
			e.ReadBool("active")
			e.ReadU8("trade_type")
			e.ReadU32("current_object")
			var objects_count_pre = e.ReadU32("objects_count_pre")
			e.ReadU32("trade_preset")
			e.ReadBool("need_attach")
			for(var i = 1; i <= objects_count_pre; i++)
			{
				e.ReadU16("object_"+i+"_link", "entity_link, uobject_link")
				e.ReadU32("object_"+i+"_needs_dlc")
			}
			e.ReadU32("objects_count")
			e.ReadU16("custom price", "entity_link, uobject_link")
			e.ReadBool("charity_mode")
			e.ReadFP32("cam_track_accrue")
			e.ReadFP32("cam_track_falloff")
		}
		else if(entity_ver >= ENTITY_VER_28a) // build 2012-10-19
		{
			e.ReadBool("active")
			e.ReadU8("trade_type")
			e.ReadU32("current_object")
			var objects_count_pre = e.ReadU32("objects_count_pre")
			e.ReadU32("trade_slot")
			e.ReadU32("trade_preset")
			e.ReadBool("need_attach")
			for(var i = 1; i <= objects_count_pre; i++)
			{
				e.ReadU16("object_"+i+"_link", "entity_link, uobject_link")
				e.ReadU16("object_"+i+"_price_link", "entity_link, uobject_link")
				e.ReadU16("object_"+i+"_info_link", "entity_link, uobject_link")
				e.ReadU32("object_"+i+"_needs_dlc")
			}
			e.ReadU32("objects_count")
			e.ReadU16("custom price", "entity_link, uobject_link")
			e.ReadU16("custom desc", "entity_link, uobject_link")
			e.ReadBool("charity_mode")
			
			if(entity_ver >= ENTITY_VER_28b) // build 2012-12-03
			{
				e.ReadFP32("cam_track_accrue")
				e.ReadFP32("cam_track_falloff")
			}
		}
	},
	"trigger/ai map leaving": function(e)
	{
		e.ReadBool("active")
		e.ReadFP32("max_dist_sq")
	},
	"trigger/ammo spend": function(e)
	{
		e.ReadBool("active")
		e.ReadU32("min_cnt")
		e.ReadU32("max_cnt")
	},
	"trigger/hearing": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("player")
		e.ReadFP32("sound_power")
	},
	"trigger/vision": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("player")
		e.ReadFP32("vision_value")
	},
	"triggers/active item": function(e)
	{
		e.ReadBool("active")
		e.ReadU32("weapon_type")
	},
	"triggers/aftercloned": ReadTrigger,
	"triggers/ammo absent": function(e)
	{
		e.ReadBool("active")
		e.ReadU16("min_ammo_count")
		e.ReadBool("loaded")
	},
	"triggers/anim event": function(e)
	{
		e.ReadBool("active")
		e.ReadU16("entity", "entity_link, uobject_link")
		e.ReadString("event")
	},
	"triggers/any player": function(e)
	{
		e.ReadBool("active")
		e.ReadS32("reaction_count")
		e.ReadU16("zone_link", "entity_link, uobject_link")
		e.ReadU8("_friend")
		e.ReadU8("_team")
	},
	"triggers/any_input": ReadTrigger,
	"triggers/aqua female": function(e)
	{
		e.ReadBool("active")
		e.ReadU8("state")
	},
	"triggers/aqua male": ReadTrigger,
	"triggers/arahind state": function(e)
	{
		e.ReadBool("active")
		e.ReadFP32("_fake_light_multiplier")
		e.ReadBool("_force_light")
	},
	"triggers/attach-detach": ReadTrigger,
	"triggers/auto_save_info": ReadTrigger,
	"triggers/body_posture": null,
	"triggers/camera-track event": function(e)
	{
		e.ReadBool("active")
		e.ReadString("event")
	},
	"triggers/charger": function(e)
	{
		e.ReadBool("active")
		e.ReadFP32("min")
		e.ReadFP32("max")
	},
	"triggers/collision": function(e) // build 2012-10-19
	{
		e.ReadBool("active")
		e.ReadU32("collisions_group")
	},
	"triggers/continue_no_available": ReadTrigger, // build 2012-10-19
	"triggers/debug_input": function(e)
	{
		e.ReadBool("active")
		e.ReadU32("key")
		e.ReadU32("modifier1")
		e.ReadU32("modifier2")
		e.ReadBool("network")
	},
	"triggers/die": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("player")
		if(entity_ver >= ENTITY_VER_29)
			e.ReadBool("from_fire")
	},
	"triggers/engine/signal": function(e)
	{
		e.ReadBool("active")
		e.ReadHintStr("signal", "choose")
	},
	"triggers/entity/health threshold": function(e)
	{
		e.ReadBool("active")
		e.ReadFP32("threshold")
		e.ReadFP32("threshold_step")
		e.ReadBool("positive")
	},
	"triggers/entity/health threshold2": function(e)
	{
		e.ReadBool("active")
		e.ReadFP32("threshold")
		e.ReadFP32("threshold_step")
		e.ReadBool("positive")
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
	"triggers/game/menu trigger": function(e)
	{
		e.ReadBool("active")
		e.ReadU32("current_object")
		var count = e.ReadU32("objects_count_pre")
		for(var i = 1; i <= count; i++)
		{
			e.ReadString("object_" + i + "_name")
			e.ReadU16("object_" + i + "_link")
			e.ReadString("entity_" + i + "_name")
			e.ReadU16("entity_" + i + "_link")
		}
		e.ReadU32("objects_count")
		e.ReadBool("ignore_select_input")
	},
	"triggers/game/ranger mode": ReadTrigger,
	"triggers/game_difficulty": function(e)
	{
		e.ReadU32("difficulty_id")
	},
	"triggers/game_difficulty_change": ReadTrigger,
	"triggers/gameloaded": ReadTrigger,
	"triggers/gamepad": ReadTrigger,
	"triggers/gasmask": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("trigger_on_wear")
	},
	"triggers/generic": function(e)
	{
		e.ReadBool("active")
		e.ReadS32("reaction_count")
		e.ReadU16("zone_link", "entity_link, uobject_link")
	},
	"triggers/has_target": function(e)
	{
		e.ReadBool("active")
		e.ReadU32("tid")
		e.ReadFP32("distance")
		e.ReadFP32("threshold")
		if(entity_ver >= ENTITY_VER_28b) 
			e.ReadFP32("aimap_threshold")
		e.ReadU32("time_threshold")
		e.ReadU8("_friend")
		e.ReadU32("npc_type")
		if(entity_ver >= ENTITY_VER_28b) 
			e.ReadU8("flags8", "bool8")
		else
			e.ReadBool("ai_map_check")
		e.ReadString("ex_prop")
	},
	"triggers/hit": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("player")
		e.ReadU8("hit_type")
		e.ReadU32("weapon_type") // in version < 8 was string
		e.ReadHintStr("bone", "locator_str")
		e.ReadFP32("range_min")
		e.ReadFP32("range_max")
		if(entity_ver > ENTITY_VER_28a) // build 2012-10-19
			e.ReadBool("fly_by")
	},
	"triggers/illumination": function(e) // build 2012-10-19
	{
		e.ReadBool("active")
		e.ReadFP32("illumination")
	},
	"triggers/input": function(e)
	{
		e.ReadBool("active")
		e.ReadU32("action")
		e.ReadU8("flags0", "bool8") // allow_input, network, check_on_activate, signal_on_activate
	},
	"triggers/interest": function(e)
	{
		e.ReadBool("active")
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
			"AK 74", "AK 74 silencer",
			"2012", "2012 silencer",
			"VSV", 
			"macheta", "dagger",
			"medved", 
			"flamethrower",
			"ventil",
			"ashot", "ashot silencer",
			"padonag", "padonag silencer",
			"gatling",
			"saiga",
			"tumak"
		]
			
		e.ReadBool("active")
		for(var i = 0; i < weapons.length; i++)
		{
			var r = e.ReadSection(weapons[i])
			r.ReadBool("primary_"+weapons[i])
			r.ReadBool("secondary_"+weapons[i])
		}
		
		e.ReadBool("enabled_dynamite")
		e.ReadBool("enabled_launcher")
		e.ReadBool("enabled_shotgun")
	},
	"triggers/is god mode": ReadTrigger,
	"triggers/is milestone": ReadTrigger,
	"triggers/ladder": ReadTrigger,
	"triggers/level_mental": function(e)
	{
		e.ReadBool("active")
		e.ReadU8("mental_border")
	},
	"triggers/menu event": function(e)
	{
		e.ReadBool("active")
		e.ReadU32("menu_event")
	},
	"triggers/net/check_prop_bit": function(e)
	{
		e.ReadBool("active")
		e.ReadU32("prop_id")
		e.ReadU8("idx")
	},
	"triggers/net/net_signin": ReadTrigger,
	"triggers/net/state": ReadTrigger, // build 2012-12-03
	"triggers/nextlevel": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("trigger_on_next_level")
	},
	"triggers/npc": function(e)
	{
		e.ReadBool("active")
		e.ReadS32("reaction_count")
		e.ReadU16("zone_link", "entity_link, uobject_link")
	},
	"triggers/npc enemy": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("player")
	},
	"triggers/npc enemy is close": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("player")
		e.ReadFP32("distance")
		e.ReadFP32("distance_far")
		e.ReadU32("npc_type")
	},
	"triggers/npc mental state": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("player")
		e.ReadU8("min_mental_state")
		e.ReadU8("max_mental_state")
	},
	"triggers/npc threat": ReadTrigger,
	"triggers/npc_die_from": function(e)
	{
		e.ReadBool("active")
		e.ReadU32("weapon_type")
		e.ReadU32("npc_type")
		e.ReadU8("friend")
		e.ReadU8("mp_class_type")
		e.ReadFP32(("threshold_min"))
		e.ReadFP32("threshold_max")
	},
	"triggers/object": function(e)
	{
		e.ReadBool("active")
		e.ReadS32("reaction_count")
		e.ReadU16("zone_link", "entity_link, uobject_link")
		e.ReadU16("object", "entity_link, uobject_link")
	},
	"triggers/object_is_close": function(e)
	{
		e.ReadBool("active")
		e.ReadFP32("distance")
	},
	"triggers/on use": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("player")
	},
	"triggers/out of filters": function(e)
	{
		e.ReadBool("active")
		e.ReadU32("border_time")
		e.ReadBool("filters")
	},
	"triggers/pass cover": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("once")
		e.ReadU8("cover_type")
	},
	"triggers/pathfind enemy": ReadTrigger,
	"triggers/player": function(e)
	{
		e.ReadBool("active")
		e.ReadS32("reaction_count")
		e.ReadU16("zone_link", "entity_link, uobject_link")
	},
	"triggers/player die": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("bullet_kill")
		e.ReadU32("npc_type") // in version <= 8 was string
	},
	"triggers/player_special": function(e)
	{
		e.ReadBool("active")
		e.ReadS32("reaction_count")
		e.ReadU8("zone_type")
	},
	"triggers/start_script": ReadTrigger,
	"triggers/startgame": function(e)
	{
		e.ReadBool("active")
		e.ReadU8("flags", "bool8")
	},
	"triggers/stop_script": ReadTrigger, // build 2012-10-19
	"triggers/time_in_game": function(e)
	{
		e.ReadBool("active")
		e.ReadFP32("time_in_game")
		e.ReadBool("once")
	},
	"triggers/torch": ReadTrigger,
	"triggers/use": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("player")
		e.ReadFP32("usage_distance")
		e.ReadHintStr("use_action", "choose")
		e.ReadVec2("use_offset")
			
		if(entity_ver >= ENTITY_VER_28b)
		{
			e.ReadFP32("blink_distance")
			e.ReadU8("flags8", "bool8")
		}
		else
		{
			e.ReadBool("blink")
			e.ReadFP32("blink_distance")
			e.ReadBool("check_need_end")
		}
		
		e.ReadU8("user_team")
		e.ReadBool("in_reloading")
		
		if(entity_ver >= ENTITY_VER_28b) // if script_version >= 20
			e.ReadU8Array("mp_classes")
		else
			e.ReadU32Array16("mp_classes", "u32_array")
	},
	"triggers/velocity": function(e)
	{
		e.ReadBool("active")
		e.ReadU16("entity", "entity_link, uobject_link")
		e.ReadFP32("velocity")
	},
	"triggers/watchman group": ReadTrigger,
	"triggers/web_burn": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("player")
	},
	"triggers/wounded": ReadTrigger,
	"triggers/zombie": function(e)
	{
		e.ReadBool("active")
		e.ReadBool("player")
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
	"trolley/kulemet mode": function(e)
	{
		e.ReadVec2("limit_yaw")
		e.ReadVec2("limit_pitch")
		e.ReadFP32("limit_factor")
		e.ReadBool("deactivate_on_use")
		e.ReadU16("npc", "entity_link, uobject_link")
	},
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
	
	// блоки для сетевой игры
	// в официальных картах не встречаются 
	"actions/net/make_ready": function(e)
	{
		e.ReadBool("remove_from_team")
	},
	"actions/net/respawn": function(e)
	{
		e.ReadU32("cover_reuse_delay")
		e.ReadU8("count")
		e.ReadU16("source", "entity_link, uobject_link")
		e.ReadU16("position", "entity_link, uobject_link")
		e.ReadU8("team")
		e.ReadU8("flags0", "bool8")
	},
	"actions/net/respawn_ex": function(e)
	{
		e.ReadU32("cover_reuse_delay")
		e.ReadU8("count")
		e.ReadU8("team")
		e.ReadU8("flags0", "bool8")
		e.ReadU32("sources_count")
		e.ReadU8("ignore_mp_class_type")
	},
	"actions/net/ui_clone": function(e)
	{
		e.ReadU32("cover_reuse_delay")
		e.ReadU8("count")
		e.ReadU8("subtype")
	},
	"logic/delay hud": function(e)
	{
		// logic/delay
		e.ReadU32("min")
		e.ReadU32("max")
		e.ReadU8("dflags", "bool8")
		
		// logic/delay hud
		e.ReadU32("delta")
		e.ReadU32("priority")
		e.ReadVec2("font_position")
		e.ReadFP32("font_size")
		e.ReadVec4("font_color", "color, vec4f")
		e.ReadU8("team_vis")
		e.ReadBool("show_minutes")
		e.ReadBool("respawn_time")
		e.ReadBool("timer_locked")
		e.ReadBool("buy_respawn")
		e.ReadU32("cost_respawn")
		e.ReadFP32("buy_time")
		e.ReadU8("buy_coeff")
		e.ReadFP32("buy_cost_coeff")
		e.ReadBool("ctf_logic")
	},
	"triggers/net/log_start_game": null,
	"triggers/net/log_spawn": null,
	"triggers/game/spectator trigger": function(e)
	{
		e.ReadBool("active")
		e.ReadU16("camera_team", "entity_link, uobject_link")
		e.ReadU32("current_object")
		var count = e.ReadU32("objects_count_pre")
		for(var i = 0; i < count; i++)
		{
			e.ReadString("object_" + i + "_name")
			e.ReadU16("object_" + i + "_link", "entity_link, uobject_link")
			e.ReadU16("object_" + i + "_lamp", "entity_link, uobject_link")
			e.ReadU16("object_" + i + "_camera", "entity_link, uobject_link")
		}
		e.ReadU32("objects_count")
		e.ReadU8("team")
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