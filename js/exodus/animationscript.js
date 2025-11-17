var vs_clsid = module("vs_clsid")

// identical to ReadVS from visualscript
function ReadAS(e)
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
	var clsid = block.ReadStringCrc("clsid", vs_clsid.get_clsid) // for some reason abramcumner's list of clsids I copied from MetroEx contains AS blocks too, so it's ok	
	
	var reader = as_block_readers[clsid]
	if(reader)
	{
		reader(block)
		
		if(block.More())
			print(clsid + " data left")
	}
	else
	{
		if(reader !== null)
		{
			print("not implemented block class " + clsid)
			as_block_readers[clsid] = null // prevent further warning messages
		}
	}
}

var as_block_readers = {
	"chooser/random"                : ReadChooserRandom,
	"empty"                         : ReadEmpty,
	"empty_not_delta"               : ReadEmpty,
	"filter/bone_filter"            : ReadFilterBoneFilter,
	"filter/bone_part"              : ReadFilterBonePart,
	"process/add"                   : ReadProcessAdd,
	"process/blender"               : ReadProcessBlender,
	"process/blender_match_events"  : ReadProcessBlender,        // not used anywhere in exodus !
	"process/blender_phased"        : ReadProcessBlender,        // not used anywhere in exodus !
	"process/bone part"             : ReadFilterBonePart,        // filter/bone_part editor's name
	"process/double"                : ReadProcessAdd,            // process/add editor's name
	"process/filter"                : ReadFilterBoneFilter,      // filter/bone_part editor's name
	"process/fixed blender"         : ReadProcessFixedBlender,   // process/fixed_blender editor's name
	"process/fixed_blender"         : ReadProcessFixedBlender,	
	"process/gesture redirect"      : ReadBlockBase,             // process/gesture_redirect editor's name
	"process/gesture_blender"       : ReadProcessGestureBlender,
	"process/gesture_redirect"      : ReadBlockBase,
	"process/IKs"                   : ReadProcessReplaceWithIKs, // process/replace with IKs editor's name
	"process/multi blender"         : ReadProcessMultiBlender,   // process/multi_blender editor's name
	"process/multi_blender"         : ReadProcessMultiBlender,
	"process/replace"               : ReadProcessReplace,
	"process/replace with IKs"      : ReadProcessReplaceWithIKs,
	"process/signal blender"        : ReadProcessSignalBlender,  // process/signal_blender editor's name
	"process/signal_blender"        : ReadProcessSignalBlender,
	
	"source/cover motion"           : ReadSourceCoverMotion,     // source/cover_motion editor's name
	"source/cover_motion"           : ReadSourceCoverMotion,
	"source/FaceFX motion"          : ReadSourceFacefxMotion,    // source/facefx_motion editor's name
	"source/facefx_motion"          : ReadSourceFacefxMotion,
	"source/gesture motion"         : ReadSourceGestureMotion,   // "source/gesture_motion" editor's name
	"source/gesture_motion"         : ReadSourceGestureMotion,
	"source/motion"                 : ReadSourceMotion,
	"source/param motion"           : ReadSourceParamMotion,     // "source/param_motion" editor's name
	"source/param_motion"           : ReadSourceParamMotion,
	"source/pausable motion"        : ReadSourcePausableMotion,  // source/pausable_motion editor's name
	"source/pausable_motion"        : ReadSourcePausableMotion,
	"source/PFNN motion"            : ReadSourcePFNNMotion,      // source/pfnn_motion editor's name
	"source/pfnn_motion"            : ReadSourcePFNNMotion,
	"source/physics"                : null,                      // not used anywhere in exodus !
	"source/signal motion"          : ReadSourceSignalMotion,    // "source/signal_motion" editor's name
	"source/signal_motion"          : ReadSourceSignalMotion,
	"source/subst motion"           : ReadSourceSubstMotion,     // "source/subst_motion" editor's name
	"source/subst_motion"           : ReadSourceSubstMotion,
	
	"state/detached state"          : ReadState,                 // is it any diffirent from state/state ?
	"state/dg transition"           : null,                      // "state/dg_transition" editor's name
	"state/dg_transition"           : null,                      // not used anywhere in exodus !
	"state/lock movement"           : ReadStateLockMovement,     // "state/lock_movement" editor's name
	"state/lock_movement"           : ReadStateLockMovement,
	"state/locomotion state"        : ReadLocomotionState,
	"state/param time transition"   : null,                      // either editor name on not used anywhere in exodus
	"state/param_transition"        : ReadStateParamTransition, 
	"state/placeholder"             : ReadStatePlaceholder,
	"state/random transition"       : ReadStateRandomTransition, // state/random_transition editor's name
	"state/random_transition"       : ReadStateRandomTransition,
	"state/redir"                   : null,                      // either editor name on not used anywhere in exodus
	"state/ref"                     : ReadStateRef,
	"state/state"                   : ReadState,
	"state/transition"              : ReadStateTransition
};

function ReadBlockBase(e)
{
	e.ReadU16("posx")
	e.ReadU16("posy")
	e.ReadString("caption")
}

function ReadChooserRandom(e)
{
	ReadBlockBase(e)
	
	e.ReadU32("source_cnt")
	e.ReadBool8("flags0", ["check_movement", "check_movement_move", "check_ai_map_only"])
	var cnt = e.ReadU32("source_cnt_pre")
	for(var i = 0; i < cnt; i++)
		e.ReadFP32("prob"+i)
	e.ReadBool8("flags0", ["choose_once", "random_mode", "use_global_cache"])
	e.ReadU16("fade")
}

function ReadEmpty(e)
{
	ReadBlockBase(e)
}

function ReadDoubleBlenderData(e) // it's called like that in arktika.1 
{
	ReadBlockBase(e)

	e.ReadU32("synch_type")
	e.ReadStrArray32("events")
	e.ReadBool("add_mode")
	var wm = e.ReadU8("weight_mode")
	if(wm === 3)
	{
		e.ReadHintStr("param", "param_str")
		e.ReadFP32("param_key0")
		e.ReadFP32("param_key1")
	}
}

function ReadFilterBoneFilter(e)
{
	ReadDoubleBlenderData(e)
	
	e.ReadBool8("flags", ["filtered_matters", "loc_add_mode", "apply_bones_mask"])
	e.ReadHintStr("bones", "choose_array, str_shared")
}

function ReadFilterBonePart(e)
{
	ReadDoubleBlenderData(e)
	
	e.ReadBool8("flags", ["filtered_matters", "loc_add_mode", "apply_bones_mask"]) // In Arktika.1 is was two separate bools btw
	e.ReadHintStr("bones", "choose_array, str_shared")
	e.ReadHintStr("bone_part", "part_str")
}

function ReadProcessAdd(e)
{
	ReadDoubleBlenderData(e)
}

function ReadProcessBlender_base(e)
{
	ReadBlockBase(e)

	e.ReadU32("synch_type")
	e.ReadStrArray32("events")
	var blend_cnt = e.ReadU32("blend_cnt_pre")
	for(var i = 0; i < blend_cnt; i++)
	{
		e.ReadFP32("param_key"+i)
	}
	e.ReadU32("blend_cnt")
}

function ReadProcessBlender(e)
{
	ReadProcessBlender_base(e)
	
	e.ReadHintStr("param", "param_str")
	e.ReadBool8("flags", ["fix_param", "chooser_mode"])
}

function ReadProcessFixedBlender(e)
{
	ReadProcessBlender_base(e)
	
	e.ReadFP32("fixed_param_value")
}

function ReadProcessGestureBlender(e)
{
	ReadDoubleBlenderData(e)
}

function ReadProcessReplace(e)
{
	ReadDoubleBlenderData(e)
}

function ReadProcessReplaceWithIKs(e)
{
	ReadDoubleBlenderData(e)
	
	var arr = e.ReadArray("ik_locks_ex", "lock_%.4d")
	while(arr.MoreElements())
	{
		var elem = arr.NextElement()
		elem.ReadFP32("pos_weight")
		elem.ReadFP32("quat_weight")
		elem.ReadU16("chain_idx")
		elem.ReadString("chain_name")
	}
}

function ReadProcessMultiBlender(e)
{
	ReadBlockBase(e)

	e.ReadU32("synch_type")
	e.ReadStrArray32("events")
	
	var param_cnt = e.ReadU32("param_cnt_pre")
	for(var i = 0; i < param_cnt; i++)
		e.ReadHintStr("param"+i, "param_str")
		
	var blend_cnt = e.ReadU32("blend_cnt_pre")
	for(var i = 0; i < blend_cnt; i++)
		for(var j = 0; j < param_cnt; j++)
			e.ReadFP32("param_key_"+i+"_"+j);
			
	var group_cnt = e.ReadU32("group_cnt_pre")
	for(var i = 0; i < group_cnt; i++)
		for(var j = 0; j < 4; j++)
			e.ReadU8("group_ind_"+i+"_"+j)
			
	e.ReadU32("param_cnt")
	e.ReadU32("blend_cnt")
	e.ReadU32("group_cnt")
	e.ReadBool("fix_param")
}

function ReadProcessSignalBlender(e)
{
	ReadProcessBlender(e)
	
	e.ReadString("on_start")
	e.ReadString("on_stop")
}

function ReadState(e)
{
	ReadBlockBase(e)

	e.ReadString("on_start")
	e.ReadString("on_stop")
	e.ReadBool8("flags", ["skip_start_stop", "disable_qsave", "detached", "hide_signals"])
	ReadAS(e)
	e.ReadU32("parent_link")
	e.ReadU32Array16("trans")
	e.ReadU32Array16("pure_from")
	var arr = e.ReadArray("trans_graph")
	while(arr.MoreElements())
	{
		var elem = arr.NextElement()
		elem.ReadU32Array16("trans_idx")
	}
}

function ReadStateRef(e)
{
	ReadBlockBase(e)

	e.ReadString("on_start")
	e.ReadString("on_stop")
	e.ReadBool8("flags", ["skip_start_stop", "disable_qsave", "detached", "hide_signals"])
	e.ReadHintStr("ref", "choose")
}

function ReadStateLockMovement(e)
{
	ReadBlockBase(e)
}

function ReadLocomotionState(e)
{
	ReadState(e)
	
	e.ReadU32("loco_link")
	e.ReadString("loco_group_id")
}

function ReadStateParamTransition(e)
{
	ReadBlockBase(e)

	e.ReadU32("synch_type")
	e.ReadStrArray32("events")
	e.ReadU16("postox")
	e.ReadU16("postoy")
	
	e.ReadString("request_cond")
	e.ReadStrArray32("events_cond")
	e.ReadU32("max_wait_time")
	e.ReadBool8("flags", ["just_check_event", "check_if_event_is_NOT_there", "param_conds_OR_mode"])
	e.ReadFP32("progress_cond")
	e.ReadFP32("current_time_cond")
	var cnt = e.ReadU32("param_cond_cnt_pre")
	for(var i = 0; i < cnt; i++)
	{
		var cond = e.ReadSection("param_cond"+i);
		cond.ReadHintStr("param", "param_str") // virtual function
		
		/* if(something) continue; */
		
		cond.ReadU8("op")
		
		var type = cond.ReadU8("value0_type0")
		switch(type)
		{
			case 0: /* nothing */ break;
			case 4: cond.ReadFP32("value0_fp32"); break;
			case 6: cond.ReadString("value0_str"); break;
			default: cond.ReadU16("value0_u16"); break;
		}
		cond.ReadU8("value0_type1");
		
		var type = cond.ReadU8("value1_type0")
		switch(type)
		{
			case 0: /* nothing */ break;
			case 4: cond.ReadFP32("value1_fp32"); break;
			case 6: cond.ReadString("value1_str"); break;
			default: cond.ReadU16("value1_u16"); break;
		}
		cond.ReadU8("value1_type1");
	}
	e.ReadU32("param_cond_cnt")
	e.ReadString("on_eval")
	e.ReadString("on_start")
	e.ReadString("on_stop")
	if( /*something*/ 1 )
	{
		e.ReadFP32("transition_speed")
	}
	e.ReadBool("force_transition")
	e.ReadS32("weight")
}

function ReadStatePlaceholder(e)
{
	ReadBlockBase(e)
	
	e.ReadString("on_start")
	e.ReadString("on_stop")
	
	e.ReadBool8("flags", ["skip_start_stop", "disable_qsave", "detached", "hide_signals"])
	
	e.ReadU32Array16("path_to_block")
	e.ReadU32Array16("path_to_ncp")
	e.ReadString("caption_block")
}

function ReadStateRandomTransition(e)
{
	ReadStateParamTransition(e)
	
	e.ReadU32("source_cnt")
	e.ReadBool8("flags0", ["check_movement", "check_movement_move", "check_ai_map_only"])
	var cnt = e.ReadU32("source_cnt_pre")
	for(var i = 0; i < cnt; i++)
		e.ReadFP32("prob"+i)
	e.ReadBool8("flags0", ["choose_once", "random_mode", "use_global_cache"])
	e.ReadU16("fade")
}

function ReadStateTransition(e)
{
	// same as state/param_transition
	ReadStateParamTransition(e)
}

function ReadSourceMotion(e)
{
	ReadBlockBase(e)
	
	e.ReadHintStr("animation", "animation_str")
	// SDK lua's also have 'animation_name' string, which is same as 'animation' ...
	e.ReadU8("force_looped")
	e.ReadFP32("force_speed")
	e.ReadU16("force_first_frame")
	e.ReadU16("force_last_frame")
	e.ReadFP32("cached_speed")
	e.ReadBool8("flags", ["is_inversed", "force_start_stop", "cached_looped", "cached_additive", "cached_is_xform_motion", "cached_absolute_xform", "cached_sound"])
}

function ReadSourceCoverMotion(e)
{
	ReadSourceMotion(e)
	
	e.ReadHintStr("pos_target", "param_str") // virtual function
	e.ReadHintStr("rot_target", "param_str") // virtual function
	e.ReadHintStr("attp_offset", "param_str") // virtual function
	e.ReadBool("move_to")
	e.ReadBool("precise_dist")
	e.ReadBool("freeze_posrot_target")
	e.ReadVec4("dest_offset_Q")
	e.ReadVec3("dest_offset_T")
	e.ReadHintStr("target_pose", "animation_str")
	e.ReadU8("cover_pose")
	
	e.ReadString("on_start")
	e.ReadString("on_stop")	
}

function ReadSourceFacefxMotion(e)
{
	ReadBlockBase(e)
	
	e.ReadHintStr("fx_anim", "choose")
	e.ReadFP32("rnd_offset")
}

function ReadSourceGestureMotion(e)
{
	ReadBlockBase(e)
	
	e.ReadString("request")
	e.ReadHintStr("param", "param_str") // virtual function
}

function ReadSourceParamMotion(e)
{
	ReadBlockBase(e)

	e.ReadU32("synch_type")
	e.ReadStrArray32("events")
	e.ReadU16("postox")
	e.ReadU16("postoy")

	e.ReadHintStr("param", "param_str") // virtual function
	
	e.ReadHintStr("pos_target", "param_str") // virtual function
	e.ReadHintStr("rot_target", "param_str") // virtual function
	e.ReadHintStr("attp_offset", "param_str") // virtual function
	e.ReadBool("move_to")
	e.ReadBool("precise_dist")
	e.ReadBool("freeze_posrot_target")
	e.ReadVec4("dest_offset_Q")
	e.ReadVec3("dest_offset_T")
	e.ReadHintStr("target_pose", "animation_str")
	e.ReadU8("cover_pose")			
}

function ReadSourcePausableMotion(e)
{
	ReadSourceMotion(e)
	
	e.ReadFP32("time_begin_value")
	e.ReadFP32("time_end_value")
	e.ReadHintStr("param", "param_str") // virtual function	
}

function ReadSourcePFNNMotion(e)
{
	ReadBlockBase(e)
}

function ReadSourceSignalMotion(e)
{
	ReadSourceMotion(e)

	e.ReadString("on_start")
	e.ReadString("on_stop")
	e.ReadString("on_end")
}

function ReadSourceSubstMotion(e)
{
	ReadBlockBase(e)
	
	e.ReadHintStr("animation", "animation_str")
}

// entrypoint
var s = reader.ReadSection("skeleton")
ReadState(s)
