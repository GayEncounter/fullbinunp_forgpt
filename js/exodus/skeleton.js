var s = reader.ReadSection("skeleton")

var version = s.ReadU32("ver")
s.ReadU32("crc")

if(version < 15) s.ReadString("facefx")
if(version >= 17) s.ReadString("pfnn")
if(version >= 21) s.ReadBool("has_as")
s.ReadString("motions")
if(version >= 13) s.ReadString("source_info")

if(version >= 14) 
{
	s.ReadString("parent_skeleton")
	
	var pbm = s.ReadArray("parent_bone_maps")
	while(pbm.MoreElements())
	{
		var rec = pbm.NextElement()
		rec.ReadString("parent_bone")
		rec.ReadString("self_bone")
		rec.ReadVec4("q")
		rec.ReadVec3("t")
		rec.ReadVec3("s")
	}
}

var bones = s.ReadArray("bones")
while(bones.MoreElements())
{
	var b = bones.NextElement()
	b.ReadString("name")
	b.ReadString("parent")
	b.ReadVec4("q")
	b.ReadVec3("t")
	if(version > 18)
	{
		b.ReadU8("bp")
		b.ReadU8("bpf")
	}
	else
		b.ReadU16("bp")
}

var locators = s.ReadArray("locators")
while(locators.MoreElements())
{
	var l = locators.NextElement()
	l.ReadString("name")
	l.ReadString("parent")
	l.ReadVec4("q")
	l.ReadVec3("t")
	//l.ReadU8("fl", "bool8") // skip_fx, editor_type, quat_calc (since 0x11 ver)
	
	// is order correct ?
	var locator_flags = ["skip_fx", "editor_type"];
	if(version >= 17)
		locator_flags.push("quat_calc");
		
	l.ReadBool8("fl", locator_flags)
}

if(version >= 6)
{
	var aux_bones = s.ReadArray("aux_bones")
	while(aux_bones.MoreElements())
	{
		var ab = aux_bones.NextElement()
		ab.ReadString("name")
		ab.ReadString("parent")
		ab.ReadVec4("q")
		ab.ReadVec3("t")
		//ab.ReadU8("fl", "bool8")
		
		ab.ReadBool8("fl", ["editor_type"])
	}
}

if(version >= 11)
{
	var p = s.ReadSection("procedural")
	var procedural_ver = p.ReadU32("ver")
	
	var pba = p.ReadArray("procedural_bones")
	while(pba.MoreElements())
	{
		var pb = pba.NextElement()
		pb.ReadU16("type")
		pb.ReadU16("index_in_array")
	}
}
else
{
	var p = s
	var procedural_ver = 0
}
	
if(version >= 7)
{
	var driven_bones = p.ReadArray("driven_bones")
	while(driven_bones.MoreElements())
	{
		var db = driven_bones.NextElement()
		db.ReadHintStr("bone", "choose")
		db.ReadHintStr("driver", "choose")
		db.ReadHintStr("driver_parent", "choose")
		db.ReadU8("component")
		db.ReadHintStr("twister", "choose")
		db.ReadFP32("value_min")
		db.ReadFP32("value_max")
		if(procedural_ver >= 1) db.ReadU8("refresh_kids")
		if(procedural_ver >= 5) db.ReadBool("use_anim_poses")
	}
}

if(version >= 8)
{
	var dynamic_bones = p.ReadArray("dynamic_bones")
	while(dynamic_bones.MoreElements())
	{
		var db = dynamic_bones.NextElement()
		db.ReadHintStr("bone", "choose")
		db.ReadFP32("inertia")
		db.ReadFP32("damping")
		
		if(procedural_ver < 9)
		{
			db.ReadVec3("constraints")
			if(procedural_ver >= 6)
			{
				db.ReadVec3("rot_limits", "ang3f")
			}
		}
		else
		{
			db.ReadVec3("pos_min_limits")
			db.ReadVec3("pos_max_limits")
			db.ReadVec3("rot_min_limits", "ang3f")
			db.ReadVec3("rot_max_limits", "ang3f")
		}
		
		if(procedural_ver >= 4)
		{
			db.ReadBool("use_world_pos")
		}
	}
}

if(version >= 9)
{
	function foo(s, name)
	{
		var sect = s.ReadSection(name)
		sect.ReadU8("axis")
		sect.ReadHintStr("bone_names", "choose_array, str_shared")
		var count = sect.ReadU32("bone_strs_size")
		for(var i = 0; i < count; i++)
		{
			sect.ReadString("bone"+i)
			sect.ReadFP32("weight"+i)
		}
	}
	
	var constrained_bones = p.ReadArray("constrained_bones")
	while(constrained_bones.MoreElements())
	{
		var cb = constrained_bones.NextElement()
		cb.ReadHintStr("bone", "choose")
		
		if(version < 10)
		{
			cb.ReadU8("axis")
		}
		else
		{
			cb.ReadU8("look_at_axis")
			cb.ReadU8("pos_axis")
			cb.ReadU8("rot_axis")
		}
		
		if(procedural_ver >= 3)
			cb.ReadU8("rotation_order")
			
		foo(cb, "position")
		foo(cb, "orientation")
		if(procedural_ver >= 1) cb.ReadU8("refresh_kids")
		if(procedural_ver >= 5)
		{
			cb.ReadBool("use_anim_poses")
			if(procedural_ver < 8)
			{
				cb.ReadVec3("pos_limits")
				cb.ReadVec3("rot_limits", "ang3f")
			}
			else
			{
				cb.ReadVec3("pos_min_limits")
				cb.ReadVec3("pos_max_limits")
				cb.ReadVec3("rot_min_limits", "ang3f")
				cb.ReadVec3("rot_max_limits", "ang3f")				
			}
			
			cb.ReadU8("uptype")
			foo(cb, "up")
		}
	}
}

if(version >= 20)
{
	var param_bones = p.ReadArray("param_bones")
	while(param_bones.MoreElements())
	{
		var pb = param_bones.NextElement()
		pb.ReadHintStr("bone", "choose")
		pb.ReadHintStr("parent", "choose")
		pb.ReadHintStr("param", "choose")
		pb.ReadU8("component")
	}
}

var partitions = s.ReadArray("partitions")
while(partitions.MoreElements())
{
	var part = partitions.NextElement()
	part.ReadString("name")
	part.ReadU8Array("infl")
}

var ik_chains = s.ReadArray("ik_chains")
while(ik_chains.MoreElements())
{
	var chain = ik_chains.NextElement()
	chain.ReadString("name")
	if(version < 12) 
	{
		chain.ReadU16("b0")
		chain.ReadU16("b1")
		chain.ReadU16("b2")
		chain.ReadVec3("knee_dir")
	}
	else
	{
		chain.ReadU16("upper_limb_bone")
		chain.ReadU16("lower_limb_bone")
		chain.ReadVec3("knee_dir")
		chain.ReadFP32("max_length")
		chain.ReadHint("flags", "flags32")
		chain.ReadU32("flags") // in LUA written as integer, so don't care for names
		chain.ReadU16("ground_locator")
		chain.ReadFP32("knee_lim")	
	}
}

var fixed_bones = s.ReadArray("fixed_bones")
while(fixed_bones.MoreElements()) 
{
	var fb = fixed_bones.NextElement()
	fb.ReadU16("id")
}

var params = s.ReadArray("params")
while(params.MoreElements()) 
{
	var param = params.NextElement()
	param.ReadString("name")
	param.ReadFP32("b")
	param.ReadFP32("e")
	param.ReadFP32("loop")
}

var mcolls = s.ReadArray("mcolls")
while(mcolls.MoreElements()) 
{
	var mcoll = mcolls.NextElement()
	mcoll.ReadString("name")
	mcoll.ReadString("path")
	
	var mots = mcoll.ReadArray("mots")
	while(mots.MoreElements())
	{
		var mot = mots.NextElement()
		mot.ReadString("m")
		mot.ReadFP32("w")
	}
}
