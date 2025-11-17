var not_implemented_actions = {}

function ReadDomain(e, name, edit_type)
{
	var d = e.ReadSection(name)
	var type = d.ReadU16("domain_type")
	
	function ReadVec(name)
	{
		switch(edit_type)
		{
			case 0: d.ReadVec3(name); break;
			case 1: d.ReadVec3(name, "ang3f"); break;
			case 2: d.ReadVec4(name, "color, vec4f"); break;
			default: d.ReadVec3(name);
		}
	}
	
	switch(type)
	{
		case 0: // point
			ReadVec("p0")
		break;
		
		case 1: // line
			ReadVec("e0")
			ReadVec("e1")
		break;
		
		case 2: // triangle
			ReadVec("p0")
			ReadVec("p1")
			ReadVec("p2")
		break;
		
		case 3: // plane
			ReadVec("p0")
			ReadVec("normal")
		break;
		
		case 4: // box
			ReadVec("e0")
			ReadVec("e1")
		break;
		
		case 5: // sphere
			ReadVec("center")
			d.ReadFP32("inner_radius")
			d.ReadFP32("outer_radius")
		break;
		
		case 6: // cyllinder	
		case 7: // cone
			ReadVec("e0")
			ReadVec("e1")
			d.ReadFP32("inner_radius")
			d.ReadFP32("outer_radius")
		break;
		
		case 8: // blob
			ReadVec("center")
			d.ReadFP32("standard_dev")
		break;
		
		case 9: // disk
			ReadVec("center")
			ReadVec("normal")
			d.ReadFP32("inner_radius")
			d.ReadFP32("outer_radius")
		break;
		
		case 10: // rectangle
			ReadVec("p0")
			ReadVec("u0")
			ReadVec("v0")
		break;
	}
}

function ReadActionList(effect)
{
	var acount = effect.ReadU16("action_count")
	for(var k = 0; k < acount; k++)
	{
		var a = effect.ReadSection(RecStr("action_", k, 4))
		
		var atype = a.ReadU16("type")
		a.ReadBool("enabled")
		var adesc = a.ReadString("description")
		
		switch(atype)
		{
			case 0: // Avoid
				a.ReadFP32("magnitude")
				a.ReadFP32("epsilon")
				a.ReadFP32("look_ahead")
				ReadDomain(a, "domain", 0)	
				a.ReadBool("dbg_draw")	
			break;
			
			case 1: // Bounce
				a.ReadFP32("friction")
				a.ReadFP32("resilience")
				a.ReadFP32("cutoff")
				ReadDomain(a, "domain", 0)	
				a.ReadBool("dbg_draw")	
			break;
			
			case 3: // Damping
				a.ReadVec3("damping")
				a.ReadFP32("vlow")
				a.ReadFP32("vhigh")
				a.ReadBool("ignore_rotation")
			break;
			
			case 4: // Rotation Damping
				a.ReadVec3("damping")
				a.ReadFP32("vlow", "angle, fp32")
				a.ReadFP32("vhigh", "angle, fp32")				
			break;
			
			case 5: // Follow
				a.ReadFP32("magnitude")
				a.ReadFP32("epsilon")
				a.ReadFP32("max_radius")
			break;
			
			case 6: // Gravitate
				a.ReadFP32("magnitude")
				a.ReadFP32("epsilon")
				a.ReadFP32("max_radius")
			break;
			
			case 7: // Gravity
				a.ReadVec3("dir")
				a.ReadBool("ignore_rotation")
			break;
			
			case 8: // Jet
				ReadDomain(a, "dom", 0)
				ReadDomain(a, "acc", 0)
				a.ReadBool("dbg_draw")
			break;
			
			case 13: // Orgit Line
				a.ReadVec3("p")
				a.ReadVec3("axis")
				a.ReadFP32("magnitude")
				a.ReadFP32("epsilon")
				a.ReadFP32("max_radius")
				a.ReadBool("dbg_draw")
			break;
			
			case 14: // Orbit Point
				a.ReadVec3("center")
				a.ReadFP32("magnitude")
				a.ReadFP32("epsilon")
				a.ReadFP32("max_radius")
				a.ReadBool("dbg_draw")
			break;
			
			case 15: // Random Accel
			case 16: // Random Displace
			case 17: // Random Velocity
			case 18: // Random Rotation Velocity
				ReadDomain(a, "dom", 0)
			break;
			
			case 20: // Sink
				a.ReadBool("kill_inside")
				ReadDomain(a, "dom", 0)
				a.ReadBool("dbg_draw")
			break;
			
			case 21: // Sink Velocity
				a.ReadBool("kill_inside")
				ReadDomain(a, "dom", 0)				
			break;
			
			case 22: // Source
				a.ReadFP32("particle_rate")
				a.ReadFP32("spawn_limit")
				a.ReadFP32("life_time_sigma")
				ReadDomain(a, "position_dom", 0)
				ReadDomain(a, "color_cdom", 2)
				ReadDomain(a, "color_adom", 0)
				ReadDomain(a, "size_dom", 0)
				ReadDomain(a, "rot_velocity_dom", 1)
				ReadDomain(a, "rotation_dom", 1)
				a.ReadBool("vel_ignore_parent_root")
				ReadDomain(a, "velocity_dom", 0)
				a.ReadBool("dbg_draw")
			break;
			
			case 23: // Speed Limit
				a.ReadFP32("min_speed")
				a.ReadFP32("max_speed")
			break;
			
			case 24: // Target Color
				a.ReadVec4("color", "color, vec4f")
				a.ReadFP32("scale")
			break;
			
			case 25: // Target Size
				a.ReadVec2("size")
				a.ReadVec2("scale")
			break;
			
			case 26: // Target Rotate
				
			break;			
			
			case 27: // Target Velocity
				a.ReadVec3("vel")
				a.ReadFP32("scale")	
				a.ReadBool("ignore_rotation")
			break;
			
			case 28: // Target Rotation Velocity
				a.ReadVec3("rvel", "ang3f")
				a.ReadFP32("scale")
			break;
			
			case 29: // Vortex
				a.ReadVec3("center")
				a.ReadVec3("axis")
				a.ReadFP32("tightness_exponent")
				a.ReadFP32("max_radius")
				a.ReadFP32("in_speed")
				a.ReadFP32("up_speed")
				a.ReadFP32("around_speed")
				a.ReadBool("dbg_draw")
			break;
			
			case 50: // Collision-
				a.ReadFP32("friction")
				a.ReadFP32("resilience")
				a.ReadFP32("cutoff")
				a.ReadBool("kill_at_collide")
				a.ReadBool("allow_dynamic")
			break;
			
			case 51: // Scatter
				a.ReadVec3("center")
				a.ReadFP32("magnitude")
				a.ReadFP32("epsilon")
				a.ReadFP32("max_radius")
				a.ReadBool("dbg_draw")				
			break;
			
			case 52: // Turbulence
				a.ReadFP32("frequency")
				a.ReadU16("octaves")
				a.ReadFP32("magnitude")
				a.ReadFP32("epsilon")
				a.ReadVec3("offset")
				a.ReadBool("dbg_draw")
			break;
			
			case 53: // Color Animator
			case 54: // Size Animator
				a.ReadHintStr("animator", "ref_coloranim")
				a.ReadBool("looped")
			break;
			
			case 55: // Velocity Animator
				a.ReadHintStr("animator", "ref_coloranim")
				a.ReadBool("looped")
				a.ReadBool("ignore_rotation")
			break;
						
			case 56: // Gradient Track
				a.ReadVec3("p0")
				a.ReadVec3("p1")
				a.ReadHintStr("color_track", "ref_coloranim")
				a.ReadHintStr("size_track", "ref_coloranim")
				a.ReadHintStr("vel_track", "ref_coloranim")
				a.ReadBool("tiled")
				a.ReadBool("ignore_rotation")
				a.ReadBool("dbg_draw")
			break;
			
			default:
				if(not_implemented_actions[atype] != true)
				{
					print("not implemented action '", adesc, "' type ", atype)
					not_implemented_actions[atype] = true
				}
		}
		
		if(a.More())
			print(adesc, " data left")
	}
}

var physx_type_preset = reader.ReadSection("physx_type_preset")
var physx_type_preset_names = ["Light Gases", "Rigid Bodies", "Liquids", "Heavy Gases", "Viscous Liquids"]

while(physx_type_preset_names.length)
{
	var preset = physx_type_preset.ReadSection(physx_type_preset_names.shift())
	
	preset.ReadBool("self_collided")
	preset.ReadFP32("damping")
	preset.ReadFP32("friction")
	preset.ReadFP32("restitution")
	preset.ReadBool("apply_physx_gravity")
	preset.ReadBool("correct_moving")
	preset.ReadFP32("stiffness")
	preset.ReadFP32("viscosity")
	preset.ReadFP32("density")
}

var effects = reader.ReadArray("effects")
for(var i = 0; effects.More(); i++)
{
	var effect = effects.ReadSection(RecStr("rec_", i, 4), false)
	var version
	
	effect.ReadString("name")
	version = effect.ReadU16("version")
	effect.ReadFP32("initial_radius")
	effect.ReadFP32("time_limit")
	effect.ReadU16("max_particles")
	effect.ReadU32("warmup_time")
	effect.ReadFP32("particles_life_time")
	effect.ReadFP32("particle_max_size")
	effect.ReadBool("fl_force_aabb")
	effect.ReadVec3("force_aabb_mn")
	effect.ReadVec3("force_aabb_mx")
	effect.ReadHintStr("rate_anim", "ref_coloranim")
	effect.ReadBool("fl_physx_enabled")
	effect.ReadBool("fl_physx_anim_enabled")
	effect.ReadU8("physx_type")
	
	if(version != 1) 
		effect.ReadU8("fl_sort")
	else             
		effect.ReadBool("fl_sort")
		
	if(version > 4)  
		effect.ReadU8("fl_velocity_scale")
	else             
		effect.ReadBool("fl_velocity_scale")
	
	if(version > 2)
	{
		effect.ReadU8("fl_align_type")
	}
	else
	{
		effect.ReadBool("fl_align_to_motion")
		effect.ReadBool("fl_face_align")
	}
	
	effect.ReadBool("fl_framed")
	effect.ReadBool("fl_animated")
	effect.ReadBool("fl_random_frame")
	effect.ReadBool("fl_random_playback")
	effect.ReadBool("fl_random_start_frame")
	effect.ReadHintStr("texture_name", "texture, str_shared")
	effect.ReadHintStr("shader_name", "shader, str_shared")
	effect.ReadBool("fl_uv_flip_u")
	effect.ReadBool("fl_uv_flip_v")
	effect.ReadVec2("frame_size")
	effect.ReadU8("frame_wcount_p2")
	effect.ReadU16("frame_count")
	effect.ReadHint("frame_num", "tex_frame, u_vector<u16>")
	effect.ReadU16Array16("frame_num")
	effect.ReadFP32("frame_speed")
	effect.ReadVec3("velocity_scale")
	effect.ReadBool("fl_still_rot_local")
	effect.ReadVec2("still_rotation", "ang2f")
	effect.ReadFP32("color_scale")
	effect.ReadBool("fl_use_parent_color")
	
	if(version >= 4)
		effect.ReadVec3("motion_factor")
	else
		effect.ReadFP32("motion_factor")
	
	ReadActionList(effect)
}

var modifs = reader.ReadArray("modifs")
for(var i = 0; modifs.More(); i++)
{
	var modif = modifs.ReadSection(RecStr("rec_", i, 4), false)
	modif.ReadString("name")
	ReadActionList(modif)
}

var systems = reader.ReadArray("systems")
for(var i = 0; systems.More(); i++)
{
	var system = systems.ReadSection(RecStr("rec_", i, 4), false)
	
	system.ReadString("name")
	system.ReadFP32("time_limit")
	system.ReadHint("flags", "flags32")
	system.ReadU32("flags")
	system.ReadHintStr("physx_alias", "choose")
	
	var items = system.ReadArray("items")
	for(var j = 0; items.More(); j++)
	{
		var item = items.ReadSection(RecStr("item_", j, 4), false)
		
		item.ReadHintStr("main_effect_name", "particles_effect, str_shared")
		item.ReadFP32("probability")
		item.ReadFP32("timetable_1")
		item.ReadFP32("timetable_2")
		item.ReadFP32("freeze_time_1")
		item.ReadFP32("freeze_time_2")
		item.ReadHintStr("on_playing_effect_name", "particles, str_shared")
		item.ReadHintStr("on_birth_effect_name", "particles, str_shared")
		item.ReadHintStr("on_death_effect_name", "particles, str_shared")
		item.ReadU8("flags", "bool8") // fl_enabled, fl_deffered_stop, fl_main_allow_modif, fl_on_playing_allow_modif, fl_on_birth_allow_modif, fl_on_death_allow_modif
	}
}