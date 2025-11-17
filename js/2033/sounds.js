var presets = reader.ReadSection("sound_source_type_preset")
var preset_names = [
	"Default", 
	"Ambient Very Loud", 
	"Ambient Loud", 
	"Ambient Medium", 
	"Ambient Quiet", 
	"Ambient Very Quiet", 
	"Hit Huge", 
	"Hit Large", 
	"Hit Medium", 
	"Hit Small", 
	"Gunshot Large", 
	"Gunshot Medium", 
	"Gunshot Small", 
	"Gunshot Silent", 
	"Weapon Reload", 
	"Explosion Large", 
	"Explosion Medium", 
	"Explosion Small", 
	"Footsteps Human Run", 
	"Footsteps Human Walk", 
	"Footsteps Human Crouch", 
	"Footsteps Monster Large", 
	"Footsteps Monster Small", 
	"Voice Human Shout", 
	"Voice Human Loud", 
	"Voice Human Medium", 
	"Voice Human Whisper", 
	"Voice Monster Loud", 
	"Voice Monster Medium", 
	"Voice Monster Quiet"
]
for(var i = 0; i < preset_names.length; i++)
{
	var e = presets.ReadSection(preset_names[i])
	e.ReadFP32("spl")
	e.ReadFP32("min_lpf_distance")
	e.ReadFP32("max_lpf_distance")
	e.ReadFP32("lpf_power")
	e.ReadFP32("min_hpf_distance")
	e.ReadFP32("max_hpf_distance")
	e.ReadFP32("hpf_power")
	e.ReadFP32("reverb_level")
	e.ReadBool("oneshot_ignore")
}

var params = reader.ReadArray("sound_params")
while(params.More())
{
	var e = params.ReadSection()
	var type = e.ReadU32("type")
	var name = e.ReadName("name")
	
	//print(type, " ", name)
	
	switch(type)
	{
		case 0:
			var loc = e.ReadBool("localizable")
			var sb = e.ReadBool("sub_titles")
			var tp = e.ReadU32("type_preset")
			if(loc)
			{
				var langs = e.ReadArray("langs")
				//print(loc, " ", sb, " ", tp, " ", !!langs)
				while(langs.More())
				{
					var l = langs.ReadSection() // TODO section name
					l.ReadString("lang")
					l.ReadU32("file_size")
					l.ReadU32("bytes_total")
					l.ReadFP32("db_rms_peak")
					l.ReadFP32("db_rms_begin")
					l.ReadU8("num_channels")
				}
			}
			else
			{
				e.ReadU32("file_size")
				e.ReadU32("bytes_total")
				e.ReadFP32("db_rms_peak")
				e.ReadFP32("db_rms_begin")
				e.ReadU8("num_channels")
			}
		break;
		
		case 1:
			var arr = e.ReadArray("items")
			for(var u = 0; arr.More(); u++)
			{
				var elem = arr.ReadSection(RecStr("item_", u, 4), false)
				elem.ReadString("handle")
				elem.ReadFP32("probability")
			}
		break;
		
		case 2:
		break;
		
		case 3:
			var arr = e.ReadArray("items")
			for(var u = 0; arr.More(); u++)
			{
				var elem = arr.ReadSection(RecStr("item_", u, 4), false)
				elem.ReadString("name")
				elem.ReadString("folder")
			}
		break;
		
		case 4:
			e.ReadString("voice_set")
			e.ReadString("brief_name")
		break;
	}
	
//	if(e.More())
//		print(name)
}

var masks = reader.ReadSection("sound_masks")
var mask_count = masks.ReadU32("mask_count")
for(var i = 0; i < mask_count; i++)
{
	masks.ReadU32("mask")
	masks.ReadU32Array("items")
}

var schemes = reader.ReadArray("sound_schemes")
while(schemes.More())
{
	var e = schemes.ReadSection()
	e.ReadName("name")
	e.ReadHint("array with no key", "array")
	var cnt = e.ReadU32("count")
	for(var i = 0; i < cnt; i++)
	{
		var s = e.ReadSection()
		s.ReadName("name")
		s.ReadBool("enabled")
		s.ReadBool("force_out")
		s.ReadHintStr("sound", "choose")
		s.ReadU32("priority")
		s.ReadU32("mask")
		s.ReadU32("game_type")
		s.ReadFP32("min_start_time")
		s.ReadFP32("max_start_time")
		s.ReadFP32("min_stop_time")
		s.ReadFP32("max_stop_time")
	}
}

var effects = reader.ReadArray("sound_effects")
while(effects.More())
{
	var e = effects.ReadSection()
	e.ReadString("name")
	e.ReadFP32("wet_dry_mix")
	e.ReadU32("reflections_delay")
	e.ReadU8("reverb_delay")
	e.ReadU8("rear_delay")
	e.ReadU8("position_left")
	e.ReadU8("position_right")
	e.ReadU8("position_matrix_left")
	e.ReadU8("position_matrix_right")
	e.ReadU8("early_diffusion")
	e.ReadU8("late_diffusion")
	e.ReadU8("low_eq_gain")
	e.ReadU8("low_eq_cutoff")
	e.ReadU8("high_eq_gain")
	e.ReadU8("high_eq_cutoff")
	e.ReadFP32("room_filter_freq")
	e.ReadFP32("room_filter_main")
	e.ReadFP32("room_filter_hf")
	e.ReadFP32("reflections_gain")
	e.ReadFP32("reverb_gain")
	e.ReadFP32("decay_time")
	e.ReadFP32("density")
	e.ReadFP32("room_size")
}