// для перегонки файлов из anims.bin в LUA

function ReadTrack(e, b_motions)
{
	var arr = e.ReadArray("envelopes")
	while(arr.MoreElements())
	{
		var elem = arr.NextElement()
		elem.ReadU8("b0")
		elem.ReadU8("b1")
		var arr2 = elem.ReadArray("keys")
		while(arr2.MoreElements())
		{
			var key = arr2.NextElement()
			key.ReadFP32("value")
			key.ReadFP32("time")
			key.ReadU8("shape")
			key.ReadVec4("params")
		}
	}
	if(b_motions)
	{
		var arr = e.ReadArray("motions")
		while(arr.MoreElements())
		{
			var elem = arr.NextElement()
			elem.ReadHintStr("name", "animation_str")
			elem.ReadFP32("time")
			elem.ReadU8("channel")
		}
	}
	var arr = e.ReadArray("tags")
	while(arr.MoreElements())
	{
		var elem = arr.NextElement()
		elem.ReadString("tag")
		elem.ReadFP32("time")
		elem.ReadHint("flags", "flags32")
		elem.ReadU32("flags")
	}
	e.ReadU32("flags")
	var arr = e.ReadArray("sounds")
	while(arr.MoreElements())
	{
		var elem = arr.NextElement()
		elem.ReadU8("b0")
		elem.ReadU8("b1")
		var arr2 = elem.ReadArray("keys")
		while(arr2.MoreElements())
		{
			var key = arr2.NextElement()
			key.ReadFP32("value")
			key.ReadFP32("time")
			key.ReadU8("shape")
			key.ReadVec4("params")
		}
		elem.ReadBool8("flags0", ["sound_slowmo_enabled", "play_as_music", "stop_at_end", "stop_at_interrupt", "check_gasmask"])
		elem.ReadString("name")
		elem.ReadFP32("time")
		elem.ReadFP32("wave_begin")
		elem.ReadFP32("wave_length")
		elem.ReadFP32("sound_end")
		elem.ReadU8("channel")
		elem.ReadU32("sound_type")
	}
	var arr = e.ReadArray("particles")
	while(arr.MoreElements())
	{
		var elem = arr.NextElement()
		elem.ReadHintStr("name", "choose")
		elem.ReadFP32("time")
		elem.ReadU8("channel")
		elem.ReadU8("type")
		elem.ReadBool8("particles_flags", ["deferred", "wipe", "constr", "ignore_fov", "allow_post_effects"])
	}
}

var ver = reader.ReadU8("version")
if(ver != 28)
	print("camera_track: version is not 28")
reader.ReadFP32("time_start")
reader.ReadFP32("time_end")
reader.ReadFP32("FPS")
reader.ReadU16("flags")
var has_camera = reader.ReadBool("has_camera")
if(has_camera)
{
	var track = reader.ReadSection("camera_track")
	ReadTrack(track)
}
var arr = reader.ReadArray("models")
while(arr.MoreElements())
{
	var elem = arr.NextElement()
	elem.ReadString("caption")
	ReadTrack(elem, true)
}