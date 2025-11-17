var flares = reader.ReadArray("flares");
while(flares.More())
{
	var flare = flares.ReadSection();

	flare.ReadName("name");
	flare.ReadBool("source");
	flare.ReadHint("source_shader", "shader, str_shared");
	flare.ReadString("source_shader");
	flare.ReadHint("source_texture", "texture, str_shared");
	flare.ReadString("source_texture");
	flare.ReadFP32("source_radius");
	flare.ReadBool("source_ignore_color");
	flare.ReadBool("flares");
	flare.ReadHint("flares_shader", "shader, str_shared");
	flare.ReadString("flares_shader");
	for(var i = 0; i < 6; i++)
	{
		var d = flare.ReadSection("desc_"+i);
		d.ReadFP32("flare_radius");
		d.ReadFP32("flare_opacity");
		d.ReadFP32("flare_position");
		d.ReadHint("flare_texture", "texture, str_shared");
		d.ReadString("flare_texture");
	}
	flare.ReadFP32("blend_rise_time");
	flare.ReadFP32("blend_down_time");
}

var l = reader.ReadSection("lightning");

l.ReadHint("top_texture", "texture, str_shared");
l.ReadString("top_texture");
l.ReadVec2("top_radius");
l.ReadFP32("top_opacity");
l.ReadHint("center_texture", "texture, str_shared");
l.ReadString("center_texture");
l.ReadVec2("center_radius");
l.ReadFP32("center_opacity");
l.ReadFP32("fog_factor");
l.ReadFP32("sky_factor");
l.ReadFP32("sun_factor");
l.ReadFP32("hemi_factor");

var descs = reader.ReadArray("descriptions");
while(descs.More())
{
	var desc = descs.ReadSection();

	desc.ReadName("name");
	desc.ReadHint("sky_texture", "texture, str_shared");
	desc.ReadString("sky_texture");
	desc.ReadVec4("sky_color", "color, vec4f");
	desc.ReadHint("clouds_texture", "texture, str_shared");
	desc.ReadString("clouds_texture");
	desc.ReadVec4("clouds_color", "color, vec4f");
	desc.ReadFP32("fog_distance");
	desc.ReadVec4("fog_color", "color, vec4f");
	desc.ReadFP32("fog_density");
	desc.ReadFP32("wind_direction", "angle, fp32");
	desc.ReadFP32("wind_velocity");
	desc.ReadVec4("ambient", "color, vec4f");
	desc.ReadVec4("hemi_color", "color, vec4f");
	desc.ReadVec4("sun_color", "color, vec4f");
	desc.ReadVec2("sun_dir");
	desc.ReadFP32("sun_shadowrange");
	desc.ReadFP32("lum_scale");
	desc.ReadHint("flares", "choose");
	desc.ReadString("flares");
	desc.ReadVec2("flares_dir");
	desc.ReadVec4("flares_color", "color, vec4f");
	desc.ReadFP32("lshafts_intensity");
	desc.ReadVec4("lshafts_color", "color, vec4f");
}
	