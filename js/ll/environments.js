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
	desc.ReadHintStr("sky_texture", "choose");
	desc.ReadHintStr("sky_hdr_light_texture", "choose");
	desc.ReadVec4("sky_color", "color, vec4f");
	desc.ReadFP32("sky_rotation", "angle, fp32");
	desc.ReadHintStr("clouds_texture", "texture, str_shared");
	desc.ReadVec4("clouds_color", "color, vec4f");
	desc.ReadFP32("clouds_scale");
	desc.ReadFP32("clouds_speed");
	desc.ReadFP32("fog_distance");
	desc.ReadVec4("fog_color", "color, vec4f");
	desc.ReadVec4("fog_color2", "color, vec4f");
	desc.ReadFP32("fog_density");
	desc.ReadFP32("fog_height");
	desc.ReadFP32("wind_direction", "angle, fp32");
	desc.ReadFP32("wind_speed");
	desc.ReadVec4("ambient", "color, vec4f");
	desc.ReadVec4("hemi_color", "color, vec4f");
	desc.ReadFP32("hemi_spec");
	desc.ReadFP32("ssr_factor");
	desc.ReadVec4("sun_color", "color, vec4f");
	desc.ReadVec2("sun_dir");
	desc.ReadFP32("sun_shadowrange");
	desc.ReadFP32("sun_curve");
	desc.ReadFP32("light_lod");
	desc.ReadFP32("lum_scale");
	desc.ReadFP32("lum_ambient_lights");
	desc.ReadHintStr("flares2", "choose");
	desc.ReadVec2("flares_dir");
	desc.ReadVec4("flares_color", "color, vec4f");
	desc.ReadFP32("lshafts_intensity");
	desc.ReadVec4("lshafts_color", "color, vec4f");
	desc.ReadHintStr("sun_texture", "texture, str_shared");
	desc.ReadVec4("sun_sprite_color", "color, vec4f");
	desc.ReadFP32("sun_sprite_radius");
}
	