// для распаковки файлов в папке foliage

var colors = reader.ReadArray("colors");
for(var i = 0; i < colors.count; i++)
{
	var color = colors.ReadSection(RecStr("color_", i, 2), false);
	color.ReadVec4("color", "color, vec4f");
	color.ReadString("name")
	var objects = color.ReadArray("objects");
	for(var j = 0; j < objects.count; j++)
	{
		var object = objects.ReadSection(RecStr("object_", j, 2), false);
		object.ReadHintStr("model_name", "choose");
		object.ReadFP32("density");
		object.ReadFP32("min_scale");
		object.ReadFP32("max_scale");
		object.ReadBool("enabled");
		object.ReadU32("flags");
	}
}