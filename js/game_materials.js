var common = reader.ReadSection("common")

common.ReadHintStr("marks_texture", "texture, str_shared")
common.ReadU16("marks_frame_width")
common.ReadU16("marks_frame_height")

var materials = reader.ReadArray("materials")
while(materials.More())
{
	var mtl = materials.ReadSection()
	
	mtl.ReadName("name")
}