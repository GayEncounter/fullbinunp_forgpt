var skl = reader.ReadSection("skeleton")

skl.ReadU32("ver")
skl.ReadU32("crc")
skl.ReadString("facefx")
skl.ReadString("motions")

var bones = skl.ReadArray("bones")
for(var i = 0; i < bones.count; i++)
{
	var b = bones.ReadSection(RecStr("rec_", i, 4), false)
	b.ReadString("name")
	b.ReadString("parent")
	b.ReadVec4("q")
	b.ReadVec3("t")
	b.ReadU16("bp")
}