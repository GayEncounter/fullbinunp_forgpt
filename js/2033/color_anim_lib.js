var anims = reader.ReadArray("color_anims");
while(anims.More())
{
	var anim = anims.ReadSection();

	anim.ReadName("name");
	anim.ReadU32("length");

	var k = 0, keys = anim.ReadArray("keys");
	while(keys.More())
	{
		var key = keys.ReadSection(RecStr("key_", k++, 4), false);

		key.ReadS32("time");
		key.ReadVec4("value", "color, vec4f");
	}
}