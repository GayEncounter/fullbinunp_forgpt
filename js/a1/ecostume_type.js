var i = 0, arr = reader.ReadArray("ecostume_type")
while(arr.More())
{
	var rec = arr.ReadSection(RecStr("rec_", i++, 4), false)
	rec.ReadString("name")
	rec.ReadU32("id")
}