var j = 0;
var arr = reader.ReadArray("render_subst_all")
while(arr.More())
{
	var elem = arr.ReadSection(RecStr("rec_", j++, 4), false);
	elem.ReadString("name");
	elem.ReadString("value");
}