// 2033

var arr = reader.ReadArray("cover_inner_type")
while(arr.MoreElements())
{
	var elem = arr.NextElement()
	elem.ReadU32("id")
	elem.ReadString("name")
}

var arr = reader.ReadArray("cover_type")
while(arr.MoreElements())
{
	var elem = arr.NextElement()
	elem.ReadU32("id")
	elem.ReadString("name")
	elem.ReadU32("type")
	elem.ReadVec3("aabb_min")
	elem.ReadVec3("aabb_max")
	elem.ReadU8("shape")
	elem.ReadU8("icon")
	elem.ReadHintStr("model", "ref_model")
}

var arr = reader.ReadArray("link_inner_type")
while(arr.MoreElements())
{
	var elem = arr.NextElement()
	elem.ReadU32("id")
	elem.ReadU8("mask")
	elem.ReadU8("icon")
}
