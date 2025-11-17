// 2662 15 Oct 2012 & 2711 3 Dec 2012 & LL & Redux

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
	elem.ReadBool("g2g")
	elem.ReadU8("radius")
}

var arr = reader.ReadArray("link_inner_type")
while(arr.MoreElements())
{
	var elem = arr.NextElement()
	elem.ReadU32("id")
	elem.ReadU8("mask")
	elem.ReadU8("icon")
}
