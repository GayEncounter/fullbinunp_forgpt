// Exodus
// "name" now goes after "shape"
// removed "model"
// added "nav_mtl_types"

var arr = reader.ReadArray("_G.types.cover_inner_type")
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
	elem.ReadU32("type")
	elem.ReadVec3("aabb_min")
	elem.ReadVec3("aabb_max")
	elem.ReadU8("shape")
	elem.ReadString("name")
	elem.ReadU8("icon")
	elem.ReadBool("g2g")
	elem.ReadU8("radius")
	elem.ReadU64("nav_mtl_types")
}

var arr = reader.ReadArray("link_inner_type")
while(arr.MoreElements())
{
	var elem = arr.NextElement()
	elem.ReadU32("id")
	elem.ReadU8("mask")
	elem.ReadU8("icon")
}
