// Exodus
// Changed 'inner_type : u32' to 'radius : fp32'

var arr = reader.ReadArray("shape_type")
while(arr.MoreElements())
{
	var elem = arr.NextElement()
	elem.ReadString("name")
	elem.ReadU32("id")
	elem.ReadFP32("radius")
}