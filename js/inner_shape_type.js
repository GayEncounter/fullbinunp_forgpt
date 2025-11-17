// 2033, Last Light, Redux, Arktika.1, Exodus, all the same

var arr = reader.ReadArray("inner_shape_type")
while(arr.MoreElements())
{
	var elem = arr.NextElement()
	elem.ReadString("name")
	elem.ReadU32("id")
	elem.ReadFP32("radius")
}