// 2033, Last Light, Redux, Arktika.1

var arr = reader.ReadArray("shape_type")
while(arr.MoreElements())
{
	var elem = arr.NextElement()
	elem.ReadString("name")
	elem.ReadU32("id")
	elem.ReadU32("inner_type")
}