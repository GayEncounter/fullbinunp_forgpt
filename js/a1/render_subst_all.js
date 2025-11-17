function RecStr(pref, n, digits)
{
	var ns = n.toString();
	var ret = pref;

	for(var z = ns.length; z < digits; z++)
		ret += '0';

	return ret + ns; 
}

var j = 0;
var arr = reader.ReadArray("shader_subst_normal")
while(arr.More())
{
	var elem = arr.ReadSection(RecStr("rec_", j++, 4), false);
	elem.ReadString("name");
	elem.ReadString("value");
}

var j = 0;
var arr = reader.ReadArray("shader_subst_obsolette")
while(arr.More())
{
	var elem = arr.ReadSection(RecStr("rec_", j++, 4), false);
	elem.ReadString("name");
	elem.ReadString("value");
}