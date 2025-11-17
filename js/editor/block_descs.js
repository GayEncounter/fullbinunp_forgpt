function GetBlockDesc(clsid, data)
{
	if(clsid == "trade/trade trigger")
	{
		var cnt = data.GetInt("objects_count", "u32")
		
		var desc = {
			in_names : ["activate", "deactivate"],
			out_names : ["nomoney", "success", "toomany", "finish", "the same", "no buy (for ammo)", "no sell (for ammo)"]
		}
		
		for(var i = 0; i < cnt; i++)
		{
			desc.out_names.push("Object " + (i+1) + " activate")
			desc.out_names.push("Object " + (i+1) + " deactivate")
		}
		
		desc.out_names.push("check dlc")
			
		return desc
	}
	
	return null
}

function NeedUpdateBlockDesc(clsid, name, vtype)
{
	return false
}