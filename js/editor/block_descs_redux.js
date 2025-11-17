function GetBlockDesc(clsid, data)
{
	if(clsid == "trade/trade trigger")
	{
		var cnt = data.GetInt("objects_count", "u32")
		
		var desc = {
			in_names : ["activate", "deactivate", "Trade preset +", "Trade preset -"],
			out_names : ["nomoney", "success", "toomany", "finish", "the same", "no buy (for ammo)", "no sell (for ammo)"]
		}

		// engine only does it if([rdi+1528] > 0) 
		// was ist das?
		for(var i = 0; i < cnt; i++)
		{
			desc.in_names.push("Enable Object " + (i+1))
			desc.in_names.push("Disable Object " + (i+1))
		}		
		
		// engine only does it if([rdi+1528] > 0) 
		// was ist das?
		for(var i = 0; i < cnt; i++)
		{
			desc.out_names.push("Object " + (i+1) + " activate")
			desc.out_names.push("Object " + (i+1) + " deactivate")
		}
		
		desc.out_names.push("check dlc")
		desc.out_names.push("target")
		desc.out_names.push("customize start")
		desc.out_names.push("customize finish")
		desc.out_names.push("customize point")
		desc.out_names.push("object browse")
		desc.out_names.push("customize browse")
		desc.out_names.push("context browse")
		desc.out_names.push("trade start")
		desc.out_names.push("trade finish")
		desc.out_names.push("customize cat browse")

		// engine only does it if([rdi+1528] > 0) 
		// was ist das?
		for(var i = 0; i < cnt; i++)
		{
			desc.out_names.push("Object " + (i+1))
		}

		desc.out_names.push("customize price entity")
			
		return desc
	}
	
	return null
}

function NeedUpdateBlockDesc(clsid, name, vtype)
{
	return false
}