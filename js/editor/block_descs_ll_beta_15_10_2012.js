function GetBlockDesc(clsid, data)
{
	print("GetBlockDesc ", clsid)

	if(clsid == "logic/switch_param")
	{
		var quant = data.GetInt("quant", "u8")
		
		var desc = {
			in_names : ["activate", "reset"],
			out_names : []
		}
		
		for(var i = 0; i < quant; i++)
			desc.out_names.push("out-" + i)
			
		return desc
	}
	
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
		
		desc.out_names.push(
			"check dlc", 
			"target", 
			"customize start", 
			"customize finish", 
			"customize point", 
			"object browse", 
			"customize browse", 
			"context browse",
			"trade start",
			"trade finish"
		);
		
		return desc
	}
	
	return null
}

function NeedUpdateBlockDesc(clsid, name, vtype, data)
{
	if(clsid == "logic/switch_param")
	{
		if(name == "quant" && vtype == "u8")
			return true
	}
	
	if(clsid == "trade/trade trigger")
	{
		if((name == "objects_count" && vtype == "u32") || (name == "objects_count_pre" && vtype == "u32"))
		{
			if(name == "object_count")
				var new_cnt = data.GetInt("objects_count", "u32")
			else if(name == "objects_count_pre")
				var new_cnt = data.GetInt("objects_count_pre", "u32")

			var active = data.GetBool("active")
			var trade_type = data.GetInt("trade_type", "u8")
			var current_object = data.GetInt("current_object", "u32")
			var trade_slot = data.GetInt("trade_slot", "u32")
			var trade_preset = data.GetInt("trade_preset", "u32")
			var need_attach = data.GetBool("need_attach")
			
			var objects = new Array	
			for(var i = 1; i <= new_cnt; i++)
			{
				var obj = new Object
				obj.link = data.GetInt("object_"+i+"_link", "entity_link, uobject_link")
				obj.price_link = data.GetInt("object_"+i+"_price_link", "entity_link, uobject_link")
				obj.info_link = data.GetInt("object_"+i+"_info_link", "entity_link, uobject_link")
				obj.needs_dlc = data.GetInt("object_"+i+"_needs_dlc", "u32")				
				objects.push(obj);
			}
			
			var custom_price = data.GetInt("custom price", "entity_link, uobject_link")
			var custom_desc = data.GetInt("custom desc", "entity_link, uobject_link")
			var charity_mode = data.GetBool("charity_mode")
		
			data.Clear()
			data.AddBool("active", active)/
			data.AddInt("trade_type", trade_type, "u8")
			data.AddInt("current_object", current_object, "u32")
			data.AddInt("objects_count_pre", new_cnt, "u32")
			data.AddInt("trade_slot", trade_slot, "u32")
			data.AddInt("trade_preset", trade_preset, "u32")
			data.AddBool("need_attach", need_attach)

			for(var i = 1; i <= new_cnt; i++)
			{
				data.AddInt("object_"+i+"_link", objects[i-1].link || 65535, "entity_link, uobject_link")
				data.AddInt("object_"+i+"_price_link", objects[i-1].price_link || 65535, "entity_link, uobject_link")
				data.AddInt("object_"+i+"_info_link", objects[i-1].info_link || 65535, "entity_link, uobject_link")
				data.AddInt("object_"+i+"_needs_dlc", objects[i-1].needs_dlc || 0, "u32")
			}
			
			data.AddInt("objects_count", new_cnt, "u32")
			data.AddInt("custom price", custom_price, "entity_link, uobject_link")
			data.AddInt("custom desc", custom_desc, "entity_link, uobject_link")
			data.AddBool("charity_mode", charity_mode)
			
			return true
		}
	}
		
	return false
}