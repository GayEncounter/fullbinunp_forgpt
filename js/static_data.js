// это короче должен был быть универсальный скрипт дл€ декомпил€ции любого файла из папки static_data
// т.к. начина€ с ласт лайта используетс€ CRC32 вместо нормальных имЄн классов пон€ть какой файл дл€ чего используетс€ стало затруднительно
// плюс получаетс€ огромное количество повтор€ющегос€ кода если дл€ каждого класса создавать отдельный скрипт
// поэтому € решил создать универсальный скрипт который по имени файла сам определ€ет к какому классу этот файл относитс€ 
// плюс тут можно повторить "правильную" иерархию классов вместо дублироваи€ кода 

// недоделано

var class_readers = {
	"FILTER" : ReadInventoryItemObject
}

if(g_bin_file_name === undefined) {
	throw new Error(".bin file name is undefined")
}

var typed_strings = module("ll\\typed_strings")

var classcrc = g_bin_file_name.substring(0, g_bin_file_name.indexOf("_"))
var classname = typed_strings.get_class(parseHex(classcrc))

if(classname == "") {
	throw new Error("unknown class crc " + classcrc)
}

print(classname)

if(!class_readers[classname]) {
	throw new Error("not implemented class '" + classcrc + "'")
}

var version = reader.ReadU16("version") * 10
class_readers[classname](reader, version);

function ReadUObject(reader, version)
{
	// uobject::static_params::load
	var section1 = reader.ReadSection("__edit")
	section1.ReadString("caption")
	reader.ReadBool("editable")
	reader.ReadBool("visible_for_ai")
	reader.ReadBool("block_ai_los")
	reader.ReadBool("accept_fast_explosion")
	reader.ReadBool("collideable")
	if(version >= ENTITY_VER_29) {
		reader.ReadFP32("usage_distance")
	}
}

function ReadCEntity(reader, version)
{
	ReadUObject(reader, version)
	
	// centity::static_params::load
	reader.ReadString("collision_sound")
	reader.ReadHintStr("collision_track", "camera_track, str_shared")
	reader.ReadU32("collision_interval")
	reader.ReadFP32("collision_move")
	reader.ReadFP32("attach_threshold")
	reader.ReadFP32("attach_armor")
}

function ReadInventoryItemObject(reader, version)
{
	// inventory_item::static_params::load
	var section0 = reader.ReadSection("slot")
	//inventory_item::static_params::e_slot::load
	section0.ReadU32("slot")
	section0.ReadU8("priority")
	
	// inventory_item::static_params::load
	reader.ReadFP32("control_inertion_factor")
	reader.ReadFP32("speed_coef")
	reader.ReadFP32("sens_coef")
	reader.ReadU32("sprint2run_time")
	reader.ReadU32("run2sprint_time")
	if(version >= ENTITY_VER_29) {
		reader.ReadFP32("convergence")
		reader.ReadFP32("aimed_convergence")
	}
	reader.ReadHint("flags", "flags8")
	reader.ReadU8("flags")
	reader.ReadU32("slot_max_num")
	reader.ReadU32("animation_slot")
	reader.ReadU32("keepsakes_count")
	reader.ReadHintStr("attach_hud_loc", "locator_str");
	reader.ReadString("active_holder_attp")
	reader.ReadString("holder_attp")
	if(version >= ENTITY_VER_30) {
		reader.ReadString("holder_attp1")
		reader.ReadString("holder_attp2")
	}
	reader.ReadString("active_item_attp")
	reader.ReadString("item_attp")
	
	ReadCEntity(reader, version)
	
	// inventory_item_object::static_params::load
	reader.ReadHintStr("hr_class", "choose")
	reader.ReadString("using_action")
	reader.ReadString("using_action_replace")
	reader.ReadFP32("take_impulse")
	reader.ReadHintStr("take_sound", "sound")
	if(version >= ENTITY_VER_29) {
		reader.ReadBool("can_be_taken_as_child")
	}
	var section2 = reader.ReadSection("icon")
	
	// ui_item::load
	section2.ReadString("texture")
	if(version < ENTITY_VER_29) {
		// Modera: type changed from vec4f to vec4i in build 2711 (3 dec 2012)
		if(version >= ENTITY_VER_28b)
			section2.ReadVec4i("tex_rect")
		else
			section2.ReadVec4("tex_rect")
		section2.ReadString("technics")
	}
	section2.ReadString("shader")
	if(version >= ENTITY_VER_29) {
		section2.ReadString("technics")
		section2.ReadVec4("tex_rect")
	}
	// Modera: type changed from vec4f to vec4i in build 2711 (3 dec 2012)
	if(version >= ENTITY_VER_28b)
		section2.ReadVec4i("wnd_rect")
	else
		section2.ReadVec4("wnd_rect")
	section2.ReadU32("color", "color, u32")
	section2.ReadU32("scale")
	section2.ReadU8("h_alignment")
	section2.ReadU8("v_alignment")
	if(version >= ENTITY_VER_29) {
		section2.ReadBool("force_ready")
	} else {
		section2.ReadVec2("icon_offset")
	}
	section2.ReadVec2("icon_scale")
	section2.ReadFP32("rotate_speed")
	section2.ReadU8("flip_tc")
	section2.ReadFP32("rotate")
	
	// inventory_item_object::static_params::load
	section2.ReadVec2("font_pos")
}