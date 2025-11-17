// Arktika.1
// Подсказка: для использования этого скрипта нужно убрать первые 8 байт из файла nav_map.covers

var arr = reader.ReadArray("unames")
for(var i = 0; i < arr.count; i++)
{
	var e = arr.ReadSection(RecStr("rec_", i, 4), false)
	e.ReadString("name")
	e.ReadU16("id")
}

var arr = reader.ReadArray("ugroups")
for(var i = 0; i < arr.count; i++)
{
	var e = arr.ReadSection(RecStr("rec_", i, 4), false)
	e.ReadString("name")
}

// before Arktika.1 this array was at beginning of the file
var arr = reader.ReadArray("ucovers")
for(var i = 0; i < arr.count; i++)
{
	var e = arr.ReadSection(RecStr("rec_", i, 4), false)
	e.ReadVec3("position")
	e.ReadS32("ground_cell")
	e.ReadS32("conn_reg")
	e.ReadU32("dist_in_dir")
	e.ReadU16("cover_id")
	e.ReadU8("group0")
	e.ReadU8("group1")
	e.ReadBool("group_names") // NEW in Arktika.1
	e.ReadU16("pe_index")
	e.ReadU16("direction")
	e.ReadU8("cover_type_id")
	e.ReadHint("allowed_actions", "flags16") // Arktika.1: changed type to flags16
	e.ReadU16("allowed_actions") // Arktika.1: changed type to U16
	e.ReadU8("radius", "fp32_q8")
}

var arr = reader.ReadArray("ulinks")
for(var i = 0; i < arr.count; i++)
{
	var e = arr.ReadSection(RecStr("rec_", i, 4), false)
	e.ReadU16("from", "cover_link, ucover_link")
	e.ReadU16("to", "cover_link, ucover_link")
	e.ReadHintStr("move_action", "animation_str")
	e.ReadHintStr("move_action_add", "animation_str")
	e.ReadU32("cost")
	e.ReadU16("trans_frame")
	e.ReadU8("trans_speed")
	e.ReadU32("lnk_type")
	e.ReadU32("type")
	e.ReadU8("anim_state")
	e.ReadU8("mental_state")
}

var arr = reader.ReadArray("ulinks_pe")
for(var i = 0; i < arr.count; i++)
{
	var e = arr.ReadSection(RecStr("rec_", i, 4), false)
	e.ReadU16("from", "cover_link, ucover_link")
	e.ReadU16("to", "cover_link, ucover_link")
	e.ReadU32("cost")
	e.ReadU8("anim_state")
	e.ReadU8("mental_state")
}

reader.ReadFP32Array16("pe_dist_coefs", "fp32_array")
reader.ReadU8Array("pe_dist_infos")