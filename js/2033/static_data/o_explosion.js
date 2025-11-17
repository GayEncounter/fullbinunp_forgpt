// Auto-generated file

reader.ReadU16("version")
var section1 = reader.ReadSection("__edit")
section1.ReadString("caption")
reader.ReadBool("editable")
reader.ReadBool("visible_for_ai")
reader.ReadBool("block_ai_los")
reader.ReadBool("collideable")
reader.ReadU32("sound_explode_ai_type")
reader.ReadHintStr("coloranim", "ref_coloranim")
reader.ReadBool("hide_on_explosion")
reader.ReadFP32("light_deviation")
