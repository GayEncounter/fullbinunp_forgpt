var i = 0, l = reader.ReadArray("loader")
while(l.More())
{
	var item = l.ReadSection(RecStr("rec_", i++, 4), false)
	item.ReadU8('mp_mode')
	item.ReadString('map_name')
	item.ReadString('name')
	item.ReadHintStr('caption', 'choose')
	item.ReadU8('max_players')
	item.ReadU8('opt_players')
	item.ReadBool('press_any_key')
}