var presets = reader.ReadSection("sound_source_type_preset")
var preset_names = [
    "Default",
    "Ambient Very Loud",
    "Ambient Loud",
    "Ambient Medium",
    "Ambient Quiet",
    "Ambient Very Quiet",
    "Hit Huge",
    "Hit Large",
    "Hit Medium",
    "Hit Small",
    "Gunshot Large",
    "Gunshot Medium",
    "Gunshot Small",
    "Gunshot Silent",
    "Weapon Reload",
    "Explosion Large",
    "Explosion Medium",
    "Explosion Small",
    "Footsteps Human Run",
    "Footsteps Human Walk",
    "Footsteps Human Crouch",
    "Footsteps Monster Large",
    "Footsteps Monster Small",
    "Voice Human Shout",
    "Voice Human Loud",
    "Voice Human Medium",
    "Voice Human Whisper",
    "Voice Monster Loud",
    "Voice Monster Medium",
    "Voice Monster Quiet",
    "Footsteps Monster Medium",
    "Explosion Huge",
    "Hit Bullet",
    "Voice Human Scenario",
    "Voice Human Multiplayer",
    "Gunshot Huge",
    "Gunshot Close",
    "Motion Quiet",
    "Motion Medium",
    "Motion Loud",
    "Voice Human Louder",
    "Voice Human Group",
    "Voice Human Battle"
]
for (var i = 0; i < preset_names.length; i++) {
    var e = presets.ReadSection(preset_names[i])
    e.ReadU8("bus")
    e.ReadU8("spl")
    e.ReadFP32("base_dist")
    e.ReadFP32("lsf_max_distance")
    e.ReadFP32("lsf_freq")
    e.ReadFP32("lsf_gain_limit")
    e.ReadFP32("lsf_slope")
    e.ReadFP32("hsf_max_distance")
    e.ReadFP32("hsf_freq")
    e.ReadFP32("hsf_gain_limit")
    e.ReadFP32("hsf_slope")
    e.ReadFP32("max_makeup_gain")
    e.ReadFP32("reverb_level")
    e.ReadFP32("echo_level")
    e.ReadFP32("rolloff_factor")
    e.ReadBool("oneshot_ignore")
}

var params = reader.ReadArray("sound_params")
while (params.More()) {
    var e = params.ReadSection()
    var type = e.ReadU32("type")
    var name = e.ReadName("name")

    switch (type) {
        case 0:
            var loc = e.ReadBool("localizable")
            var sb = e.ReadBool("sub_titles")
            var sb_rad = e.ReadU8("sub_radius")
            var spl_ov = e.ReadU8("spl_ovveride")
            var nonpos_vol = e.ReadFP32("nonpos_volume")
            var nonpos_hdr = e.ReadBool("nonpos_hdr")
            var using_lop = e.ReadBool("using_as_looped")
            var tp = e.ReadU32("type_preset")
            if (loc) {
                var langs = e.ReadArray("langs")
                while (langs.More()) {
                    var l = langs.ReadSection() // TODO section name
                    l.ReadString("lang")
                    l.ReadU16("rms_peak")
                    l.ReadU16("rms_begin")
                    l.ReadU8("num_channels")
                    l.ReadU32("bytes_total")
                }
            } else {
                e.ReadU16("rms_peak")
                e.ReadU16("rms_begin")
                e.ReadU8("num_channels")
                e.ReadU32("bytes_total")
            }
            break;

        case 1:
            var arr = e.ReadArray("items")
            for (var u = 0; arr.More(); u++) {
                var elem = arr.ReadSection(RecStr("item_", u, 4), false)
                elem.ReadString("handle")
                elem.ReadFP32("probability")
            }
            break;

        case 2:
            break;

        case 3:
            var arr = e.ReadArray("items")
            for (var u = 0; arr.More(); u++) {
                var elem = arr.ReadSection(RecStr("item_", u, 4), false)
                elem.ReadString("name")
                elem.ReadString("folder")
            }
            break;

        case 4:
            e.ReadString("voice_set")
            e.ReadString("brief_name")
            break;

        case 5:
            e.ReadHintStr("master", "choose")
            e.ReadHintStr("slave", "choose")
            e.ReadFP32("slave_volume_factor")
            break;
    }
}

var masks = reader.ReadSection("sound_masks")
var mask_count = masks.ReadU32("mask_count")
for (var i = 0; i < mask_count; i++) {
    masks.ReadU32("mask")
    masks.ReadU32Array16("items", "u32_array")
}

var schemes = reader.ReadArray("sound_schemes")
while (schemes.More()) {
    var e = schemes.ReadSection()
    e.ReadName("name")
    e.ReadHint("array with no key", "array")
    var cnt = e.ReadU32("count")
    for (var i = 0; i < cnt; i++) {
        var s = e.ReadSection()
        s.ReadName("name")
        s.ReadBool("enabled")
        s.ReadBool("force_out")
        s.ReadHintStr("sound", "choose")
        s.ReadU32("priority")
        s.ReadU32("mask")
        s.ReadU32("game_type")
        s.ReadFP32("min_start_time")
        s.ReadFP32("max_start_time")
        s.ReadFP32("min_stop_time")
        s.ReadFP32("max_stop_time")
    }
}

var effects = reader.ReadArray("sound_effects")
while (effects.More()) {
    var e = effects.ReadSection()
    e.ReadString("name")
    e.ReadFP32("wet_dry_mix")
    e.ReadU32("reflections_delay")
    e.ReadU8("reverb_delay")
    e.ReadU8("rear_delay")
    e.ReadU8("position_left")
    e.ReadU8("position_right")
    e.ReadU8("position_matrix_left")
    e.ReadU8("position_matrix_right")
    e.ReadU8("early_diffusion")
    e.ReadU8("late_diffusion")
    e.ReadU8("low_eq_gain")
    e.ReadU8("low_eq_cutoff")
    e.ReadU8("high_eq_gain")
    e.ReadU8("high_eq_cutoff")
    e.ReadFP32("room_filter_freq")
    e.ReadFP32("room_filter_main")
    e.ReadFP32("room_filter_hf")
    e.ReadFP32("reflections_gain")
    e.ReadFP32("reverb_gain")
    e.ReadFP32("decay_time")
    e.ReadFP32("density")
    e.ReadFP32("room_size")
    e.ReadFP32("min_distance")
    e.ReadFP32("critical_distance")
    e.ReadFP32("max_distance")
    e.ReadFP32("min_reverb")
    e.ReadFP32("new_time")
    e.ReadFP32("new_level")
}

var echos = reader.ReadArray("echos")
for (var i = 0; i < echos.count; i++) {
    var e = echos.ReadSection()
    var name = e.ReadString("name")
    var listener = e.ReadSection("listener")

    // Listener data
    listener.ReadFP32("level")
    listener.ReadFP32("min_distance")
    listener.ReadFP32("max_distance")
    listener.ReadFP32("min_echo")
    listener.ReadFP32("max_echo")
    listener.ReadFP32("base_distance")
    listener.ReadU16("delay0")
    listener.ReadU16("delay1")
    listener.ReadU16("delay2")
    listener.ReadU16("delay3")
    listener.ReadFP32("feedback")
    listener.ReadFP32("fbc0")
    listener.ReadFP32("fbc1")
    listener.ReadFP32("fbc2")
    listener.ReadFP32("lpf_cutoff")
    listener.ReadFP32("lpf_q")
    listener.ReadFP32("hpf_cutoff")
    listener.ReadFP32("hpf_q")

    // Source
    var source = e.ReadSection("source")
    source.ReadFP32("level")
    source.ReadFP32("min_distance")
    source.ReadFP32("max_distance")
    source.ReadFP32("min_echo")
    source.ReadFP32("max_echo")
    source.ReadU16("delay0")
    source.ReadU16("delay1")
    source.ReadU16("delay2")
    source.ReadFP32("feedback")
    source.ReadFP32("tap_gain0")
    source.ReadFP32("tap_gain1")
    source.ReadFP32("tap_gain2")
    source.ReadFP32("lpf_cutoff")
    source.ReadFP32("lpf_q")
    source.ReadFP32("hpf_cutoff")
    source.ReadFP32("hpf_q")
}

var music_schemes = reader.ReadArray("music_schemes")
for (var i = 0; i < music_schemes.count; i++) {
    var e = music_schemes.ReadSection(RecStr('rec_', i, 4), false)
    e.ReadString("name")

    var dfd = e.ReadSection("default_link_def")
    dfd.ReadU32("source_bar_count")
    dfd.ReadU32("fade_out_time")
    dfd.ReadFP32("fade_out_offset")
    dfd.ReadU32("fade_in_time")
    dfd.ReadFP32("fade_in_offset")
    dfd.ReadU16("fade_out_type")
    dfd.ReadU16("fade_in_type")

    var themes = e.ReadArray("themes")
    while (themes.More()) {
        var themes_s = themes.ReadSection()
        themes_s.ReadHintStr("source_themes", "choose_array, str_shared")
        themes_s.ReadHintStr("target_themes", "choose_array, str_shared")
    }

    var links = e.ReadArray("links")
    while (links.More()) {
        var l_sec = links.ReadSection()
        l_sec.ReadString("name")
        l_sec.ReadHintStr("source", "choose")
        l_sec.ReadU32("source_bar_count")
        l_sec.ReadU32("fade_out_time")
        l_sec.ReadFP32("fade_out_offset")
        l_sec.ReadU32("fade_in_time")
        l_sec.ReadFP32("fade_in_offset")
        l_sec.ReadU16("fade_out_type")
        l_sec.ReadU16("fade_in_type")
    }
}