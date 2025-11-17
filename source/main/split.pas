program split;
uses windows, classes, sysutils, uCrc, Variants;

var
  knownscripts : array of String = [
    // configs (45)
    'gamedata',
    'language_type',
    'game_materials',
    'mainmenu',
    'subtitle_manager',
    'cover_manager',
    'render_subst_all',
    'render_tint_colors',
    'sound_type',
    'hit_type',
    'inner_shape_type',
    'shape_type',
    'behaviour_type',
    'hud_manager',
    'bullet_manager',
    'speech_manager',
    'suit_type',
    'joystick_delay',
    'trigger_main_menu_sequences',
    'environments',
    'color_anim_lib',
    'render_static_textures',
    'flares',
    'menus',
    'ehit_type',
    'interest_type',
    'net_props',
    'player_props',
    'damage_handle_presets',
    'ammo_type',
    'zone_type',
    'hud_particles_type',
    'achievements_icons',
    'class_icons',
    'weapon_icons',
    'weapon_attaches',
    'maploader_datas',
    'trade_presets',
    'dlc_tokens',
    'diary',
    'loader_data',
    'activities_storage',
    'speech_config_data',
    'mp_class_type_sgl',
    '_g.entities.ui_weapons_data',
    
    // visual scripts 2033 (8)
    'vs\player\achievement',
    'vs\player\death',
    'vs\player\fire_zone',
    'vs\player\gas_zone',
    'vs\player\rad_zone',
    'vs\npc\npc_gas_zone_1_mask',
    'vs\npc\npc_gas_zone_2_mask',
    'vs\npc\npc_outdoor_breath',
    
    // visual scripts Last Light (278)
    'aura\aura_15_1',
    'aura\aura_15_1_h',
    'aura\aura_15_2',
    'aura\aura_15_2_h',
    'aura\aura_15_3',
    'aura\aura_15_3_h',
    'aura\aura_15_4',
    'aura\aura_15_4_h',
    'aura\aura_17_pusher',
    'aura\aura_17_scout',
    'aura\luminocity_check',
    'dynamic\human\flag_l06',
    'dynamic\human\head_torch_free',
    'dynamic\objects\lamps\electric_lamp_hit_alert',
    'dynamic\objects\lamps\kerosinka_skrip',
    'dynamic\objects\lamps\lamp_attached_turn_off',
    'dynamic\objects\lamps\lamp_kerosinka_off_on',
    'dynamic\objects\lamps\lamp_npc_attached_turn_on_off',
    'fash_shoot_scary_3',
    'human\npc_human_deathbyfire',
    'human\vsref_npc_human_deathbyfire',
    'level_vs_temp\l14_entrance_wire',
    'levels\dlc_bonus\arahi_destroy',
    'levels\dlc_bonus\arahi_destroy_cocon',
    'levels\dlc_bonus\arahi_small_in_cocon_burn',
    'levels\dlc_bonus\arahi_small_orda',
    'levels\dlc_bonus\arahi_small_orda_dummy',
    'levels\dlc_bonus\arahind_fake_hit',
    'levels\dlc_bonus\arahind_footsteps',
    'levels\dlc_bonus\arahind_gasmask_hit',
    'levels\dlc_bonus\arahind_normal_flamethrower',
    'levels\dlc_bonus\arahind_normal_flamethrower_cocon',
    'levels\dlc_bonus\arahind_normal_flamethrower_fake',
    'levels\dlc_bonus\arahind_torch',
    'levels\dlc_khan\cocoon_dum',
    'levels\dlc_khan\cocoon_dum0',
    'levels\dlc_khan\rat_anomaly_lamphit',
    'levels\dlc_khan\rat_hit',
    'levels\dlc_khan\rat_hit_vent',
    'levels\dlc_khan\skelet_interes',
    'levels\dlc_ranger\lamp_electric_turn_on',
    'levels\dlc_ranger\rats_no_destroy',
    'levels\dlc_red\npc_ach_die',
    'levels\dlc_red\npc_house2_die',
    'levels\dlc_red\npc_zone1_die',
    'levels\dlc_red\npc_zone2_die',
    'levels\dlc_red\sniper_part2_die',
    'levels\l00\nos_speed',
    'levels\l04\destroy_roof',
    'levels\l07\cocon_small_final_rusik',
    'levels\l07\l07_fly_sound',
    'levels\l07\l07_ghosts',
    'levels\l07\l07_water_drippling_sound',
    'levels\l07\l07_water_falling_hard_sound',
    'levels\l07\l07_water_small_stream_sound',
    'levels\l07\rat_01',
    'levels\l07\wood_broken_effect',
    'levels\l08\l08_aqua_female_death',
    'levels\l08\l08_aqua_female_sound_scheme',
    'levels\l08\l08_kamish',
    'levels\l08\l08_reed_idle',
    'levels\l14_bridge\l14_1_demon_aqua_hit',
    'levels\l14_bridge\l14_2_ach',
    'levels\l14_bridge\l14_2_darckone_aura',
    'levels\l14_bridge\l14_2_demons_aura',
    'levels\l14_bridge\l14_2_metal_plats_sound_2',
    'levels\l14_bridge\l14_2_watchmans_aura',
    'levels\l14_bridge\l14_2_water_dripling',
    'levels\l14_bridge\swing_from_metal_01',
    'levels\l14_bridge\swing_from_metal_02',
    'levels\l14_bridge\swing_from_metal_02_up',
    'levels\l14_bridge\swing_from_metal_03',
    'levels\l14_bridge\swing_from_metal_03_up',
    'levels\l14_bridge\swing_from_metal_04',
    'levels\l14_bridge\swing_from_metal_04_up',
    'manto_l04',
    'monster\aqua_female_armor',
    'monster\aqua_female_swim_pfx',
    'monster\aqua_male_armor',
    'monster\arahnid_burn_effect',
    'monster\arahnid_death_melee',
    'monster\arahnid_on_shoot_kill',
    'monster\arahnid_poison_hit',
    'monster\arahnid_res_death',
    'monster\l08_aqua_female_swim_pfx',
    'monster\nosalis_male_deathbyfire',
    'monster\vsref_aqua_female_deathbyfire',
    'monster\vsref_aqua_male_deathbyfire',
    'monster\vsref_arahnid_deathbyfire',
    'monster\vsref_nosalis_female_deathbyfire',
    'monster\vsref_nosalis_male_deathbyfire',
    'monster\vsref_watchman_deathbyfire',
    'random_fash_face',
    'temp\alarm_camp_p1',
    'temp\alarm_camp_p2',
    'temp\alarm_camp_p3',
    'temp\alarm_venice',
    'temp\antiwallman',
    'temp\aqua_bulb',
    'temp\cable_idle',
    'temp\cg_door',
    'temp\danger_checker',
    'temp\dlc_arena_teleport',
    'temp\dlc_armory_mantabat_1',
    'temp\dlc_armory_mantabat_2',
    'temp\dlc_npc_part2_die',
    'temp\dlc_tower_nos_in_water',
    'temp\face_idle_bandit',
    'temp\face_idle_inventor',
    'temp\fash_shoot_scary_2',
    'temp\fash_shoot_scary_4',
    'temp\goo_reaction',
    'temp\l04_catacombs_ach',
    'temp\l05_inter',
    'temp\l05_smeh',
    'temp\l06_flag_2',
    'temp\l06_glass\glass_big_parts',
    'temp\l06_glass\glass_big_parts_danger',
    'temp\l06_glass\glass_big_true',
    'temp\l06_glass\glass_strem_sound',
    'temp\l06_intro_fake_chest',
    'temp\l06_intro_fake_listen',
    'temp\l06_intro_fake_pressup',
    'temp\l06_intro_fake_salute',
    'temp\l06_intro_fake_walk',
    'temp\nos_female_effects',
    'temp\nos_jump_effects',
    'temp\npc_arah_fire_control',
    'temp\npc_radio_mls',
    'temp\npc_torchlight_lin',
    'temp\obst_ceiling_hard_script',
    'temp\obst_ceiling_light_script',
    'temp\obst_high_hard_script',
    'temp\obst_high_light_script',
    'temp\obst_low_hard_script',
    'temp\obst_low_light_script',
    'temp\polis_uber_anim_1',
    'temp\polis_uber_anim_zit',
    'temp\random_fash_anim',
    'temp\random_fash_scared_left',
    'temp\random_fash_scared_right',
    'temp\random_lent',
    'temp\sound_manager_1',
    'temp\sound_support',
    'temp\tarakan_hide',
    'temp\temp_dlc_end_level',
    'temp\tower_arm',
    'temp\tower_cheat_dispersion',
    'temp\tower_destroy_weapon',
    'temp\tower_nos_speed_control',
    'temp\tower_npc_logic',
    'temp\wall_dyn_h_city',
    'temp\wall_dyn_l_city',
    'temp\wall_dyn_m_city',
    'temp\wallman',
    'temp\wallman_v2',
    'temp\watchman_aura_lin',
    'temp\watchman_die_karma',
    'temp\watchman_hit_impulse',
    'temp\watchman_regular',
    'temp\yazoo\attach_feedback',
    'temp\yazoo\death_noregdoll_0',
    'temp\yazoo\death_noregdoll_ent',
    'temp\yazoo\l20\dead_friend',
    'temp\yazoo\l20\helm_close',
    'temp\yazoo\l20\interes_onplayer',
    'temp\yazoo\l20\l20_alarm_react',
    'temp\yazoo\l20\l20_alarm_react0',
    'temp\yazoo\l20\npc_cvr_events',
    'temp\yazoo\l20\npc_cvr_hit',
    'temp\yazoo\tolpa_d6_inside',
    'vs\levels\destroy_loot',
    'vs\levels\dlc\bib_melee_attack_1',
    'vs\levels\dlc\fire_ammo_death',
    'vs\levels\dlc\han_nos_snarl',
    'vs\levels\dlc\monster_long_destroy',
    'vs\levels\dlc\npc_blood',
    'vs\levels\gel_loot',
    'vs\levels\gel_loot_use',
    'vs\levels\l00_intro_2\auto_lamps',
    'vs\levels\l00_intro_2\bugs_hide',
    'vs\levels\l00_intro_2\mantabat_monsters',
    'vs\levels\l00_intro_2\mantabat_monsters_lin',
    'vs\levels\l00_intro_2\mantabat_monsters_lin_2',
    'vs\levels\l00_intro_2\mantabat_monsters_lin_3',
    'vs\levels\l00_intro_2\mantabat_monsters_lin_4',
    'vs\levels\l00_intro_2\mantabat_monsters_lin_6',
    'vs\levels\l04_catacombs\l04_catacombs_cocon_mid',
    'vs\levels\l04_catacombs\l04_monst_destr',
    'vs\levels\l04_plane\demon_death',
    'vs\levels\l04_plane\watch_horde_destroy',
    'vs\levels\l10_swamp\aqua_demon_hit',
    'vs\levels\l10_swamp\bug',
    'vs\levels\l10_swamp\demon_att',
    'vs\levels\l10_swamp\manta_restore',
    'vs\levels\l10_swamp\naushnik_hit',
    'vs\levels\l10_swamp\swap_model_to_night',
    'vs\levels\l10_swamp\wall_aqua',
    'vs\levels\l11_undercity\l11_monster_destroy',
    'vs\levels\l13_1\nos_eye',
    'vs\levels\l13_1\water_avoid',
    'vs\levels\l16_dead_city\dark_loot',
    'vs\levels\l16_dead_city\demon_roof_attack_new',
    'vs\levels\l16_dead_city\far_ghost_aura',
    'vs\levels\l16_dead_city\ghost_aura',
    'vs\levels\l16_dead_city\metro_ghost_walk',
    'vs\levels\l16_dead_city\monster_destroy',
    'vs\levels\l16_dead_city\provod_short',
    'vs\levels\l16_dead_city\tryapka_1',
    'vs\levels\l16_dead_city\watch_aura_new',
    'vs\levels\l16_dead_city\watch_aura_new_2',
    'vs\levels\l18_garden\l18_arachnid_hide_water',
    'vs\levels\l18_garden\l18_buggy_static',
    'vs\levels\l18_garden\l18_dark_teleport',
    'vs\levels\l20\helm_lynx_close',
    'vs\levels\mob_loot',
    'vs\levels\mob_loot_ammo',
    'vs\levels\mob_loot_bib',
    'vs\monster\aqua_attack_watch\aqua_attack_watch_0000',
    'vs\monster\l04\rat_hide',
    'vs\monster\l04_catacombs\l04_cata_rats',
    'vs\monster\l10\aqua_big_attack',
    'vs\monster\l10\aqua_female_swim_part',
    'vs\monster\l10\aqua_male_swim_part',
    'vs\monster\l10\aqua_part_dead',
    'vs\monster\l10\demon_attack_no_p_force',
    'vs\monster\l10\demon_death',
    'vs\monster\l10\player_friend',
    'vs\monster\manta\arahi_go',
    'vs\monster\rat_hide',
    'vs\npc\collision',
    'vs\npc\cvr_force_fire',
    'vs\npc\death_noregdoll_test',
    'vs\npc\ik',
    'vs\npc\lian_att',
    'vs\npc\luminocity',
    'vs\npc\npc_fonarik',
    'vs\npc\npc_fonarik_helm',
    'vs\npc\npc_fonarik_lamp',
    'vs\npc\npc_gas_zone_1_mask',
    'vs\npc\npc_gas_zone_mls',
    'vs\npc\npc_melee_death_2',
    'vs\npc\npc_melee_death_2_no_stun',
    'vs\npc\npc_outdoor_breath',
    'vs\player\achievement',
    'vs\player\achievement_dlc',
    'vs\player\achievement_dlc2',
    'vs\player\bullets_dirt',
    'vs\player\civil_station',
    'vs\player\crouch_control',
    'vs\player\crouch_control_dlc1',
    'vs\player\death',
    'vs\player\death_dlc_armory',
    'vs\player\death_dlc_story_ann',
    'vs\player\end_level',
    'vs\player\fast_drop_hint',
    'vs\player\fire_zone',
    'vs\player\gas_zone',
    'vs\player\gas_zone_dlc_story_ann',
    'vs\player\hints',
    'vs\player\karma',
    'vs\player\map_task',
    'vs\player\no_electro_anomaly',
    'vs\player\rad_zone',
    'vs\player\rad_zone_dlc1',
    'vs\player\rain_zone',
    'vs\player\ranger_mode',
    'vs\player\take_weapon_hint',
    'vs\player\water_death',
    'vs\test\demon_npc_hit',
    'vs\test\l17\aura',
    'vs\weapon\fire_imitation_oneshot',
    'vs\weapon\fire_imitation_oneshot_noai',
    'vs\weapon\fire_imitation_oneshot_noai_true',
    'vs\weapon\fire_imitation_oneshot_shorts',
    'vs\weapon\laser_sight_disable_on_npc_death',
    'vslevelsl10_swampbug_2',
    'web_nospider_ach',
    
    // levels 2033
    '000',
    'l00_intro',
    'l01_hunter',
    'l02_exhibition',
    'l03_chase',
    'l04_riga',
    'l05_lost_tunnel',
    'l06_bridge',
    'l07_catacombes',
    'l08_market',
    'l09_dead_city_1',
    'l10_dead_city_2',
    'l11_dry',
    'l12_ghosts',
    'l13_anomaly',
    'l14_cursed',
    'l15_armory',
    'l16_frontline',
    'l17_trolley_combat',
    'l18_depot',
    'l19_defence',
    'l20_child',
    'l21_nazi_outpost',
    'l22_black',
    'l23_polis',
    'l24_alley',
    'l25_library',
    'l26_depository',
    'l27_archives',
    'l28_driving',
    'l30_darkstar',
    'l31_catacombes',
    'l32_cave',
    'l33_d6',
    'l34_biomass',
    'l35_division',
    'l36_ostankino',
    'l38_tower',
    'l39_ethereal',
    
    // levels Last Light
    //'000',
    'benchmark',
    'dlc_dev_armory',
    'dlc_faction_ganza',
    'dlc_faction_nazi',
    'dlc_faction_red',
    'dlc_pyro',
    'dlc_story_anna',
    'dlc_story_khan',
    'dlc_story_pavel',
    'dlc_tower1',
    'l00_intro1',
    'l00_intro2',
    'l01_jail',
    'l02_escape',
    'l03_camp_1',
    'l03_camp_2',
    'l04_catacombes',
    'l04_plane',
    'l05_theatre',
    'l06_revolution',
    'l06_revolution_intro',
    'l07_tunnel',
    'l07_tunnel_2',
    'l08_water_tunnel',
    'l09_venice',
    'l10_swamp',
    'l10_swamp_02',
    'l11_undercity',
    'l12_circus',
    'l12_circus_2',
    'l13_train_1',
    'l13_train_2',
    'l14_bridge',
    'l14_bridge_2',
    'l15_train_depot',
    'l16_dead_city',
    'l17_red_square',
    'l18_garden',
    'l19_polis',
    'l20_d6_defense',
    
    // levels Redux
    '2033\000', 
    '2033\l00_intro', 
    '2033\l01_hunter', 
    '2033\l02_exhibition', 
    '2033\l03_chase', 
    '2033\l04_riga', 
    '2033\l05_lost_tunnel', 
    '2033\l08_market', 
    '2033\l09_dead_city_1', 
    '2033\l11_dry', 
    '2033\l12_ghosts', 
    '2033\l14_cursed', 
    '2033\l15_armory', 
    '2033\l16_frontline', 
    '2033\l17_trolley_combat', 
    '2033\l18_depot', 
    '2033\l19_defence', 
    '2033\l21_nazi_outpost', 
    '2033\l22_black', 
    '2033\l23_polis', 
    '2033\l24_alley', 
    '2033\l26_depository', 
    '2033\l27_archives', 
    '2033\l28_driving', 
    '2033\l30_darkstar', 
    '2033\l32_cave', 
    '2033\l33_d6', 
    '2033\l36_ostankino', 
    '2034\000', 
    '2034\dlc_dev_armory', 
    '2034\dlc_faction_ganza', 
    '2034\dlc_faction_nazi', 
    '2034\dlc_faction_red', 
    '2034\dlc_pyro', 
    '2034\dlc_story_anna', 
    '2034\dlc_story_khan', 
    '2034\dlc_story_pavel', 
    '2034\dlc_tower1', 
    '2034\l00_intro_1', 
    '2034\l00_intro_2', 
    '2034\l01_jail', 
    '2034\l02_escape', 
    '2034\l03_camp_1', 
    '2034\l03_camp_2', 
    '2034\l04_catacombs', 
    '2034\l04_plane', 
    '2034\l05_theatre', 
    '2034\l06_revolution_intro', 
    '2034\l06_revolution_main', 
    '2034\l07_tunnel', 
    '2034\l07_tunnel_2', 
    '2034\l08_water_tunnel', 
    '2034\l09_venice', 
    '2034\l10_swamp', 
    '2034\l10_swamp_02', 
    '2034\l11_undercity', 
    '2034\l12_circus', 
    '2034\l12_circus_2', 
    '2034\l13_train_1', 
    '2034\l13_train_2', 
    '2034\l14_bridge', 
    '2034\l14_bridge_2', 
    '2034\l15_train_depot', 
    '2034\l16_dead_city', 
    '2034\l17_red_square', 
    '2034\l18_garden', 
    '2034\l19_polis', 
    '2034\l20_d6_defense',
    'benchmarks\benchmark',
    'benchmarks\benchmark33',
    
    // static data 2033 (101)
    'static_data\ameba__g.config.entity.ameba',
    'static_data\ammo__g.config.entity.ammo_044_fmj',
    'static_data\ammo__g.config.entity.ammo_12x70mm',
    'static_data\ammo__g.config.entity.ammo_15mm',
    'static_data\ammo__g.config.entity.ammo_545x39_fmj',
    'static_data\ammo__g.config.entity.ammo_money',
    'static_data\anim_object__g.config.entity.hands_for_drezina',
    'static_data\anomaly__g.config.entity.simple_anomaly',
    'static_data\biomass_large__g.config.entity.biomass_large',
    'static_data\charger__g.config.entity.charger',
    'static_data\dark__g.config.entity.dark',
    'static_data\drezina_hand__g.config.entity.drezina_hand',
    'static_data\drezina_moto__g.config.entity.drezina_moto',
    'static_data\effect__g.config.entity.effect',
    'static_data\filter__g.config.entity.filter',
    'static_data\flower__g.config.entity.flower',
    'static_data\gasmask__g.config.entity.gasmask',
    'static_data\hands_for_drezina__g.config.entity.hands_for_drezina',
    'static_data\harpy__g.config.entity.harpy',
    'static_data\helsing_arrow__g.config.entity.wpn_arrow',
    'static_data\kid__g.config.entity.kid',
    'static_data\kulemet__g.config.entity.flame_thrower',
    'static_data\kulemet__g.config.entity.kulemet',
    'static_data\kulemet__g.config.entity.kulemet_bashnya',
    'static_data\ladder__g.config.entity.ladder',
    'static_data\lian__g.config.entity.lian',
    'static_data\librarian__g.config.entity.librarian',
    'static_data\lurker__g.config.entity.lurker',
    'static_data\medkit__g.config.entity.medkit',
    'static_data\nightvision__g.config.entity.nightvision',
    'static_data\nosalis__g.config.entity.nosalis',
    'static_data\nosalis__g.config.entity.volosaty',
    'static_data\nosalis_female__g.config.entity.nosalis_female',
    'static_data\npc_fx__g.config.entity.npc_enemy_fx',
    'static_data\npc_fx__g.config.entity.npc_friend_fx',
    'static_data\o_aipoint__g.config.entity.ai_point',
    'static_data\o_basezone__g.config.entity.restrictor',
    'static_data\o_entity__g.config.entity.entity',
    'static_data\o_entity__g.config.entity.station',
    'static_data\o_explosion__g.config.entity.explosion',
    'static_data\o_hlamp__g.config.entity.hanging_lamp',
    'static_data\o_waterzone__g.config.entity.waterzone',
    'static_data\patrol_point__g.config.entity.patrol_point',
    'static_data\player__g.config.entity.player',
    'static_data\player_map__g.config.entity.player_map',
    'static_data\players_hands__g.config.entity.players_hands',
    'static_data\players_knife__g.config.entity.players_knife',
    'static_data\proxy__g.config.entity.proxy',
    'static_data\rat__g.config.entity.kid',
    'static_data\rat__g.config.entity.rat',
    'static_data\simple_monster__g.config.entity.simple_anomaly',
    'static_data\simple_npc__g.config.entity.monster',
    'static_data\soft_entity__g.config.entity.soft_entity',
    'static_data\soft_entity_inst__g.config.entity.soft_entity_inst',
    'static_data\staticprop__g.config.entity.static',
    'static_data\station_stand__g.config.entity.station_stand',
    'static_data\torch__g.config.entity.torchlight',
    'static_data\visualscript__g.config.entity.vs',
    'static_data\watchman__g.config.entity.watchman',
    'static_data\weapon_2012__g.config.entity.wpn_2012',
    'static_data\weapon_2012__g.config.entity.wpn_2012_scope',
    'static_data\weapon_abzac__g.config.entity.wpn_abzac',
    'static_data\weapon_ak74_test__g.config.entity.wpn_ak74_optics_test',
    'static_data\weapon_ak74_test__g.config.entity.wpn_ak74_relsa',
    'static_data\weapon_ak_74__g.config.entity.wpn_ak_74',
    'static_data\weapon_ak_74__g.config.entity.wpn_ak_74_optics',
    'static_data\weapon_dagger__g.config.entity.wpn_dagger',
    'static_data\weapon_duplet__g.config.entity.wpn_duplet_fx',
    'static_data\weapon_dynamite__g.config.entity.wpn_dynamite',
    'static_data\weapon_flash_grenade__g.config.entity.wpn_flash_grenage',
    'static_data\weapon_hellbreath__g.config.entity.wpn_hellbreath',
    'static_data\weapon_helsing__g.config.entity.wpn_helsing',
    'static_data\weapon_helsing__g.config.entity.wpn_optics_helsing',
    'static_data\weapon_macheta__g.config.entity.wpn_macheta',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_barrel',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_barrel_laser',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle_laser',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle_optic',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle_optic_laser',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle_optic_silencer',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle_silencer',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle_silencer_laser',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_silencer',
    'static_data\weapon_sticky_dynamite__g.config.entity.wpn_sticky_dynamite',
    'static_data\weapon_tihar__g.config.entity.wpn_optics_tihar',
    'static_data\weapon_tihar__g.config.entity.wpn_tihar',
    'static_data\weapon_ubludok__g.config.entity.wpn_ak74',
    'static_data\weapon_ubludok__g.config.entity.wpn_ak74_fixed',
    'static_data\weapon_ubludok__g.config.entity.wpn_ak74_fx',
    'static_data\weapon_ubludok__g.config.entity.wpn_ubludok',
    'static_data\weapon_ubludok__g.config.entity.wpn_ubludok_fx',
    'static_data\weapon_ubludok__g.config.entity.wpn_ubludok_silencer',
    'static_data\weapon_uboynicheg__g.config.entity.wpn_bayonet_uboynicheg_fx',
    'static_data\weapon_uboynicheg__g.config.entity.wpn_shotgun_trap',
    'static_data\weapon_uboynicheg__g.config.entity.wpn_uboynicheg',
    'static_data\weapon_uboynicheg__g.config.entity.wpn_uboynicheg_fx',
    'static_data\weapon_vsv__g.config.entity.wpn_vsv',
    'static_data\weapon_vsv__g.config.entity.wpn_vsv_scope',
    'static_data\woman__g.config.entity.woman',
    
    // static data Last Light (239)
    'static_data\2301c4ef_2301c4ef_539d30e8', // static_data\staticprop__g.config.entity.static
    'static_data\7f6adca7_7f6adca7_5af35372', // static_data\weapon_item_noobetube__g.config.entity.weapon_noobtube
    'static_data\0cbbe8a6_0cbbe8a6_7b9fa65a', // static_data\o_hlamp__g.config.entity.hanging_lamp
    'static_data\ebc6700d_ebc6700d_76e6b55a', // static_data\weapon_item__g.config.entity.weapon_magazine
    'static_data\46985674_46985674_653d893f', // static_data\effect__g.config.entity.effect
    'static_data\186d3d2d_186d3d2d_ee3048bf', // static_data\weapon_item_noobegun__g.config.entity.weapon_noobgun
    'static_data\27fdf76a_27fdf76a_8fb8d996', // static_data\weapon_item_laser__g.config.entity.weapon_laser
    'static_data\b42ea669_b42ea669_6dfe6770', // static_data\patrol_point__g.config.entity.patrol_point
    'static_data\d91a939e_d91a939e_5ab4373b', // static_data\ammo__g.config.entity.ammo_12x70mm_fire
    'static_data\5aeb1d81_5aeb1d81_cd2679e2', // static_data\players_hands__g.config.entity.players_hands
    'static_data\d91a939e_d91a939e_81767601', // static_data\ammo__g.config.entity.ammo_044_ap
    'static_data\45215999_45215999_68518ffc', // static_data\player_map__g.config.entity.player_map
    'static_data\0f10b43b_0f10b43b_dd755ca5', // static_data\o_entity__g.config.entity.entity
    'static_data\d91a939e_d91a939e_172a1e1e', // static_data\ammo__g.config.entity.ammo_12x70mm
    'static_data\b713a3c2_b713a3c2_94b67c89', // static_data\medkit__g.config.entity.medkit
    'static_data\b616569a_b616569a_bf923686', // static_data\weapon_dynamite__g.config.entity.wpn_dynamite
    'static_data\4015029a_4015029a_80e3ed61', // static_data\force_field__g.config.entity.force_field
    'static_data\2c1992e1_2c1992e1_4c834911', // static_data\weapon_launcher_time__g.config.entity.wpn_launcher_time_grenage
    'static_data\d91a939e_d91a939e_f2c05bb2', // static_data\ammo__g.config.entity.ammo_044_fmj
    'static_data\1c36400d_1c36400d_2e1a7721', // static_data\weapon_macheta__g.config.entity.wpn_macheta
    'static_data\d91a939e_d91a939e_4c6bd212', // static_data\ammo__g.config.entity.ammo_money
    'static_data\d91a939e_d91a939e_bc62ab9b', // static_data\ammo__g.config.entity.ammo_545x39_fmj
    'static_data\9151aa6f_9151aa6f_f7eb5312', // static_data\npc_fx__g.config.entity.npc_friend_fx
    'static_data\84260b0e_84260b0e_f89effd9', // static_data\proxy__g.config.entity.proxy
    'static_data\d91a939e_d91a939e_9ca45a39', // static_data\ammo__g.config.entity.ammo_762x39_fmj
    'static_data\05275e6e_05275e6e_4f2ab9f8', // static_data\o_basezone__g.config.entity.restrictor
    'static_data\d91a939e_d91a939e_d47ea361', // static_data\ammo__g.config.entity.ammo_dshk
    'static_data\72ad9e96_72ad9e96_a8ee8399', // static_data\charger__g.config.entity.charger
    'static_data\15583d02_15583d02_e8690f4b', // static_data\helsing_arrow__g.config.entity.wpn_arrow
    'static_data\473ce97a_473ce97a_1601ce81', // static_data\player_timer__g.config.entity.player_timer_entity
    'static_data\8f3c989b_8f3c989b_ac9947d0', // static_data\filter__g.config.entity.filter
    'static_data\04adf9c7_04adf9c7_049b81cd', // static_data\torch__g.config.entity.torchlight
    'static_data\b0584545_b0584545_98bdb76a', // static_data\weapon_flame_dynamite__g.config.entity.wpn_flamethrower_grenage
    'static_data\8b576ec2_8b576ec2_a707b243', // static_data\weapon_sticky_dynamite__g.config.entity.wpn_sticky_dynamite
    'static_data\d91a939e_d91a939e_c8aefcf3', // static_data\ammo__g.config.entity.ammo_15mm
    'static_data\b0584545_b0584545_a9e80d6b', // static_data\weapon_flame_dynamite__g.config.entity.wpn_flame_grenage
    'static_data\7a245948_7a245948_73a03954', // static_data\weapon_claymore__g.config.entity.wpn_claymore
    'static_data\d91a939e_d91a939e_a117d05d', // static_data\ammo__g.config.entity.ammo_762x39_mg
    'static_data\54571bfa_54571bfa_fd805e46', // static_data\visualscript__g.config.entity.vs
    'static_data\1f193ba1_1f193ba1_eb42f4ad', // static_data\weapon_dagger__g.config.entity.wpn_dagger
    'static_data\e8ca57b3_e8ca57b3_229f6c22', // static_data\o_waterzone__g.config.entity.waterzone
    'static_data\e13d62a8_e13d62a8_3b7e7fa7', // static_data\lighter__g.config.entity.lighter
    'static_data\8685b651_8685b651_1148d232', // static_data\players_knife__g.config.entity.players_knife
    'static_data\bd40ada9_bd40ada9_323af09f', // static_data\weapon_abzac__g.config.entity.wpn_abzac
    'static_data\71871a27_71871a27_685a059b', // static_data\weapon_ak_74__g.config.entity.wpn_ak_74
    'static_data\68e1bde3_68e1bde3_4b4462a8', // static_data\player__g.config.entity.player
    'static_data\d91a939e_d91a939e_bdeb3a72', // static_data\ammo__g.config.entity.ammo_flame_fmj
    'static_data\ebc6700d_ebc6700d_0dd9d5eb', // static_data\weapon_item__g.config.entity.weapon_magazine_base_abzac
    'static_data\d91a939e_d91a939e_564ccb2b', // static_data\ammo__g.config.entity.ammo_12x70mm_flamethrower
    'static_data\27fdf76a_27fdf76a_887dff27', // static_data\weapon_item_laser__g.config.entity.weapon_laser_shotgun
    'static_data\ebc6700d_ebc6700d_3f3b57ec', // static_data\weapon_item__g.config.entity.weapon_autofire_abzac
    'static_data\ebc6700d_ebc6700d_5e065b85', // static_data\weapon_item__g.config.entity.weapon_magazine_abzac
    'static_data\ebc6700d_ebc6700d_6d213aca', // static_data\weapon_item__g.config.entity.weapon_compens_abzac
    'static_data\ebc6700d_ebc6700d_01b49735', // static_data\weapon_item__g.config.entity.weapon_silencer
    'static_data\ebc6700d_ebc6700d_4f7286df', // static_data\weapon_item__g.config.entity.weapon_magazine_2
    'static_data\ebc6700d_ebc6700d_37008456', // static_data\weapon_item__g.config.entity.weapon_envg
    'static_data\ebc6700d_ebc6700d_665b2889', // static_data\weapon_item__g.config.entity.weapon_scope
    'static_data\ebc6700d_ebc6700d_cf440776', // static_data\weapon_item__g.config.entity.weapon_optics_mount_ak
    'static_data\ebc6700d_ebc6700d_86a5a38e', // static_data\weapon_item__g.config.entity.weapon_kalimator
    'static_data\8f22a0ed_8f22a0ed_f39a543a', // static_data\woman__g.config.entity.woman
    'static_data\ebc6700d_ebc6700d_82be931d', // static_data\weapon_item__g.config.entity.weapon_silencer_saiga
    'static_data\ebc6700d_ebc6700d_f0a7eb0a', // static_data\weapon_item__g.config.entity.weapon_magazine_saiga
    'static_data\b574ea5d_b574ea5d_7f21d1cc', // static_data\o_explosion__g.config.entity.explosion
    'static_data\ebc6700d_ebc6700d_a3786564', // static_data\weapon_item__g.config.entity.weapon_magazine_base_saiga
    'static_data\2fa04ac0_2fa04ac0_fc6a503c', // static_data\kulemet__g.config.entity.kulemet_light
    'static_data\2fa04ac0_2fa04ac0_a5b4bb3b', // static_data\kulemet__g.config.entity.kulemet_bashnya
    'static_data\ebc6700d_ebc6700d_6b0641df', // static_data\weapon_item__g.config.entity.weapon_flashhider_ventil
    'static_data\ebc6700d_ebc6700d_980014e0', // static_data\weapon_item__g.config.entity.weapon_magazine_ventil
    'static_data\ebc6700d_ebc6700d_b90aa004', // static_data\weapon_item__g.config.entity.weapon_optics_mount_picatinny_ventil
    'static_data\ebc6700d_ebc6700d_32598731', // static_data\weapon_item__g.config.entity.weapon_magazine_base_ventil
    'static_data\ebc6700d_ebc6700d_33a16665', // static_data\weapon_item__g.config.entity.weapon_barrel_revolver
    'static_data\83c583e4_83c583e4_8a41e3f8', // static_data\weapon_revolver__g.config.entity.wpn_revolver
    'static_data\ebc6700d_ebc6700d_6e7242cf', // static_data\weapon_item__g.config.entity.weapon_no_butt_revolver
    'static_data\ebc6700d_ebc6700d_1d8f9dd5', // static_data\weapon_item__g.config.entity.weapon_collimator_small
    'static_data\ebc6700d_ebc6700d_d67646db', // static_data\weapon_item__g.config.entity.weapon_optics_mount_picatinny
    'static_data\ebc6700d_ebc6700d_3dda1af5', // static_data\weapon_item__g.config.entity.weapon_optics_sniper
    'static_data\ebc6700d_ebc6700d_ea72283e', // static_data\weapon_item__g.config.entity.weapon_magazine_preved
    'static_data\b616569a_b616569a_a429ee92', // static_data\weapon_dynamite__g.config.entity.wpn_dynamite_yz
    'static_data\ebc6700d_ebc6700d_402bbbef', // static_data\weapon_item__g.config.entity.weapon_magazine_base_preved
    'static_data\151dc865_151dc865_65d5e65c', // static_data\rat__g.config.entity.rat
    'static_data\ebc6700d_ebc6700d_c6dab65d', // static_data\weapon_item__g.config.entity.weapon_stabilizer_gear_gatling
    'static_data\e32722e3_e32722e3_177cedef', // static_data\weapon_preved__g.config.entity.wpn_preved
    'static_data\9151aa6f_9151aa6f_fe5690a7', // static_data\npc_fx__g.config.entity.npc_enemy_fx
    'static_data\13e11d26_13e11d26_9c9b4010', // static_data\weapon_saiga__g.config.entity.wpn_saiga
    'static_data\27dcdaf2_27dcdaf2_862a2a69', // static_data\o_aipoint__g.config.entity.ai_point
    'static_data\91551e3d_91551e3d_650ed131', // static_data\weapon_ventil__g.config.entity.wpn_ventil
    'static_data\2fa04ac0_2fa04ac0_4f0e9a77', // static_data\kulemet__g.config.entity.kulemet_light_bench
    'static_data\ebc6700d_ebc6700d_19747d01', // static_data\weapon_item__g.config.entity.weapon_flashhider_preved
    'static_data\ebc6700d_ebc6700d_e27788e1', // static_data\weapon_item__g.config.entity.weapon_butt_gatling
    'static_data\b5b0650c_b5b0650c_1ff32d7b', // static_data\weapon_gatling__g.config.entity.wpn_gatling_d6
    'static_data\ebc6700d_ebc6700d_7152687c', // static_data\weapon_item__g.config.entity.weapon_engine_base_frame_gatling
    'static_data\aa177401_aa177401_53549294', // static_data\staticprop_breakable__g.config.entity.static_breakable
    'static_data\ebc6700d_ebc6700d_a7975e59', // static_data\weapon_item__g.config.entity.weapon_stabilizer_gatling
    'static_data\ebc6700d_ebc6700d_8909bcdd', // static_data\weapon_item__g.config.entity.weapon_engine_base_disk_gatling
    'static_data\ebc6700d_ebc6700d_6e52ffbc', // static_data\weapon_item__g.config.entity.weapon_engine_fuel_gatling
    'static_data\ebc6700d_ebc6700d_86d16236', // static_data\weapon_item__g.config.entity.weapon_engine_frame_gatling
    'static_data\ebc6700d_ebc6700d_0e5f2d6c', // static_data\weapon_item__g.config.entity.weapon_engine_gatling
    'static_data\ebc6700d_ebc6700d_da6989ba', // static_data\weapon_item__g.config.entity.weapon_pentabarrel_gatling
    'static_data\ebc6700d_ebc6700d_895731de', // static_data\weapon_item__g.config.entity.weapon_stabilizer_wheel_gatling
    'static_data\ebc6700d_ebc6700d_5e4fdcc3', // static_data\weapon_item__g.config.entity.weapon_engine_base_arm_gatling
    'static_data\ebc6700d_ebc6700d_e1c4be32', // static_data\weapon_item__g.config.entity.weapon_engine_mech_gatling
    'static_data\ebc6700d_ebc6700d_c4ba7684', // static_data\weapon_item__g.config.entity.weapon_engine_cap_gatling
    'static_data\ebc6700d_ebc6700d_d7b55915', // static_data\weapon_item__g.config.entity.weapon_engine_base_gear_gatling
    'static_data\ebc6700d_ebc6700d_293ea716', // static_data\weapon_item__g.config.entity.weapon_tribarrel_gatling
    'static_data\ebc6700d_ebc6700d_f568347e', // static_data\weapon_item__g.config.entity.weapon_engine_base_gatling
    'static_data\ebc6700d_ebc6700d_6388322e', // static_data\weapon_item__g.config.entity.weapon_silencer_revolver
    'static_data\ebc6700d_ebc6700d_6647fa04', // static_data\weapon_item__g.config.entity.weapon_butt_revolver
    'static_data\ebc6700d_ebc6700d_00fa62fb', // static_data\weapon_item__g.config.entity.weapon_pivot_revolver
    'static_data\ebc6700d_ebc6700d_f3da7a78', // static_data\weapon_item__g.config.entity.weapon_optics_tihar
    'static_data\ebc6700d_ebc6700d_53365630', // static_data\weapon_item__g.config.entity.weapon_barrel_padonag
    'static_data\ebc6700d_ebc6700d_66f99440', // static_data\weapon_item__g.config.entity.weapon_tank_base_tihar
    'static_data\ebc6700d_ebc6700d_edbaa894', // static_data\weapon_item__g.config.entity.weapon_barrel_shotgun
    'static_data\ebc6700d_ebc6700d_d2c2de64', // static_data\weapon_item__g.config.entity.weapon_silencer_shotgun_pistol
    'static_data\65655829_65655829_bf264526', // static_data\arahind__g.config.entity.arahind
    'static_data\ebc6700d_ebc6700d_171b470d', // static_data\weapon_item__g.config.entity.weapon_no_butt_ashot
    'static_data\ebc6700d_ebc6700d_963b0e64', // static_data\weapon_item__g.config.entity.weapon_barrel_ashot
    'static_data\ebc6700d_ebc6700d_b41e34eb', // static_data\weapon_item__g.config.entity.weapon_magazine_rpk_base
    'static_data\ebc6700d_ebc6700d_10c2ba20', // static_data\weapon_item__g.config.entity.weapon_pivot_ashot
    'static_data\a6d94122_a6d94122_e42601e5', // static_data\big_mother__g.config.entity.big_mother
    'static_data\ebc6700d_ebc6700d_05c45a8d', // static_data\weapon_item__g.config.entity.weapon_butt_ashot
    'static_data\ebc6700d_ebc6700d_f1565dc7', // static_data\weapon_item__g.config.entity.weapon_butt_ubludok
    'static_data\ebc6700d_ebc6700d_a9d3bb8a', // static_data\weapon_item__g.config.entity.weapon_no_butt_ubludok
    'static_data\ebc6700d_ebc6700d_14b0402b', // static_data\weapon_item__g.config.entity.weapon_radiator_ubludok
    'static_data\a691b02a_a691b02a_3047382b', // static_data\weapon_ubludok__g.config.entity.wpn_ubludok_fx
    'static_data\81146306_81146306_5b577e09', // static_data\nosalis__g.config.entity.nosalis
    'static_data\ebc6700d_ebc6700d_49bf8626', // static_data\weapon_item__g.config.entity.weapon_optics_mount_vsv
    'static_data\a2812f11_a2812f11_d95e7de3', // static_data\weapon_duplet__g.config.entity.wpn_duplet_fx
    'static_data\ebc6700d_ebc6700d_9725d7fa', // static_data\weapon_item__g.config.entity.weapon_silencer_vsv
    'static_data\28005254_28005254_0ba58d1f', // static_data\grizly__g.config.entity.grizly
    'static_data\ebc6700d_ebc6700d_8eff4650', // static_data\weapon_item__g.config.entity.weapon_battery_hellbreath
    'static_data\ebc6700d_ebc6700d_414ac3fd', // static_data\weapon_item__g.config.entity.weapon_optics_mount_hellbreath
    'static_data\ebc6700d_ebc6700d_eeab07e3', // static_data\weapon_item__g.config.entity.weapon_capacitor_hellbreath
    'static_data\0e154f91_0e154f91_a51e31d5', // static_data\watchman__g.config.entity.watchman
    'static_data\ebc6700d_ebc6700d_0d1e3d40', // static_data\weapon_item__g.config.entity.weapon_silencer_aksu
    'static_data\27fdf76a_27fdf76a_ed48bba9', // static_data\weapon_item_laser__g.config.entity.weapon_laser_aksu
    'static_data\ebc6700d_ebc6700d_7f132140', // static_data\weapon_item__g.config.entity.weapon_silencer_padonag
    'static_data\ebc6700d_ebc6700d_0c2cd322', // static_data\weapon_item__g.config.entity.weapon_butt
    'static_data\1029375d_1029375d_8b3205b4', // static_data\weapon_uboynicheg__g.config.entity.wpn_uboynicheg_fx
    'static_data\6e8a02bb_6e8a02bb_81c8dff5', // static_data\nosalis_female__g.config.entity.nosalis_female
    'static_data\ebc6700d_ebc6700d_fa5b04ae', // static_data\weapon_item__g.config.entity.weapon_pivot
    'static_data\ebc6700d_ebc6700d_d5309510', // static_data\weapon_item__g.config.entity.weapon_magazine_base_padonag
    'static_data\ebc6700d_ebc6700d_b4ca4f1c', // static_data\weapon_item__g.config.entity.weapon_no_butt
    'static_data\ebc6700d_ebc6700d_93b1cc8f', // static_data\weapon_item__g.config.entity.weapon_autofire_2_padonag
    'static_data\ebc6700d_ebc6700d_99c7d339', // static_data\weapon_item__g.config.entity.weapon_autofire_padonag
    'static_data\ebc6700d_ebc6700d_244e2ec1', // static_data\weapon_item__g.config.entity.weapon_magazine_padonag
    'static_data\29b629e8_29b629e8_2151fab9', // static_data\aqua_female__g.config.entity.aqua_female
    'static_data\b616569a_b616569a_cb6ad99b', // static_data\weapon_dynamite__g.config.entity.wpn_aqua_sputum
    'static_data\39b5ac32_39b5ac32_eb164a07', // static_data\aqua_male_small__g.config.entity.aqua_male_small
    'static_data\57174204_57174204_d86d1f32', // static_data\weapon_ashot__g.config.entity.wpn_ashot
    'static_data\4a3d9517_4a3d9517_368561c0', // static_data\harpy__g.config.entity.harpy
    'static_data\41403263_41403263_b51bfd6f', // static_data\weapon_vyhlop__g.config.entity.wpn_vyhlop
    'static_data\8cd5596a_8cd5596a_3e7715a2', // static_data\aqua_male_big__g.config.entity.aqua_male_big
    'static_data\36fc912e_36fc912e_293f58d2', // static_data\weapon_aksu__g.config.entity.wpn_aksu
    'static_data\548be63c_548be63c_4dc174cf', // static_data\weapon_rpk__g.config.entity.wpn_rpk
    'static_data\ebc6700d_ebc6700d_e24cab6f', // static_data\weapon_item__g.config.entity.weapon_magazine_base_vyhlop
    'static_data\e74bf56a_e74bf56a_d567c246', // static_data\weapon_padonag__g.config.entity.wpn_padonag
    'static_data\27fdf76a_27fdf76a_ffaf0f23', // static_data\weapon_item_laser__g.config.entity.weapon_laser_hellbreath
    'static_data\ebc6700d_ebc6700d_47a5a1b1', // static_data\weapon_item__g.config.entity.weapon_optics_hellbreath
    'static_data\1029375d_1029375d_e67a9685', // static_data\weapon_uboynicheg__g.config.entity.wpn_bigun
    'static_data\ebc6700d_ebc6700d_52e17c03', // static_data\weapon_item__g.config.entity.weapon_duplet_no_butt
    'static_data\ebc6700d_ebc6700d_7534b7cc', // static_data\weapon_item__g.config.entity.weapon_bell_handle
    'static_data\ebc6700d_ebc6700d_489c7795', // static_data\weapon_item__g.config.entity.weapon_bell_base
    'static_data\ebc6700d_ebc6700d_a6359d08', // static_data\weapon_item__g.config.entity.weapon_tank_base_helsing
    'static_data\71871a27_71871a27_d126bfcc', // static_data\weapon_ak_74__g.config.entity.wpn_ak_74_l17_green_laser
    'static_data\e74bf56a_e74bf56a_7299fdad', // static_data\weapon_padonag__g.config.entity.wpn_padonag_green_laser
    'static_data\e32722e3_e32722e3_cbdd59e4', // static_data\weapon_preved__g.config.entity.wpn_preved_green_laser
    'static_data\93cfe433_93cfe433_2d2c81d9', // static_data\weapon_2012__g.config.entity.wpn_2012_green_laser
    'static_data\27fdf76a_27fdf76a_27d7d003', // static_data\weapon_item_laser__g.config.entity.weapon_laser_green
    'static_data\93cfe433_93cfe433_ba99c07b', // static_data\weapon_2012__g.config.entity.wpn_2012
    'static_data\91551e3d_91551e3d_b043f608', // static_data\weapon_ventil__g.config.entity.wpn_ventil_dlc_faction_nazi_green_laser
    'static_data\b5b0650c_b5b0650c_879c5220', // static_data\weapon_gatling__g.config.entity.wpn_gatling
    'static_data\1029375d_1029375d_b710633d', // static_data\weapon_uboynicheg__g.config.entity.wpn_uboynicheg_fx_green_laser
    'static_data\27fdf76a_27fdf76a_fc1cc2c6', // static_data\weapon_item_laser__g.config.entity.weapon_laser_shotgun_green
    'static_data\13e11d26_13e11d26_f6135754', // static_data\weapon_saiga__g.config.entity.wpn_saiga_green_laser
    'static_data\83c583e4_83c583e4_47fcda70', // static_data\weapon_revolver__g.config.entity.wpn_revolver_green_laser
    'static_data\945fd87c_945fd87c_56314427', // static_data\weapon_hellbreath__g.config.entity.wpn_hellbreath_green_laser
    'static_data\27fdf76a_27fdf76a_f25adf4a', // static_data\weapon_item_laser__g.config.entity.weapon_laser_hellbreath_green
    'static_data\945fd87c_945fd87c_9170e997', // static_data\weapon_hellbreath__g.config.entity.wpn_hellbreath
    'static_data\ebc6700d_ebc6700d_06b67403', // static_data\weapon_item__g.config.entity.weapon_optics_vyhlop
    'static_data\8083cc79_8083cc79_b2affb55', // static_data\weapon_helsing__g.config.entity.wpn_helsing
    'static_data\5060e530_5060e530_12dece68', // static_data\weapon_flamethrower__g.config.entity.wpn_flamethrower
    'static_data\36131f51_36131f51_c248d05d', // static_data\weapon_medved__g.config.entity.wpn_medved
    'static_data\b75365b6_b75365b6_38293880', // static_data\weapon_tihar__g.config.entity.wpn_tihar
    'static_data\1ba971fa_1ba971fa_02e3e309', // static_data\weapon_vsv__g.config.entity.wpn_vsv
    'static_data\ebc6700d_ebc6700d_8c5ab1f3', // static_data\weapon_item__g.config.entity.weapon_duplet_silencer
    'static_data\ebc6700d_ebc6700d_59d6656f', // static_data\weapon_item__g.config.entity.weapon_duplet_barrel
    'static_data\ebc6700d_ebc6700d_442ad5c9', // static_data\weapon_item__g.config.entity.weapon_duplet_butt_2
    'static_data\ebc6700d_ebc6700d_2954bd4d', // static_data\weapon_item__g.config.entity.weapon_duplet_butt
    'static_data\ebc6700d_ebc6700d_761c9ee4', // static_data\weapon_item__g.config.entity.weapon_duplet_barrel_big
    'static_data\ebc6700d_ebc6700d_1c011d41', // static_data\weapon_item__g.config.entity.weapon_duplet_barrel_big_hammer
    'static_data\ebc6700d_ebc6700d_c667eeae', // static_data\weapon_item__g.config.entity.weapon_magazine_rpk
    'static_data\27fdf76a_27fdf76a_40d32ae7', // static_data\weapon_item_laser__g.config.entity.weapon_laser_vyhlop
    'static_data\2c0b059f_2c0b059f_0faedad4', // static_data\lurker__g.config.entity.lurker
    'static_data\ebc6700d_ebc6700d_c19fdfe4', // static_data\weapon_item__g.config.entity.weapon_silencer_shotgun
    'static_data\ebc6700d_ebc6700d_69bb4240', // static_data\weapon_item__g.config.entity.weapon_tank_helsing
    'static_data\ebc6700d_ebc6700d_42f15e29', // static_data\weapon_item__g.config.entity.weapon_tank_tihar
    'static_data\7975e29b_7975e29b_33014ca4', // static_data\librarian__g.config.entity.librarian
    'static_data\cfc909c3_cfc909c3_158a14cc', // static_data\gasmask__g.config.entity.gasmask
    'static_data\c7457ba3_c7457ba3_e4e0a4e8', // static_data\ladder__g.config.entity.ladder
    'static_data\d5452dd1_d5452dd1_121fc71c', // static_data\nightvision__g.config.entity.nightvision
    'static_data\0cbbe8a6_0cbbe8a6_c34945d2', // static_data\o_hlamp__g.config.entity.reflected_light
    'static_data\836e7adb_836e7adb_f3a654e2', // static_data\web__g.config.entity.web
    'static_data\57174204_57174204_3f31b037', // static_data\weapon_ashot__g.config.entity.wpn_ashot_dlc_class_pack
    'static_data\572f065c_572f065c_748ad917', // static_data\flower__g.config.entity.flower
    'static_data\f7cb9c05_f7cb9c05_93d0fceb', // static_data\lian__g.config.entity.lian
    'static_data\e74bf56a_e74bf56a_04633c14', // static_data\weapon_padonag__g.config.entity.wpn_padonag_dlc_class_pack
    'static_data\83c583e4_83c583e4_662fbdf0', // static_data\weapon_revolver__g.config.entity.wpn_revolver_dlc_class_pack
    'static_data\1029375d_1029375d_82d61831', // static_data\weapon_uboynicheg__g.config.entity.wpn_uboynicheg_fx_dlc_class_pack
    'static_data\a2812f11_a2812f11_0a71f95c', // static_data\weapon_duplet__g.config.entity.wpn_duplet_fx_dlc_class_pack
    'static_data\ebc6700d_ebc6700d_28add819', // static_data\weapon_item__g.config.entity.weapon_duplet_barrel_big_class_pack
    'static_data\4250e763_4250e763_0824495c', // static_data\darkchild__g.config.entity.darkchild
    'static_data\eeffe3be_eeffe3be_0106f86f', // static_data\o_interest__g.config.entity.interest
    'static_data\b713a3c2_b713a3c2_9884d789', // static_data\medkit__g.config.entity.medkit_dlc_story_ann
    'static_data\27fdf76a_27fdf76a_c27f1273', // static_data\weapon_item_laser__g.config.entity.weapon_laser_dlc_story_ann
    'static_data\e32722e3_e32722e3_9f377752', // static_data\weapon_preved__g.config.entity.wpn_preved_dlc_story_ann_far
    'static_data\cfc909c3_cfc909c3_ae22d6fc', // static_data\gasmask__g.config.entity.gasmask_dlc_story_ann
    'static_data\e32722e3_e32722e3_71f02ab1', // static_data\weapon_preved__g.config.entity.wpn_preved_dlc_story_ann
    'static_data\1f193ba1_1f193ba1_9c74a56e', // static_data\weapon_dagger__g.config.entity.wpn_dagger_dlc_story_ann
    'static_data\1c36400d_1c36400d_c2db3eb8', // static_data\weapon_macheta__g.config.entity.wpn_macheta_dlc_story_ann
    'static_data\68e1bde3_68e1bde3_b21f88bb', // static_data\player__g.config.entity.player_dlc_story_ann
    'static_data\d384caf6_d384caf6_a34ce4cf', // static_data\kid__g.config.entity.kid
    'static_data\6881c5cb_6881c5cb_a8772a30', // static_data\woman_strip__g.config.entity.woman_strip
    'static_data\03016a46_03016a46_0be6b917', // static_data\soft_entity__g.config.entity.soft_entity
    'static_data\b616569a_b616569a_2941c323', // static_data\weapon_dynamite__g.config.entity.wpn_nosalis_sputum
    'static_data\2de9504f_2de9504f_49f230a1', // static_data\dark__g.config.entity.dark
    'static_data\8d41ffd3_8d41ffd3_5702e2dc', // static_data\effectm__g.config.entity.effectm
    'static_data\2fa04ac0_2fa04ac0_f5e357cf', // static_data\kulemet__g.config.entity.kulemet
    'static_data\23b85b2c_23b85b2c_cd459813', // static_data\hands_for_drezina__g.config.entity.hands_for_drezina
    'static_data\93bf64c4_93bf64c4_f7a4042a', // static_data\bush__g.config.entity.bush
    'static_data\9b9f0f1f_9b9f0f1f_8422e434', // static_data\drezina_hand__g.config.entity.drezina_hand
    'static_data\8120f174_8120f174_9e9d1a5f', // static_data\drezina_moto__g.config.entity.drezina_moto
    'static_data\9b1fdd5e_9b1fdd5e_1dc2b93a', // static_data\siege_bomb__g.config.entity.siege_bomb
    'static_data\8bbccf55_8bbccf55_f5752449', // static_data\heap__g.config.entity.heap_2
    'static_data\b0584545_b0584545_3a2bc827', // static_data\weapon_flame_dynamite__g.config.entity.wpn_flame_good
    'static_data\67a85977_67a85977_57fb971c', // static_data\breakable_ice__g.config.entity.breakable_ice
    'static_data\151dc865_151dc865_a34ce4cf', // static_data\rat__g.config.entity.kid
    'static_data\301e54df_301e54df_4e503fce', // static_data\stretchy_man__g.config.entity.stretchy_man
    'static_data\301e54df_301e54df_4578e2f6', // static_data\stretchy_man__g.config.entity.stretchy_handleft
    'static_data\301e54df_301e54df_d3910576', // static_data\stretchy_man__g.config.entity.stretchy_hand
	
	// static data REDUX (34)
	'static_data\c3cf4d71_c3cf4d71_175c03d2',
	'static_data\ebc6700d_ebc6700d_184459f9',
	'static_data\ebc6700d_ebc6700d_c8c48dfa',
	'static_data\ebc6700d_ebc6700d_c1e5e9f1',
	'static_data\ebc6700d_ebc6700d_f5a17390',
	'static_data\ebc6700d_ebc6700d_9f9eb18b',
	'static_data\ebc6700d_ebc6700d_ed1468c3',
	'static_data\945fd87c_945fd87c_1fed156b',
	'static_data\b616569a_b616569a_d193432e',
	'static_data\7f0a15ae_7f0a15ae_03b2e179',
	'static_data\c4233840_c4233840_53ee5c23',
	'static_data\945fd87c_945fd87c_0eadca9d',
	'static_data\5060e530_5060e530_cf9016f1',
	'static_data\bd40ada9_bd40ada9_582fecca',
	'static_data\6900cbb3_6900cbb3_e67a9685',
	'static_data\00917627_00917627_cd459813',
	'static_data\06583184_06583184_c15825e1',
	'static_data\1ddf6053_1ddf6053_8a120430',
	'static_data\b0584545_b0584545_1934295e',
	'static_data\2fa04ac0_2fa04ac0_0b7218ab',
	'static_data\ebc6700d_ebc6700d_ac218c4b',
	'static_data\7c49c064_7c49c064_7867f577',
	'static_data\2fa04ac0_2fa04ac0_feeb65dd',
	'static_data\0f10b43b_0f10b43b_7a3f1ba4',
	'static_data\1029375d_1029375d_1ec5db17',
	'static_data\ebc6700d_ebc6700d_241c4e8b',
	'static_data\2fa04ac0_2fa04ac0_fe4e3e53',
	'static_data\81146306_81146306_299a6dc8',
	'static_data\ebc6700d_ebc6700d_2eac319a',
	'static_data\ebc6700d_ebc6700d_0bff603a',
	'static_data\ebc6700d_ebc6700d_e5f10116',
	'static_data\ebc6700d_ebc6700d_92f63180',
	'static_data\ebc6700d_ebc6700d_59ab010c',
	'static_data\ebc6700d_ebc6700d_c0a250b6',
    
    // skeleton sequence value 2033 (23)
    'dynamic\hud\weapon\2012\2012_skeleton_sequence_value',
    'dynamic\hud\weapon\ak\ak_skeleton_sequence_value',
    'dynamic\hud\weapon\bomb\predohranitel\bomb_adhesive_skeleton_sequence_value',
    'dynamic\hud\weapon\bomb\predohranitel\bomb_flare_skeleton_sequence_value',
    'dynamic\hud\weapon\bomb\predohranitel\bomb_predohranitel_skeleton_sequence_value',
    'dynamic\hud\weapon\duplet\duplet_skeleton_sequence_value',
    'dynamic\hud\weapon\dynamo_hand\dynamo_hand_skeleton_sequence_value',
    'dynamic\hud\weapon\gasmask\gasmask_skeleton_sequence_value',
    'dynamic\hud\weapon\helsing\helsing_skeleton_sequence_value',
    'dynamic\hud\weapon\knives\knives_skeleton_sequence_value',
    'dynamic\hud\weapon\macheta\macheta_skeleton_sequence_value',
    'dynamic\hud\weapon\map\map_skeleton_sequence_value',
    'dynamic\hud\weapon\med_kit\med_kit_skeleton_sequence_value',
    'dynamic\hud\weapon\nv\nv_skeleton_sequence_value',
    'dynamic\hud\weapon\ognemet\ognemet_skeleton_sequence_value',
    'dynamic\hud\weapon\railgun_hand\railgun_hand_skeleton_sequence_value',
    'dynamic\hud\weapon\revolver\a_revolver_skeleton_sequence_value',
    'dynamic\hud\weapon\shot\shot_skeleton_sequence_value',
    'dynamic\hud\weapon\tihar\tihar_skeleton_sequence_value',
    'dynamic\hud\weapon\ubludok\ubludok_gold_skeleton_sequence_value',
    'dynamic\hud\weapon\uboynicheg\uboynicheg_skeleton_sequence_value',
    'dynamic\hud\weapon\vsv\vsv_skeleton_sequence_value',
    'dynamic\objects\drezina\drezina_skeleton_sequence_value',
	
	// skeleton sequence value REDUX (165)
	'dynamic\hud\weapon\att\att_magazin_1_skeleton_sequence_value',
	'dynamic\hud\34_hand\hud_skeleton_sequence_value',
	'dynamic\hud\weapon\map\map_skeleton_sequence_value',
	'dynamic\hud\weapon\med_kit\med_kit_skeleton_sequence_value',
	'dynamic\hud\weapon\bomb\att_bomb_low_skeleton_sequence_value',
	'dynamic\hud\weapon\bomb\att_bomb_flame_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_patron_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_filtr_skeleton_sequence_value',
	'dynamic\wpn\wpn_molotov_skeleton_sequence_value',
	'dynamic\hud\weapon\bomb\att_bomb_hard_skeleton_sequence_value',
	'dynamic\hud\weapon\saiga\saiga_skeleton_sequence_value',
	'dynamic\hud\weapon\ventil\ventil_skeleton_sequence_value',
	'dynamic\hud\weapon\ubludok\ubludok_skeleton_sequence_value',
	'dynamic\hud\weapon\rpk\rpk_skeleton_sequence_value',
	'dynamic\hud\weapon\aksu\aksu_skeleton_sequence_value',
	'dynamic\hud\weapon\tihar\tihar_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_kollimator_skeleton_sequence_value',
	'dynamic\hud\weapon\att\vsv_glushak_skeleton_sequence_value',
	'dynamic\hud\weapon\att\vsv_mount_skeleton_sequence_value',
	'dynamic\hud\weapon\durshlag\durshlag_skeleton_sequence_value',
	'dynamic\hud\weapon\knives\att_knife_skeleton_sequence_value',
	'dynamic\hud\weapon\zazigalko\zazigalko_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_shot_stvol_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_grenade_prujina_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_shot_boy_skeleton_sequence_value',
	'dynamic\hud\hand\hand_sviter_skeleton_sequence_value',
	'dynamic\hud\weapon\timer_detector\timer_detector_skeleton_sequence_value',
	'dynamic\hud\weapon\timer_detector\timer_detector_2033_skeleton_sequence_value',
	'dynamic\hud\hand\basic_suit_skeleton_sequence_value',
	'dynamic\hud\hand\heavy_suit_skeleton_sequence_value',
	'dynamic\hud\hand\stealth_suit_skeleton_sequence_value',
	'dynamic\hud\hand\soldier_suit_skeleton_sequence_value',
	'dynamic\hud\hand\ranger_suit_skeleton_sequence_value',
	'dynamic\hud\hand\sniper_suit_skeleton_sequence_value',
	'dynamic\hud\hand\pavel_suit_skeleton_sequence_value',
	'dynamic\hud\hand\khan_suit_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_glushak_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_magazin_2_skeleton_sequence_value',
	'dynamic\hud\weapon\ashot\ashot_skeleton_sequence_value',
	'dynamic\hud\weapon\padonacheg\padonacheg_skeleton_sequence_value',
	'dynamic\hud\weapon\att\envg_skeleton_sequence_value',
	'dynamic\hud\weapon\att\ak_optika_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_mount_skeleton_sequence_value',
	'dynamic\hud\weapon\granade_att\grenade_root_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_shot_root_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_lazer_skeleton_sequence_value',
	'dynamic\hud\hand\basic_suit_cs_skeleton_sequence_value',
	'dynamic\hud\hand\heavy_suit_cs_skeleton_sequence_value',
	'dynamic\hud\hand\stealth_suit_cs_skeleton_sequence_value',
	'dynamic\hud\hand\soldier_suit_cs_skeleton_sequence_value',
	'dynamic\hud\hand\ranger_suit_cs_skeleton_sequence_value',
	'dynamic\hud\hand\sniper_suit_cs_skeleton_sequence_value',
	'dynamic\hud\hand\pavel_suit_cs_skeleton_sequence_value',
	'dynamic\hud\hand\khan_suit_cs_skeleton_sequence_value',
	'dynamic\wpn\wpn_dshk_bashnya_skeleton_sequence_value',
	'dynamic\hud\weapon\att\hellbreath_accum_skeleton_sequence_value',
	'dynamic\hud\weapon\preved\preved_skeleton_sequence_value',
	'dynamic\hud\weapon\vyhlop\vyhlop_skeleton_sequence_value',
	'dynamic\hud\weapon\att\ventil_optika_skeleton_sequence_value',
	'dynamic\hud\weapon\att\railgun_scope_rail_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_collimator_small_skeleton_sequence_value',
	'dynamic\hud\weapon\att\hellbreath_capacitor_skeleton_sequence_value',
	'dynamic\objects\web\web_big34_new_skeleton_sequence_value',
	'dynamic\monsters\biomasa_hobot_big\biomasa_hobot_big_skeleton_sequence_value',
	'dynamic\hud\weapon\ognemet_mini\ognemet_dlc_skeleton_sequence_value',
	'dynamic\hud\weapon\att\abzac_compens_skeleton_sequence_value',
	'dynamic\hud\weapon\att\abzac_autofire_skeleton_sequence_value',
	'dynamic\hud\weapon\att\abzac_magazin_small_skeleton_sequence_value',
	'dynamic\hud\weapon\att\padonacheg_magazin_big_skeleton_sequence_value',
	'dynamic\hud\weapon\att\padonache_gazootvod_skeleton_sequence_value',
	'dynamic\hud\weapon\att\padonacheg_shtir_skeleton_sequence_value',
	'dynamic\hud\weapon\att\padonacheg_butt_small_skeleton_sequence_value',
	'dynamic\hud\weapon\att\padonacheg_magazin_small_skeleton_sequence_value',
	'dynamic\hud\weapon\att\padonache_butt_skeleton_sequence_value',
	'dynamic\hud\weapon\att\padonacheg_barrel_skeleton_sequence_value',
	'dynamic\hud\weapon\att\padonacheg_butt_big_skeleton_sequence_value',
	'dynamic\hud\weapon\att\padonacheg_silencer_skeleton_sequence_value',
	'dynamic\monsters\lians\lian_skeleton_sequence_value',
	'dynamic\monsters\flovers\big\big_flover_1_skeleton_sequence_value',
	'dynamic\hud\weapon\bigun\bigun_skeleton_sequence_value',
	'dynamic\hud\weapon\att\ventil_magazin_skeleton_sequence_value',
	'dynamic\hud\weapon\att\bigun_bell_skeleton_sequence_value',
	'dynamic\hud\weapon\att\bigun_bell_handle_skeleton_sequence_value',
	'dynamic\objects\drezina\drezina_par\drezina_par_skeleton_sequence_value',
	'dynamic\wpn\termodatchik_skeleton_sequence_value',
	'dynamic\monsters\flovers\big\big_flover_0_skeleton_sequence_value',
	'dynamic\objects\drezina\quadro\quadro_skeleton_sequence_value',
	'dynamic\monsters\nosach\nosach_skeleton_sequence_value',
	'dynamic\wpn\bashnya_skeleton_sequence_value',
	'dynamic\objects\drezina\drezima_motocikl\drezina_motocikl_skeleton_sequence_value',
	'dynamic\hud\weapon\nightvision\nightvision_skeleton_sequence_value',
	'dynamic\wpn\wpn_panzar_bashnya_skeleton_sequence_value',
	'dynamic\objects\drezina\drezina_buggi\drezina_baggi_skeleton_sequence_value',
	'dynamic\hud\weapon\att\saiga_magazin_small_skeleton_sequence_value',
	'dynamic\hud\weapon\att\saiga_magazin_big_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_patron_uboynicheg_skeleton_sequence_value',
	'dynamic\objects\drezina\l05_drezina\l05_drezina2_skeleton_sequence_value',
	'dynamic\objects\drezina\l05_drezina\l05_drezina_skeleton_sequence_value',
	'dynamic\hud\weapon\att\helsing_pnevma2_skeleton_sequence_value',
	'dynamic\hud\weapon\att\helsing_pnevma1_skeleton_sequence_value',
	'dynamic\wpn\termometr_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_butt2_skeleton_sequence_value',
	'dynamic\hud\weapon\att\ashot_pivot_skeleton_sequence_value',
	'dynamic\hud\weapon\att\ashot_barrel_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_butt_skeleton_sequence_value',
	'dynamic\wpn\wpn_dshk_skeleton_sequence_value',
	'dynamic\hud\weapon\att\preved_magazin_skeleton_sequence_value',
	'dynamic\hud\weapon\att\preved_hider_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_dummy_skeleton_sequence_value',
	'dynamic\objects\web\web_big_02_skeleton_sequence_value',
	'dynamic\objects\drezina\l02_indoor_vdnh\l02_indoor_vdnh_skeleton_sequence_value',
	'dynamic\objects\box\dummy_01_skeleton_sequence_value',
	'dynamic\hud\weapon\att\ubludok_radiator_skeleton_sequence_value',
	'dynamic\hud\weapon\att\ubludok_butt_2_skeleton_sequence_value',
	'dynamic\hud\weapon\att\tihar_valve_01_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_tihar_prizel_skeleton_sequence_value',
	'dynamic\hud\weapon\att\tihar_valve_02_skeleton_sequence_value',
	'dynamic\hud\weapon\att\ubludok_butt_1_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_barrel_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_glushak_big_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_picatinny_rail_skeleton_sequence_value',
	'dynamic\hud\weapon\att\revolver_butt_small_skeleton_sequence_value',
	'dynamic\hud\weapon\att\revolver_pivot_skeleton_sequence_value',
	'dynamic\hud\weapon\att\revolver_barrel_skeleton_sequence_value',
	'dynamic\hud\weapon\att\revolver_butt_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_klema_skeleton_sequence_value',
	'dynamic\hud\weapon\att\duplet_quadbarrel_skeleton_sequence_value',
	'dynamic\hud\weapon\att\duplet_butt_priklad_skeleton_sequence_value',
	'dynamic\hud\weapon\att\duplet_barrel_long_skeleton_sequence_value',
	'dynamic\hud\weapon\att\duplet_butt_small_skeleton_sequence_value',
	'dynamic\hud\weapon\att\duplet_silencer_skeleton_sequence_value',
	'dynamic\hud\weapon\att\duplet_butt_big_skeleton_sequence_value',
	'dynamic\objects\carcass\carcass_hand_skeleton_sequence_value',
	'dynamic\objects\carcass\carcass_handleft_skeleton_sequence_value',
	'dynamic\objects\carcass\carcass_a_skeleton_sequence_value',
	'dynamic\hud\hand\basic_suit_cs_short_skeleton_sequence_value',
	'dynamic\hud\weapon\wpn_box\box_metal_8_skeleton_sequence_value',
	'dynamic\monsters\flovers\big\big_flover_2_skeleton_sequence_value',
	'dynamic\objects\kamish\reed2_skeleton_sequence_value',
	'dynamic\hud\34_hand\hud_dlc4_skeleton_sequence_value',
	'dynamic\hud\34_hand\hud_dlc3_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_magazin_3_skeleton_sequence_value',
	'dynamic\hud\weapon\att\att_vyhlop_magazin_skeleton_sequence_value',
	'dynamic\hud\weapon\att\ventil_magazin_1_skeleton_sequence_value',
	'dynamic\hud\weapon\att\ventil_flashhider_skeleton_sequence_value',
	'dynamic\hud\weapon\medved\medved_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_giro_gear_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_m_pruj_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_dulo3_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_m_meh_02_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_b_truba_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_b_meh_01_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_m_meh_01_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_giro_wheel_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_dulo5_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_b_motor_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_base5_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_b_bak_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_m_meh_03_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_giro_base_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_base3_skeleton_sequence_value',
	'dynamic\hud\weapon\att\gatling_grip_skeleton_sequence_value',
	'dynamic\hud\weapon\gatling\gatling_skeleton_sequence_value',
	'dynamic\hud\34_hand\hud_dlc_skeleton_sequence_value',
	'dynamic\hud\weapon\att\abzac_magazin_big_skeleton_sequence_value',
    
	// configs REDUX (130)
	'achievement_types',
	'location_types',
	'action_types',
	'diary_2033',
	'diary_2034',
	'mp_class_type_mp',
	'_g.entities.primary_weapons',
	'_g.entities.secondary_weapons',
	'_g.entities.perks',
	'materials',
	'environment_lightning',
	'texture_aliases',
	'fonts_cz',
	'fonts_de',
	'fonts_es',
	'fonts_fr',
	'fonts_it',
	'fonts_nl',
	'fonts_pl',
	'fonts_ru',
	'fonts_uk',
	'fonts_us',
	'dynamic\monsters\rat\capral_rat',
	'dynamic\monsters\murzik\murzik',
	'dynamic\monsters\demon\demon',
	'dynamic\monsters\mantabat\mantabat',
	'dynamic\monsters\baggy\buggy',
	'dynamic\human\woman_new',
	'dynamic\human\human',
	'dynamic\human\kid_new',
	'dynamic\objects\box\dummy_01',
	'temp\watchman_npc_short_attack',
	'temp\watchman_npc_long_attack',
	'nosach\watchman\watch_360_short_attack',
	'nosach\watchman\watch_jump_attack_1',
	'nosach\watchman\watch_360_short_attack_death',
	'temp\yazoo\watch_clone_skin',
	'temp\yazoo\make_enemy',
	'dynamic\objects\smatrelka\smatrelka',
	'temp\l38_tower_demon_effect',
	'dynamic\monsters\ameba\ameba',
	'dynamic\monsters\biomasa_hobot_big\biomasa_hobot_big_skeleton_sequence_value',
	'levels\dlc_bonus\dlc_arahind_melee_heavy',
	'levels\dlc_bonus\dlc_arahind_melee_normal',
	'monster\arahnid_deathbyfire',
	'dynamic\monsters\arahind\vsref_arahnid_torch',
	'monster\arahnid_heavy_melee',
	'monster\arahnid_melee',
	'dynamic\monsters\arahind\arahind',
	'dynamic\monsters\nosach\vsref_l30_add_damage',
	'monster\vsref_librarian_hitbyfire',
	'dynamic\monsters\bibliotekar\vsref_bib_melee',
	'dynamic\monsters\bibliotekar\vsref_bib_effects',
	'dynamic\monsters\bibliotekar\bibliotekar',
	'dynamic\monsters\demon\vsref_demon_catch',
	'monster\aqua_male_deathbyfire',
	'dynamic\monsters\aqua_male_small\aqua_male_small',
	'dynamic\human\effects\vsref_npc_breath',
	'temp\d6_alllamp_ref',
	'temp\l22_black_add_script',
	'dynamic\objects\car\vsref_car_snow_02',
	'dynamic\human\vsref_catch_by_demon',
	'dynamic\objects\car\vsref_car_snow',
	'temp\l21_nazi_outpost_add_script',
	'dynamic\monsters\churzik\lurker_hole',
	'levels\l19_def_nos_dest',
	'nosach\lurker\attack_4',
	'nosach\lurker\attack_1',
	'monster\lurker_deathbyfire',
	'monster\vsref_lurker_deathbyfire',
	'levels\l18_depot\vsref_mr_nos_attach',
	'dynamic\levels\l20_deshence\flare',
	'temp\cprhicks\l16_npc_light_control',
	'temp\cprhicks\l16_alarm_lamp_nazi',
	'temp\cprhicks\l16_npc_light_support_mental',
	'temp\cprhicks\l16_npc_light_support',
	'temp\cprhicks\l16_alarm_lamp_red',
	'temp\cprhicks\l16_turn_off_fonarik',
	'human\360_melee_attack',
	'monster\vsref_nosalis_special_l12_male_deathbyfire',
	'levels\l12_ghosts_anomaly_burn',
	'dynamic\monsters\anomaly\anomaly',
	'dynamic\objects\car\vsref_car_snow_03',
	'nosach\watchman\watch_melee_attack_1',
	'dynamic\monsters\murzik\vsref_wat_breath',
	'monster\watchman_deathbyfire',
	'monster\lurker_hole_l05',
	'temp\uber_alert_helper',
	'temp\yazoo\nos_step_pfx',
	'temp\yazoo\nos_step_snd',
	'dynamic\human\story_characters\strip\strip',
	'dynamic\monsters\dark\dark',
	'dynamic\monsters\churzik\churzik',
	'nosach\nos_attack_1_2_3',
	'monster\nosalis_female_deathbyfire',
	'dynamic\monsters\nosach\nosach',
	'dynamic\monsters\rat\rat',
	'nosach\run_far_knife',
	'temp\nosalis_npc_short_attack',
	'temp\nosalis_npc_long_attack',
	'levels\01_hunter\easy_ach',
	'monster\nosa_combat_sound',
	'dynamic\monsters\nosach\nosach_samec',
	'dynamic\objects\sound\vsref_water_dripl',
	'monster\aqua_female_deathbyfire',
	'dynamic\monsters\aqua_female\aqua_female_lo',
	'dynamic\monsters\nosach\vsref_nos_breath',
	'dynamic\monsters\murzik\vsref_wat_landing_snow2',
	'temp\arahnid_melee',
	'temp\arahind_npc_long_attack',
	'temp\arahnid_short_attack',
	'temp\ragdoll_death_control',
	'dynamic\monsters\darkchild\darkchild',
	'dynamic\monsters\aqua_male\aqua_male',
	'temp\nosalis_npc_monster_short_attack',
	'temp\nosalis_npc_monster_long_attack',
	'dynamic\monsters\aqua_female\aqua_female_lg\aqua_female_lg',
	'temp\watchman_npc_monster_long_attack',
	'temp\watchman_npc_monster_short_attack',
	'dynamic\monsters\forest_god\forest_god_new',
	'dynamic\monsters\big_mother\big_mother',
	'human\npc_skeleton_deathbyfire',
	'human\vsref_npc_skeleton_deathbyfire',
	'monster\nosalis_male_deathbyfire_tower_no_burn',
	'monster\vsref_nosalis_male_deathbyfire_tower_no_burn',
	'dynamic\human\npc_human_deathbyfire_tower_no_burn',
	'vsref_npc_human_deathbyfire_tower_no_burn',
	'monster\nosalis_female_deathbyfire_tower_no_burn',
	'monster\vsref_nosalis_female_deathbyfire_tower_no_burn',
	'dynamic\objects\box\button_use',
	
	// hit factors presets REDUX (21)
	'hit_factors_presets_npc_actor_preset',
	'hit_factors_presets_npc_actor_heavy_preset',
	'hit_factors_presets_npc_light_armor_preset',
	'hit_factors_presets_monster_nosach_preset',
	'hit_factors_presets_npc_no_armor_preset',
	'hit_factors_presets_npc_heavy_armor_preset',
	'hit_factors_presets_monster_murzik_preset',
	'hit_factors_presets_monster_demon_preset',
	'hit_factors_presets_monster_arahnid',
	'hit_factors_presets_monster_bibliotekar_preset',
	'hit_factors_presets_mp_ranger_preset',
	'hit_factors_presets_monster_aquamale',
	'hit_factors_presets_monster_murzik_preset_strong',
	'hit_factors_presets_npc_super_armor_preset',
	'hit_factors_presets_monster_lurker_preset',
	'hit_factors_presets_monster_nosach_female_preset',
	'hit_factors_presets_bear',
	'hit_factors_presets_rhino',
	'hit_factors_presets_npc_heavy_armor_preset_no_hs',
	'hit_factors_presets_npc_light_armor_preset_no_hs',
	'hit_factors_presets_mp_heavy_preset',
	
	// visual scripts REDUX (20)
	'vs\levels\2033\anim_wires',
	'vs\player\achievement_2033',
	'vs\npc\melee_start_signal',
	'vs\npc\melee_stun_hit_relative_signal',
	'vs\npc\melee_kill_hit_relative_signal',
	'vs\levels\2033\l32_poezd',
	'vs\levels\2033\demon_roof_attack_new',
	'vs\levels\2033\l20_lurhole_explosion',
	'vs\levels\2033\14_lamp',
	'vs\levels\2033\14_ghost',
	'vs\levels\2033\l09_demon_simple_attack',
	'vs\levels\2033\l09_remove_watch',
	'vs\levels\2033\l09_watch_pluh',
	'vs\monster\watch_howl_crit',
	'vs\traps\jestjanki_v2',
	'vs\levels\l11_undercity\l05_monster_dest',
	'vs\door\door_use_closed_wooden',
	'vs\blood_spot',
	'vs\monster\l10\demon_death_1',
	'vs\player\achievement_2034',
	
    // configs Arktika.1
    'ecostume_type',
    'upgradables_registry',
    'upgradables_registry_ovr',
    
    // visual scripts Arktika.1 (69)
    'human\vsref_npc_human_deathbyfire',
    'mishen_hit',
    'npc_sound_hitmarks_vr',
    'sf4_col_maniken',
    'subscripts\weapon\modular_gun_module_fx',
    'temp\3523',
    'temp\blink',
    'temp\butilka_hit',
    'temp\death_from_laser',
    'temp\death_from_laser_sniper',
    'temp\death_from_laser_vr_sniper',
    'temp\death_from_plasma',
    'temp\dron_target',
    'temp\for_hunter_attach',
    'temp\hit_sf4_col',
    'temp\lifgr_tree',
    'temp\oculus\ach_box_killer',
    'temp\oculus\ach_killer',
    'temp\oculus\achievement_oculus_new',
    'temp\oculus\fire_extinguisher_npc',
    'temp\oculus\fudo_shot',
    'temp\oculus\hit_mark_sfrom_small',
    'temp\oculus\key_combat_2npc',
    'temp\oculus\movement_type_change',
    'temp\oculus\multi_hit_rsnake',
    'temp\oculus\npc_breath',
    'temp\oculus\npc_grenade_targeting',
    'temp\oculus\npc_grenade_targeting_heavy',
    'temp\oculus\npc_grenade_targeting_heavy_vr',
    'temp\oculus\npc_grenade_targeting_sniper',
    'temp\oculus\npc_grenade_targeting_sniper_vr',
    'temp\oculus\npc_grenade_targeting_vr',
    'temp\oculus\npc_lose_of_player',
    'temp\oculus\npc_sound_hitmarks',
    'temp\oculus\npc_stun_grenade',
    'temp\oculus\npc_surrender',
    'temp\oculus\npc_torch_reaction',
    'temp\oculus\oculus_stars',
    'temp\oculus\perk_all_die',
    'temp\oculus\sniper_icon',
    'temp\oculus\sniper_radar',
    'temp\oculus\stun_death_check',
    'temp\oculus\stun_from_freccia',
    'temp\oculus\torch_player_check',
    'temp\oculus\vr_mission_cube_01_generic',
    'temp\oculus\vr_mission_cube_01_generic_no_move',
    'temp\oculus\vr_mission_cube_01_simple',
    'temp\oculus\vr_mission_cube_02',
    'temp\oculus\vr_mission_cube_perf',
    'temp\oculus_challenges',
    'temp\oculus_monster_death',
    'temp\sound_support',
    'temp\throwable_object',
    'temp\trail_for_dron_engine',
    'temp\yazoo\attach_feedback',
    'vs\npc\m3\role_machine_gunner_oculus',
    'vs\player\bullets_dirt',
    'vs\player\civil_station',
    'vs\player\crouch_control_dlc1',
    'vs\player\death',
    'vs\player\end_level',
    'vs\player\jumpover',
    'vs\player\karma',
    'vs\player\map_task_2033',
    'vs\player\metro3\binocular',
    'vs\player\metro3\bracer',
    'vs\player\metro3\general_rules',
    'vs\player\safe',
    'vs\player\weapon',
    
    // levels Arktika.1
    'oculus\pc_01_citadel',
    'oculus\pc_02_subway_phase_1',
    'oculus\pc_03_airport_phase_1',
    'oculus\pc_04_base_phase_1',
    'oculus\pc_04_base_phase_2',
    'oculus\pc_05_mall_phase_1',
    'oculus\pc_05_mall_phase_2',
    'oculus\pc_06_railstation_phase_1',
    'oculus\pc_06_railstation_phase_2',
    'oculus\pc_07_baggages_phase_1',
    'oculus\pc_07_baggages_phase_2',
    
    // static data Arktika.1 (104)
    'static_data\4f830e36_4f830e36_4e4e2e53', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_barrel
    'static_data\0f10b43b_0f10b43b_dd755ca5', // static_data\o_entity__g.config.entity.entity
    'static_data\0cbbe8a6_0cbbe8a6_7b9fa65a', // static_data\o_hlamp__g.config.entity.hanging_lamp
    'static_data\46985674_46985674_653d893f', // static_data\effect__g.config.entity.effect
    'static_data\05275e6e_05275e6e_4f2ab9f8', // static_data\o_basezone__g.config.entity.restrictor
    'static_data\b42ea669_b42ea669_6dfe6770', // static_data\patrol_point__g.config.entity.patrol_point
    'static_data\2301c4ef_2301c4ef_539d30e8', // static_data\staticprop__g.config.entity.static
    'static_data\4f830e36_4f830e36_ae5e1359', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_rotor_em
    'static_data\4f830e36_4f830e36_5b68eda5', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_barrel_damage
    'static_data\4f830e36_4f830e36_cdc7c4fb', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_rotor_plasma
    'static_data\4f830e36_4f830e36_463aefa7', // static_data\weapon_item_vr_attach__g.config.entity.vr_attach_base
    'static_data\4f830e36_4f830e36_df49be8c', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_ammo_laser_ricochet
    'static_data\4f830e36_4f830e36_3bc02e0c', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_barrel_accuracy
    'static_data\945fd87c_945fd87c_d671f740', // static_data\weapon_hellbreath__g.config.entity.wpn_lpt
    'static_data\4f830e36_4f830e36_69f565e3', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_magazine
    'static_data\4f830e36_4f830e36_779dfdd6', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_battery
    'static_data\6acd63fa_6acd63fa_ef04f459', // static_data\vr_entity__g.config.entity.vr_entity
    'static_data\4f830e36_4f830e36_c57d11ed', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_cylinder
    'static_data\f7fc7e58_f7fc7e58_5da11d93', // static_data\vr_weapon_sf4__g.config.entity.wpn_sf4ag_gun
    'static_data\4df96cbf_4df96cbf_e71c623d', // static_data\virtual_monitor__g.config.entity.virtual_monitor
    'static_data\46985674_46985674_c418c458', // static_data\effect__g.config.entity.dummy
    'static_data\84260b0e_84260b0e_f89effd9', // static_data\proxy__g.config.entity.proxy
    'static_data\391ab8a3_391ab8a3_1abf67e8', // static_data\turret__g.config.entity.turret
    'static_data\4015029a_4015029a_80e3ed61', // static_data\force_field__g.config.entity.force_field
    'static_data\54571bfa_54571bfa_fd805e46', // static_data\visualscript__g.config.entity.vs
    'static_data\68e1bde3_68e1bde3_4b4462a8', // static_data\player__g.config.entity.player
    'static_data\d91a939e_d91a939e_a366b3de', // static_data\ammo__g.config.entity.ammo_15mm_sabot
    'static_data\d91a939e_d91a939e_172a1e1e', // static_data\ammo__g.config.entity.ammo_12x70mm
    'static_data\5aeb1d81_5aeb1d81_cd2679e2', // static_data\players_hands__g.config.entity.players_hands
    'static_data\a7f76a1e_a7f76a1e_d8e29a66', // static_data\o_helpertext__g.config.entity.text_helper
    'static_data\89207469_89207469_81c7a738', // static_data\mech_entity__g.config.entity.mech_entity
    'static_data\e74bf56a_e74bf56a_e4d85d1f', // static_data\weapon_padonag__g.config.entity.wpn_stechkin
    'static_data\b616569a_b616569a_11a4fe47', // static_data\weapon_dynamite__g.config.entity.vr_decoy_npc
    'static_data\a2812f11_a2812f11_f5c81f23', // static_data\weapon_duplet__g.config.entity.wpn_e_mag_oculus
    'static_data\9151aa6f_9151aa6f_f7eb5312', // static_data\npc_fx__g.config.entity.npc_friend_fx
    'static_data\c6bc8b79_c6bc8b79_7867f577', // static_data\anomaly__g.config.entity.simple_anomaly
    'static_data\ea2ae739_ea2ae739_5eeedf12', // static_data\vr_weapon_modular__g.config.entity.wpn_modular_gun
    'static_data\021b4a02_021b4a02_cfcdcfd6', // static_data\o_helpertext_vr_info__g.config.entity.text_helper_vr_info
    'static_data\945fd87c_945fd87c_40384ab8', // static_data\weapon_hellbreath__g.config.entity.wpn_rsnake
    'static_data\9feecd57_9feecd57_fde89fd0', // static_data\torchlight_upgradable__g.config.entity.torchlight_upgradable
    'static_data\9151aa6f_9151aa6f_fe5690a7', // static_data\npc_fx__g.config.entity.npc_enemy_fx
    'static_data\945fd87c_945fd87c_c56c7186', // static_data\weapon_hellbreath__g.config.entity.wpn_rsnake_drone
    'static_data\77af1ab6_77af1ab6_df684305', // static_data\o_scaling_entity__g.config.entity.scaling_entity
    'static_data\e74bf56a_e74bf56a_67bad31e', // static_data\weapon_padonag__g.config.entity.wpn_spray_6
    'static_data\03016a46_03016a46_0be6b917', // static_data\soft_entity__g.config.entity.soft_entity
    'static_data\e74bf56a_e74bf56a_17d02791', // static_data\weapon_padonag__g.config.entity.wpn_spray_3
    'static_data\d91a939e_d91a939e_bc62ab9b', // static_data\ammo__g.config.entity.ammo_545x39_fmj
    'static_data\e74bf56a_e74bf56a_60d71707', // static_data\weapon_padonag__g.config.entity.wpn_spray_2
    'static_data\e74bf56a_e74bf56a_feb382a4', // static_data\weapon_padonag__g.config.entity.wpn_spray_5
    'static_data\8d41ffd3_8d41ffd3_5702e2dc', // static_data\effectm__g.config.entity.effectm
    'static_data\b0584545_b0584545_ce66eb07', // static_data\weapon_flame_dynamite__g.config.entity.plasma_ball_expl_alt_ult
    'static_data\b0584545_b0584545_1026b2e2', // static_data\weapon_flame_dynamite__g.config.entity.plasma_ball_expl_alt_med
    'static_data\b0584545_b0584545_6fb57181', // static_data\weapon_flame_dynamite__g.config.entity.plasma_ball_expl_alt_low
    'static_data\b0584545_b0584545_ca96f994', // static_data\weapon_flame_dynamite__g.config.entity.plasma_ball_expl_alt_high
    'static_data\15583d02_15583d02_c2617beb', // static_data\helsing_arrow__g.config.entity.ammo_sf4ag_arrow_pin
    'static_data\0dd68074_0dd68074_881f17d7', // static_data\vr_weapon__g.config.entity.vr_weapon
    'static_data\3a55b380_3a55b380_a67be6c8', // static_data\flexible_entity__g.config.entity.flexible_entity
    'static_data\b574ea5d_b574ea5d_7f21d1cc', // static_data\o_explosion__g.config.entity.explosion
    'static_data\6b10940e_6b10940e_84524940', // static_data\virtual_camera__g.config.entity.virtual_camera
    'static_data\b574ea5d_b574ea5d_f48bb5e6', // static_data\o_explosion__g.config.entity.explosion_oculus
    'static_data\b574ea5d_b574ea5d_f41c6835', // static_data\o_explosion__g.config.entity.explosion_oculus_drone
    'static_data\d91a939e_d91a939e_c8aefcf3', // static_data\ammo__g.config.entity.ammo_15mm
    'static_data\a2812f11_a2812f11_da11c7f9', // static_data\weapon_duplet__g.config.entity.wpn_sf4ag
    'static_data\a2812f11_a2812f11_02fbdf25', // static_data\weapon_duplet__g.config.entity.wpn_punisher_oculus
    'static_data\eea4510f_eea4510f_3fc6bf78', // static_data\effect_pausable__g.config.entity.effect_pausable
    'static_data\daef9a87_daef9a87_46c1cfcf', // static_data\scripted_entity__g.config.entity.scripted_entity
    'static_data\e66cf1c4_e66cf1c4_6dd970ff', // static_data\grab_zone__g.config.entity.grab_zone
    'static_data\15583d02_15583d02_2ec16f2b', // static_data\helsing_arrow__g.config.entity.ammo_sf4ag_arrow
    'static_data\01437105_01437105_1ac1430e', // static_data\vr_cube__g.config.entity.vr_cube
    'static_data\71871a27_71871a27_b786199b', // static_data\weapon_ak_74__g.config.entity.wpn_ak_74_ng
    'static_data\9a1939a3_9a1939a3_85a4d288', // static_data\virtual_hand__g.config.entity.virtual_hand
    'static_data\8f22a0ed_8f22a0ed_f39a543a', // static_data\woman__g.config.entity.woman
    'static_data\1c0e989a_1c0e989a_5ffb5fa5', // static_data\magnetic_holster__g.config.entity.magnetic_holster
    'static_data\57b8e36c_57b8e36c_39d5170f', // static_data\vr_weapon_carver__g.config.entity.wpn_baranka
    'static_data\b0584545_b0584545_5985dbc3', // static_data\weapon_flame_dynamite__g.config.entity.plasma_ball_expl
    'static_data\e74bf56a_e74bf56a_90908d34', // static_data\weapon_padonag__g.config.entity.wpn_revolver_vr
    'static_data\1eb40ac1_1eb40ac1_b5bf7485', // static_data\teleport__g.config.entity.teleport
    'static_data\13e11d26_13e11d26_9c9b4010', // static_data\weapon_saiga__g.config.entity.wpn_saiga
    'static_data\91551e3d_91551e3d_650ed131', // static_data\weapon_ventil__g.config.entity.wpn_ventil
    'static_data\945fd87c_945fd87c_da3a8ba7', // static_data\weapon_hellbreath__g.config.entity.wpn_fudo_hell
    'static_data\e8ca57b3_e8ca57b3_229f6c22', // static_data\o_waterzone__g.config.entity.waterzone
    'static_data\71871a27_71871a27_685a059b', // static_data\weapon_ak_74__g.config.entity.wpn_ak_74
    'static_data\e74bf56a_e74bf56a_217b03ab', // static_data\weapon_padonag__g.config.entity.wpn_spray
    'static_data\71871a27_71871a27_702644db', // static_data\weapon_ak_74__g.config.entity.wpn_ak74_oculus_griz
    'static_data\90c81990_90c81990_dabcb7af', // static_data\humanimal__g.config.entity.humanimal
    'static_data\b616569a_b616569a_abf295fc', // static_data\weapon_dynamite__g.config.entity.wpn_humanimal_stuff
    'static_data\151dc865_151dc865_65d5e65c', // static_data\rat__g.config.entity.rat
    'static_data\a6d94122_a6d94122_e312ab02', // static_data\big_mother__g.config.entity.ruckus
    'static_data\b0584545_b0584545_a9e80d6b', // static_data\weapon_flame_dynamite__g.config.entity.wpn_flame_grenage
    'static_data\aa177401_aa177401_53549294', // static_data\staticprop_breakable__g.config.entity.static_breakable
    'static_data\27dcdaf2_27dcdaf2_862a2a69', // static_data\o_aipoint__g.config.entity.ai_point
    'static_data\0648d18a_0648d18a_0a5f7b5a', // static_data\o_helpertext_counter__g.config.entity.text_helper_counter
    'static_data\945fd87c_945fd87c_94e4093b', // static_data\weapon_hellbreath__g.config.entity.wpn_fudo_hell_jeremie
    'static_data\16f5ef2e_16f5ef2e_aef34af2', // static_data\o_anim_entity__g.config.entity.anim_entity
    'static_data\71871a27_71871a27_7b43ba6d', // static_data\weapon_ak_74__g.config.entity.wpn_ak74_oculus_plasma_rifle
    'static_data\7740aebc_7740aebc_f83af38a', // static_data\weapon_flare__g.config.entity.wpn_flare
    'static_data\b5b0650c_b5b0650c_55b0bc3d', // static_data\weapon_gatling__g.config.entity.wpn_gatling_l17_pasha
    'static_data\b574ea5d_b574ea5d_d2cf4d05', // static_data\o_explosion__g.config.entity.explosion_oculus_rocket
    'static_data\90c81990_90c81990_c35b054f', // static_data\humanimal__g.config.entity.humanimal_oculus
    'static_data\9861566b_9861566b_056dc363', // static_data\weapon_decoy__g.config.entity.vr_decoy
    'static_data\e74bf56a_e74bf56a_89b4b232', // static_data\weapon_padonag__g.config.entity.wpn_spray_4
    'static_data\57174204_57174204_69bf2010', // static_data\weapon_ashot__g.config.entity.wpn_ashot_ng
    'static_data\71871a27_71871a27_ad2afe80', // static_data\weapon_ak_74__g.config.entity.wpn_ak74_oculus
    'static_data\9151aa6f_9151aa6f_bcc89637', // static_data\npc_fx__g.config.entity.npc_teleporting_boss
    // my invention, doesn't exist in original game
    'static_data\24c51a9c_24c51a9c_fe860793', // static_data\vehicle__g.config.entity.vehicle
    'static_data\512ec1ff_512ec1ff_76e6b55a',  // static_data\weapon_item_magazine__g.config.entity.weapon_magazine
    
	// visual scripts Exodus (793)
    'addtional_humanimal_diver_property_dlc_2',
    'alador\temp_combat_speech_rebel_after_clone',
    'borscht\08_desert_ara_qte_custom',
    'borscht\10_cannibal_sets',
    'borscht\13_deadcity_m2_ghost',
    'borscht\13_deadcity_miller_wetness',
    'borscht\cannibal_headlamp',
    'borscht\destroy_after_cloned',
    'borscht\e3_toggle_lamp',
    'borscht\m3_spartan_attach_cloner',
    'borscht\npc_helmet_detach',
    'borscht\sets_joint_deact',
    'borscht\sets_loot_destroy',
    'bridge_special_anomaly',
    'dynamic\human\effects\vsref_npc_breath',
    'dynamic\monsters\arahind\vsref_arahnid_torch',
    'dynamic\objects\boat\vsref_boat_lum_correction',
    'dynamic\objects\car\vsref_car_snow',
    'dynamic\objects\car\vsref_car_snow_03',
    'dynamic\objects\trolley\vsref_trolley_anomaly_reaction',
    'dynamic\vsref_attach_destroy',
    'dynamic\vsref_corpse_destroy',
    'dynamic\vsref_corpse_destroy_fast',
    'dynamic\wpn\vsref_weapon_destroyer',
    'elber\temp_dead_manager',
    'faith\garland_unsync',
    'faith\ny_crowd_control\crowd_man_01',
    'faith\ny_crowd_control\crowd_man_02',
    'faith\ny_crowd_control\crowd_man_03',
    'faith\ny_crowd_control\crowd_woman_01',
    'friogar\vsref_pool_enemies_mental_state',
    'friogar\vsref_pool_turn_on_off_enemies',
    'human\npc_human_deathbyfire',
    'human\vsref_attach_deathbyfire',
    'human\vsref_boat_speech_manager',
    'human\vsref_game_plus_armored_mode',
    'human\vsref_gasmask_voice',
    'human\vsref_npc_human_deathbyfire',
    'human\vsref_npc_human_deathbyfire_init',
    'human\vsref_npc_sounds',
    'human\vsref_vehicle_speech_manager',
    'jeremie\burn_preset',
    'jeremie\destroy_by_signal',
    'jeremie\jerrycan_fuel',
    'jeremie\physx_wake_up',
    'jeremie\raven_feather_takeoff',
    'jeremie\sounds_collision',
    'jeremie\sounds_collision_barrel',
    'jeremie\weapon_show_signal',
    'lar_special_manta_desert',
    'levels\12_valley\camp_camp_1',
    'levels\12_valley\camp_camp_10',
    'levels\12_valley\camp_camp_11',
    'levels\12_valley\camp_camp_12',
    'levels\12_valley\camp_camp_13',
    'levels\12_valley\camp_camp_14',
    'levels\12_valley\camp_camp_2',
    'levels\12_valley\camp_camp_3',
    'levels\12_valley\camp_camp_4',
    'levels\12_valley\camp_camp_5',
    'levels\12_valley\camp_camp_6',
    'levels\12_valley\camp_camp_7',
    'levels\12_valley\camp_camp_8',
    'levels\12_valley\camp_camp_9',
    'levels\12_valley\camp_camp_escape',
    'levels\12_valley\ch_vill_1',
    'levels\12_valley\ch_vill_10',
    'levels\12_valley\ch_vill_11',
    'levels\12_valley\ch_vill_12',
    'levels\12_valley\ch_vill_13',
    'levels\12_valley\ch_vill_14',
    'levels\12_valley\ch_vill_15',
    'levels\12_valley\ch_vill_16',
    'levels\12_valley\ch_vill_17',
    'levels\12_valley\ch_vill_18',
    'levels\12_valley\ch_vill_19',
    'levels\12_valley\ch_vill_2',
    'levels\12_valley\ch_vill_20',
    'levels\12_valley\ch_vill_3',
    'levels\12_valley\ch_vill_4',
    'levels\12_valley\ch_vill_5',
    'levels\12_valley\ch_vill_6',
    'levels\12_valley\ch_vill_7',
    'levels\12_valley\ch_vill_8',
    'levels\12_valley\ch_vill_9',
    'levels\12_valley\ch_vill_escape',
    'levels\12_valley\lake_camp_1',
    'levels\12_valley\lake_camp_10',
    'levels\12_valley\lake_camp_11',
    'levels\12_valley\lake_camp_12',
    'levels\12_valley\lake_camp_13',
    'levels\12_valley\lake_camp_14',
    'levels\12_valley\lake_camp_15',
    'levels\12_valley\lake_camp_16',
    'levels\12_valley\lake_camp_17',
    'levels\12_valley\lake_camp_2',
    'levels\12_valley\lake_camp_3',
    'levels\12_valley\lake_camp_4',
    'levels\12_valley\lake_camp_5',
    'levels\12_valley\lake_camp_6',
    'levels\12_valley\lake_camp_7',
    'levels\12_valley\lake_camp_8',
    'levels\12_valley\lake_camp_9',
    'levels\12_valley\lake_patrol_1',
    'levels\12_valley\lake_patrol_2',
    'levels\12_valley\lake_patrol_3',
    'levels\12_valley\lake_patrol_4',
    'levels\12_valley\lake_pirat_1',
    'levels\12_valley\lake_pirat_2',
    'levels\12_valley\lake_pirat_3',
    'levels\12_valley\lake_pirat_4',
    'levels\12_valley\lake_robin_1',
    'levels\12_valley\lake_robin_2',
    'levels\12_valley\lake_robin_3',
    'levels\12_valley\lake_robin_4',
    'levels\12_valley\lake_robin_5',
    'levels\12_valley\lake_robin_6',
    'levels\12_valley\sawmill_snake',
    'levels\12_valley\swamp_aqua_lum',
    'levels\12_valley\swamp_camp_01',
    'levels\12_valley\swamp_camp_02',
    'levels\12_valley\swamp_camp_03',
    'levels\12_valley\swamp_camp_04',
    'levels\12_valley\swamp_camp_05',
    'levels\12_valley\swamp_camp_06',
    'levels\12_valley\swamp_camp_07',
    'levels\12_valley\swamp_camp_08',
    'levels\12_valley\swamp_camp_09',
    'levels\12_valley\swamp_camp_10',
    'levels\12_valley\swamp_camp_11',
    'levels\12_valley\swamp_camp_12',
    'levels\12_valley\swamp_camp_13',
    'levels\12_valley\swamp_camp_14',
    'levels\12_valley\swamp_camp_15',
    'levels\12_valley\swamp_camp_16',
    'levels\dlc_bonus\arahi_destroy',
    'levels\dlc_bonus\arahi_destroy_cocon',
    'levels\dlc_bonus\arahi_small_in_cocon_burn',
    'levels\dlc_bonus\arahind_footsteps',
    'levels\dlc_bonus\arahind_gasmask_hit',
    'levels\dlc_bonus\arahind_normal_flamethrower',
    'levels\l07\l07_fly_sound',
    'levels\l07\l07_water_drippling_sound',
    'levels\l07\l07_water_falling_hard_sound',
    'levels\l07\l07_water_small_stream_sound',
    'levels\l13_dead_city\nos_melnik_long_attack',
    'levels\l13_dead_city\nos_melnik_short_attack',
    'levels\l13_dead_city\nosalis_presentation',
    'levels\l13_dead_city\snow_ghost',
    'levels\m3_01\mall_watch_act',
    'levels\m3_01\npc_weapon_dismantle_forbid',
    'levels\m3_01\watch_attack_npc_woman',
    'levels\m3_dead_city\phys_door_sound_collision',
    'levels\m3_l05\fallen_handler',
    'levels\m3_l06\06_npc_vill_civ',
    'levels\m3_l06\06_npc_vill_civ_w',
    'levels\m3_l06\borscht\06_npc_sets',
    'levels\m3_l06\borscht\bandit_tower\bandit_dagger',
    'levels\m3_l06\borscht\bandit_tower\bandit_grisha',
    'levels\m3_l06\borscht\bandit_tower\bandit_jager',
    'levels\m3_l06\borscht\bandit_tower\bandit_misha',
    'levels\m3_l06\borscht\bandit_tower\bandit_tolya',
    'levels\m3_l06\borscht\bandit_tower\hostage_bandit_tower',
    'levels\m3_l06\borscht\port_act_1\humani_dungeon',
    'levels\m3_l06\borscht\port_act_1\humani_dungeon_runner',
    'levels\m3_l06\borscht\port_act_1\humani_dungeon_water',
    'levels\m3_l06\borscht\trainyard_base\enemy_balcony',
    'levels\m3_l06\borscht\trainyard_base\enemy_bober',
    'levels\m3_l06\borscht\trainyard_base\enemy_catwalk',
    'levels\m3_l06\borscht\trainyard_base\enemy_gorlo',
    'levels\m3_l06\borscht\trainyard_base\enemy_hammers_friend',
    'levels\m3_l06\borscht\trainyard_base\enemy_outside_1',
    'levels\m3_l06\borscht\trainyard_base\enemy_outside_2',
    'levels\m3_l06\borscht\trainyard_base\enemy_sleeping_beauty',
    'levels\m3_l06\borscht\trainyard_base\enemy_subspawn_out_2',
    'levels\m3_l06\borscht\trainyard_base\enemy_zewa',
    'levels\m3_l06\borscht\trainyard_base\enemy_zhiga',
    'levels\m3_l06\bri_chr_sup_01',
    'levels\m3_l06\bri_chr_sup_02',
    'levels\m3_l06\bri_chr_sup_03',
    'levels\m3_l06\bri_civ_upper',
    'levels\m3_l06\bri_comb_01',
    'levels\m3_l06\bri_comb_02',
    'levels\m3_l06\bri_comb_03',
    'levels\m3_l06\bri_comb_04',
    'levels\m3_l06\bri_comb_05',
    'levels\m3_l06\bri_comb_06',
    'levels\m3_l06\bri_comb_07',
    'levels\m3_l06\bri_comb_08',
    'levels\m3_l06\bri_comb_09',
    'levels\m3_l06\bri_comb_10',
    'levels\m3_l06\bri_comb_11',
    'levels\m3_l06\bri_comb_12',
    'levels\m3_l06\bri_comb_13',
    'levels\m3_l06\bri_comb_14',
    'levels\m3_l06\bri_comb_15',
    'levels\m3_l06\bri_comb_16',
    'levels\m3_l06\bri_comb_17',
    'levels\m3_l06\bri_comb_18',
    'levels\m3_l06\bri_comb_19',
    'levels\m3_l06\bri_comb_20',
    'levels\m3_l06\bri_nast_cry',
    'levels\m3_l06\bri_supp_1',
    'levels\m3_l06\destroy_npc_torchlight',
    'levels\m3_l06\npc_boat_bandit_front',
    'levels\m3_l06\npc_boat_bandit_leader',
    'levels\m3_l06\npc_boat_bandit_rear',
    'levels\m3_l06\port_act2_enem1',
    'levels\m3_l06\port_act2_enem10',
    'levels\m3_l06\port_act2_enem11',
    'levels\m3_l06\port_act2_enem12',
    'levels\m3_l06\port_act2_enem13',
    'levels\m3_l06\port_act2_enem14',
    'levels\m3_l06\port_act2_enem15',
    'levels\m3_l06\port_act2_enem2',
    'levels\m3_l06\port_act2_enem3',
    'levels\m3_l06\port_act2_enem4',
    'levels\m3_l06\port_act2_enem5',
    'levels\m3_l06\port_act2_enem6',
    'levels\m3_l06\port_act2_enem7',
    'levels\m3_l06\port_act2_enem8',
    'levels\m3_l06\port_act2_enem9',
    'levels\m3_l06\port_act2_enem_boss',
    'levels\m3_l06\port_act2_enem_reinforcement_1',
    'levels\m3_l06\port_act2_enem_reinforcement_2',
    'levels\m3_l06\port_act2_enem_reinforcement_3',
    'levels\m3_l06\port_act2_enem_reinforcement_4',
    'levels\m3_l06\port_act2_idiot',
    'levels\m3_l06\port_act2_knyaz',
    'levels\m3_l06\port_act2_krest',
    'levels\m3_l06\port_act2_sam',
    'levels\m3_l06\vill_enem_0000',
    'levels\m3_l06\vill_enem_0001',
    'levels\m3_l06\vill_enem_0002',
    'levels\m3_l06\vill_enem_0003',
    'levels\m3_l06\vill_enem_0004',
    'levels\m3_l06\vill_enem_0005',
    'levels\m3_l06\vill_enem_0006',
    'levels\m3_l06\vill_enem_0007',
    'levels\m3_l06\vill_enem_0008',
    'levels\m3_l06\vill_enem_tower_1',
    'levels\m3_l06\vill_enem_tower_2',
    'levels\m3_l06\vill_enem_tower_3',
    'levels\m3_l07\yama_control_monitors',
    'levels\m3_l07\yama_control_warning_lights',
    'levels\m3_l07\yama_meeting_cleaner',
    'levels\m3_l08\borscht\canyon_dog_eating',
    'levels\m3_l08\borscht\canyon_dog_subspawn1',
    'levels\m3_l08\borscht\canyon_dog_subspawn2',
    'levels\m3_l08\borscht\canyon_dog_surround_1',
    'levels\m3_l08\borscht\enemy_canyon_arch',
    'levels\m3_l08\borscht\enemy_canyon_arch2',
    'levels\m3_l08\borscht\enemy_canyon_bowl',
    'levels\m3_l08\borscht\enemy_canyon_cliff',
    'levels\m3_l08\borscht\enemy_canyon_subspawn1',
    'levels\m3_l08\borscht\enemy_canyon_subspawn2',
    'levels\m3_l08\borscht\enemy_canyon_subspawn3',
    'levels\m3_l08\borscht\enemy_canyon_subspawn4',
    'levels\m3_l08\borscht\enemy_canyon_subspawn5',
    'levels\m3_l08\borscht\enemy_oasis_dog_guard',
    'levels\m3_l08\borscht\enemy_oasis_guard_gate',
    'levels\m3_l08\borscht\enemy_oasis_low_guard_1',
    'levels\m3_l08\borscht\enemy_oasis_low_guard_2',
    'levels\m3_l08\borscht\enemy_oasis_sentry_1',
    'levels\m3_l08\borscht\enemy_oasis_sentry_2',
    'levels\m3_l08\borscht\enemy_oasis_sentry_3',
    'levels\m3_l08\borscht\enemy_oasis_sentry_gate',
    'levels\m3_l08\borscht\enemy_oasis_warden',
    'levels\m3_l08\borscht\enemy_quicksand_reaction_universal',
    'levels\m3_l08\borscht\floating_object',
    'levels\m3_l08\borscht\humanimal_vs_human_long',
    'levels\m3_l08\borscht\humanimal_vs_human_short',
    'levels\m3_l08\borscht\oasis_damir',
    'levels\m3_l08\borscht\oasis_dog_sleeping',
    'levels\m3_l08\borscht\oasis_dog_tribals',
    'levels\m3_l08\borscht\oasis_tribal_bucket',
    'levels\m3_l08\borscht\oasis_tribal_cargo_loader',
    'levels\m3_l08\borscht\oasis_tribal_driver',
    'levels\m3_l08\borscht\oasis_tribal_warden',
    'levels\m3_l08\borscht\player_quicksand_effects',
    'levels\m3_l08\brief3_imdead',
    'levels\m3_l08\human_vehicle_evade',
    'levels\m3_l08\lighthouse\lh_up_logic',
    'levels\m3_l08\oil_outpost\outpost_garrison_1',
    'levels\m3_l08\oil_outpost\outpost_garrison_10',
    'levels\m3_l08\oil_outpost\outpost_garrison_11',
    'levels\m3_l08\oil_outpost\outpost_garrison_12',
    'levels\m3_l08\oil_outpost\outpost_garrison_13',
    'levels\m3_l08\oil_outpost\outpost_garrison_2',
    'levels\m3_l08\oil_outpost\outpost_garrison_3',
    'levels\m3_l08\oil_outpost\outpost_garrison_4',
    'levels\m3_l08\oil_outpost\outpost_garrison_5',
    'levels\m3_l08\oil_outpost\outpost_garrison_6',
    'levels\m3_l08\oil_outpost\outpost_garrison_7',
    'levels\m3_l08\oil_outpost\outpost_garrison_8',
    'levels\m3_l08\oil_outpost\outpost_garrison_9',
    'levels\m3_l08\oil_outpost\outpost_perimeter_1',
    'levels\m3_l08\oil_outpost\outpost_perimeter_2',
    'levels\m3_l08\oil_outpost\outpost_perimeter_3',
    'levels\m3_l08\oil_outpost\outpost_perimeter_4',
    'levels\m3_l08\oil_outpost\outpost_perimeter_5',
    'levels\m3_l08\oil_outpost\outpost_perimeter_6',
    'levels\m3_l08\oil_outpost\outpost_perimeter_7',
    'levels\m3_l08\oil_outpost\outpost_perimeter_8',
    'levels\m3_l08\oil_outpost\outpost_slave_1',
    'levels\m3_l08\oil_outpost\outpost_slave_2',
    'levels\m3_l08\oil_outpost\outpost_slave_3',
    'levels\m3_l08\oil_terminal\vsref_desert_terminal',
    'levels\m3_l08\oilrig_beater',
    'levels\m3_l08_desert\08_cannibal_sets',
    'levels\m3_l10_yamantau\cannibal_anim_death_fireplace',
    'levels\m3_l10_yamantau\ym_2nd_run_beh',
    'levels\m3_l10_yamantau\ym_can_dead',
    'levels\m3_l10_yamantau\ym_fireplace_can_yz',
    'levels\m3_l10_yamantau\ym_idi_targets',
    'levels\m3_l10_yamantau\ym_mill_targets',
    'levels\m3_l10_yamantau\ym_pl2_targets',
    'levels\m3_l10_yamantau\ym_pl_range_tgt',
    'levels\m3_l10_yamantau\ym_sam_targets',
    'levels\m3_l10_yamantau\ym_scn_support',
    'levels\m3_l13\corpse_footstep_reaction',
    'levels\m3_l13\l13_m1_5_corpse_shake',
    'levels\m3_l13\nosalis_jump_out',
    'levels\m3demo\tree_destructible',
    'levels\m3demo\tree_destructible_bush',
    'levels\m3demo\tree_destructible_cc',
    'levels\m3demo\tree_destructible_fence',
    'm3_dlc_1\dlc1_civilian_kid_model_manager',
    'm3_dlc_1\dlc1_civilian_model_manager',
    'm3_dlc_1\dlc1_crowd_manager',
    'm3_dlc_1\dlc1_dead_manager',
    'm3_dlc_1\dlc1_loot_corpse_random_manager',
    'm3_dlc_1\npc_matrix\dlc1_civilian_from_set',
    'm3_dlc_1\npc_matrix\dlc1_civilian_sound_scheme',
    'm3_dlc_1\npc_matrix\dlc1_npc_helmet_detach',
    'm3_dlc_2\dlc2_civilian_from_set',
    'm3_dlc_2\dlc2_civilian_model_manager',
    'm3_dlc_2\dlc2_civilian_sound_scheme',
    'm3_dlc_2\dlc2_loot_corpse_random_manager',
    'm3_dlc_2\dlc2_motorboat_seat_1_hide_headrest',
    'm3_dlc_2\dlc2_motorboat_seat_2_hide_headrest',
    'm3_dlc_2\dlc2_motorboat_seat_3_hide_headrest',
    'm3_dlc_2\dlc2_motorboat_vibration',
    'm3_dlc_2\dlc2_quick_model_swapper',
    'mls\demo\01_m_w_w_1',
    'mls\demo\01_m_w_w_2',
    'mls\demo\01_m_w_w_3',
    'mls\demo\01_m_w_wa1',
    'mls\demo\01_m_w_wa2',
    'mls\demo\01_m_w_wa3',
    'mls\demo\01_m_w_wa4',
    'mls\demo\01_m_w_wa5',
    'mls\demo\01_m_w_wa6',
    'mls\demo\01_m_w_wa7',
    'mls\demo\01_m_w_wf1',
    'mls\demo\01_m_w_wf2',
    'mls\demo\01_m_w_wf3',
    'mls\demo\scr_killme_stl',
    'monster\aqua_female_armor',
    'monster\aqua_male_armor',
    'monster\arahnid_death_melee',
    'monster\arahnid_on_shoot_kill',
    'monster\arahnid_vs_player_falling',
    'monster\arahnid_vs_player_onfoot',
    'monster\arahnid_vs_player_onfoot_kill',
    'monster\blind_vs_player_onfoot',
    'monster\blind_vs_player_v2',
    'monster\dog_vs_player_falling',
    'monster\dog_vs_player_onfoot',
    'monster\dog_vs_player_onfoot_kill',
    'monster\humanimal_vs_player_falling',
    'monster\humanimal_vs_player_onfoot_kill',
    'monster\humanimal_vs_player_short',
    'monster\humanimal_vs_player_short_kill',
    'monster\lurker_vs_player_falling',
    'monster\lurker_vs_player_onfoot',
    'monster\lurker_vs_player_onfoot_kill',
    'monster\monster_vsref_biom_activity_frog_lair',
    'monster\nosach_vs_player_falling',
    'monster\nosach_vs_player_onfoot',
    'monster\nosach_vs_player_onfoot_kill',
    'monster\vsref_aggro_melee',
    'monster\vsref_aggro_range',
    'monster\vsref_ai_sound',
    'monster\vsref_anomaly_disable_save',
    'monster\vsref_anomaly_logic',
    'monster\vsref_aqua_female_deathbyfire_init',
    'monster\vsref_aqua_female_immunities',
    'monster\vsref_aqua_female_land_behaviour',
    'monster\vsref_aqua_female_land_boat_reaction',
    'monster\vsref_aqua_female_sounds',
    'monster\vsref_aqua_female_state_controll',
    'monster\vsref_aqua_male_deathbyfire',
    'monster\vsref_aqua_male_deathbyfire_init',
    'monster\vsref_aqua_male_immunities',
    'monster\vsref_aqua_male_mustache_controll',
    'monster\vsref_aqua_male_sounds',
    'monster\vsref_aquas_armor_attach',
    'monster\vsref_aquas_armor_attach_02',
    'monster\vsref_aquas_armor_attach_03',
    'monster\vsref_aquas_depth_control',
    'monster\vsref_aquas_vfx_control',
    'monster\vsref_arachnid_deathbyfire_init',
    'monster\vsref_arachnid_deathbyfire_init_dlc_2',
    'monster\vsref_arachnid_effects',
    'monster\vsref_arachnid_emp_effects',
    'monster\vsref_arachnid_group_behaviour',
    'monster\vsref_arachnid_group_behaviour_dlc_2',
    'monster\vsref_arachnid_hud_sounds',
    'monster\vsref_arachnid_immunities',
    'monster\vsref_arachnid_main_behaviour',
    'monster\vsref_arachnid_panzer',
    'monster\vsref_arachnid_sets',
    'monster\vsref_arachnid_sets_dlc_2',
    'monster\vsref_arahnid_deathbyfire',
    'monster\vsref_arahnid_fatality',
    'monster\vsref_arahnid_fatality_dlc_2',
    'monster\vsref_biom_activity_aqua_female_lair',
    'monster\vsref_biom_activity_aqua_male_lair',
    'monster\vsref_biom_activity_attack',
    'monster\vsref_biom_activity_bug_lair',
    'monster\vsref_biom_activity_deer_lair',
    'monster\vsref_biom_activity_demon_bat',
    'monster\vsref_biom_activity_demon_lair',
    'monster\vsref_biom_activity_horde',
    'monster\vsref_biom_activity_humanimal_lair',
    'monster\vsref_biom_activity_lurker_lair',
    'monster\vsref_biom_activity_rabbit_lair',
    'monster\vsref_biom_activity_rat_lair',
    'monster\vsref_biom_activity_seagull_ground_lair',
    'monster\vsref_biom_activity_snake_lair',
    'monster\vsref_biom_activity_watchman_lair',
    'monster\vsref_biom_activity_wolf_lair',
    'monster\vsref_biom_hide_logic',
    'monster\vsref_blind_deathbyfire_init',
    'monster\vsref_blind_deathbyfire_init_dlc',
    'monster\vsref_blind_flamethrower_logic',
    'monster\vsref_blind_sound',
    'monster\vsref_bug_biom_spawn',
    'monster\vsref_bug_deathbyfire_init',
    'monster\vsref_bug_fear',
    'monster\vsref_bug_foot_kill',
    'monster\vsref_bug_random_scale',
    'monster\vsref_bullets_reaction',
    'monster\vsref_cannibal_coctail',
    'monster\vsref_cannibal_deathbyfire_init',
    'monster\vsref_cannibal_sounds',
    'monster\vsref_corpse_none',
    'monster\vsref_crit_on_butt_control',
    'monster\vsref_crosshair_control',
    'monster\vsref_deer_deathbyfire_init',
    'monster\vsref_deer_sounds',
    'monster\vsref_demon_deathbyfire_init',
    'monster\vsref_demon_hit_by_uaz',
    'monster\vsref_demon_immunities',
    'monster\vsref_demon_sounds',
    'monster\vsref_dog_deathbyfire_init',
    'monster\vsref_dog_entity_deathbyfire_init',
    'monster\vsref_dog_sounds',
    'monster\vsref_fly_corpse_spawn',
    'monster\vsref_fly_logic',
    'monster\vsref_food_creatures',
    'monster\vsref_food_set_pose_01',
    'monster\vsref_food_set_pose_02',
    'monster\vsref_frog_effects',
    'monster\vsref_frog_smash_it_by_foot',
    'monster\vsref_game_plus_tough_creatures',
    'monster\vsref_hud_butt',
    'monster\vsref_human_hit_by_uaz',
    'monster\vsref_humanimal_ambush',
    'monster\vsref_humanimal_deathbyfire',
    'monster\vsref_humanimal_deathbyfire_init',
    'monster\vsref_humanimal_diver',
    'monster\vsref_humanimal_drooling',
    'monster\vsref_humanimal_effects',
    'monster\vsref_humanimal_hit_by_uaz',
    'monster\vsref_humanimal_immunities',
    'monster\vsref_humanimal_retreat',
    'monster\vsref_humanimal_sounds',
    'monster\vsref_init_matrix_deactivate',
    'monster\vsref_lurker_deathbyfire_init',
    'monster\vsref_lurker_effects',
    'monster\vsref_lurker_effects_dig',
    'monster\vsref_lurker_hit_by_uaz',
    'monster\vsref_lurker_scheme_control',
    'monster\vsref_lurker_sounds',
    'monster\vsref_lurker_species_control',
    'monster\vsref_maggot_hand_logic',
    'monster\vsref_mantabat_bat_behaviour',
    'monster\vsref_mantabat_buhanka_spawner',
    'monster\vsref_mantabat_deathbyfire_init',
    'monster\vsref_mantabat_effects',
    'monster\vsref_mantabat_land_behaviour',
    'monster\vsref_mantabat_sounds',
    'monster\vsref_mantademon_flock_logic',
    'monster\vsref_mantademon_horde_attack_02',
    'monster\vsref_mantademon_horde_attack_03',
    'monster\vsref_mantademon_horde_come',
    'monster\vsref_mantademon_horde_come_02',
    'monster\vsref_mantademon_horde_ep03_logic',
    'monster\vsref_mantademon_horde_forward',
    'monster\vsref_mantademon_horde_forward_ep03',
    'monster\vsref_mantademon_horde_logic',
    'monster\vsref_mantademon_horde_logic_02',
    'monster\vsref_mantademon_huge_horde_logic',
    'monster\vsref_mantademon_logic',
    'monster\vsref_mantademon_tree_reaction',
    'monster\vsref_middle_worm_ach_antibiotic',
    'monster\vsref_middle_worm_deathbyfire_init',
    'monster\vsref_middle_worm_loot',
    'monster\vsref_monster_dead_eyes',
    'monster\vsref_monster_hit_by_trolley',
    'monster\vsref_monster_level_modifiers_aqua_female',
    'monster\vsref_monster_level_modifiers_aqua_male',
    'monster\vsref_monster_level_modifiers_buggy',
    'monster\vsref_monster_level_modifiers_demon',
    'monster\vsref_monster_level_modifiers_humanimal',
    'monster\vsref_monster_level_modifiers_lurker',
    'monster\vsref_monster_level_modifiers_mantabats',
    'monster\vsref_monster_level_modifiers_rabbit',
    'monster\vsref_monster_level_modifiers_raven',
    'monster\vsref_monster_level_modifiers_snake',
    'monster\vsref_monster_level_modifiers_watchman',
    'monster\vsref_monster_sputum',
    'monster\vsref_nosalis_male_attacks_controll',
    'monster\vsref_nosalis_male_deathbyfire_init',
    'monster\vsref_nosalis_male_deathbyfire_init_dlc',
    'monster\vsref_nosalis_male_flamethrower',
    'monster\vsref_nosalis_sounds',
    'monster\vsref_npc_collision_control',
    'monster\vsref_rabbit_deathbyfire_init',
    'monster\vsref_rabbit_effects',
    'monster\vsref_rabbit_sounds',
    'monster\vsref_rat_deathbyfire_init',
    'monster\vsref_rat_sounds',
    'monster\vsref_raven_boat_spawner',
    'monster\vsref_raven_boat_spawner_init',
    'monster\vsref_raven_deathbyfire',
    'monster\vsref_raven_deathbyfire_init',
    'monster\vsref_raven_effects',
    'monster\vsref_raven_land_behaviour',
    'monster\vsref_raven_sounds',
    'monster\vsref_raven_trolley_spawner',
    'monster\vsref_savage_ach',
    'monster\vsref_savage_deathbyfire_init',
    'monster\vsref_savage_hit_by_uaz',
    'monster\vsref_script_frog_sound',
    'monster\vsref_seagull_ground_behaviour',
    'monster\vsref_seagull_water_behaviour',
    'monster\vsref_seagull_wave_emmiter',
    'monster\vsref_slave_killed_by_player',
    'monster\vsref_slave_logic',
    'monster\vsref_smell_control',
    'monster\vsref_snake_collision',
    'monster\vsref_snake_deathbyfire_init',
    'monster\vsref_snake_hit_by_uaz',
    'monster\vsref_snake_poison_hit',
    'monster\vsref_snake_speed_control',
    'monster\vsref_snake_speed_control_dlc2',
    'monster\vsref_tumbleweed_logic',
    'monster\vsref_vehicles_friend',
    'monster\vsref_vs_battles',
    'monster\vsref_watchman_deathbyfire',
    'monster\vsref_watchman_deathbyfire_init',
    'monster\vsref_watchman_looker',
    'monster\vsref_watchman_reinforcement_activity',
    'monster\vsref_watchman_reinforcement_call',
    'monster\vsref_watchman_sounds',
    'monster\vsref_water_worm_immunities',
    'monster\vsref_wolf_deathbyfire_init',
    'monster\vsref_wolf_reinforcement_call',
    'monster\vsref_wolf_sounds',
    'monster\vsref_worm_ach_antibiotic',
    'monster\vsref_worm_death',
    'monster\vsref_worm_drooling',
    'monster\vsref_worm_effects',
    'monster\vsref_worm_effects_entity',
    'monster\vsref_worm_hit_sender',
    'monster\vsref_worm_speed_controll',
    'monster\watchman_vs_player_falling',
    'monster\watchman_vs_player_onfoot',
    'monster\watchman_vs_player_onfoot_kill',
    'monster\wolf_vs_player_bite',
    'monster\wolf_vs_player_falling',
    'monster\wolf_vs_player_jump',
    'monster\wolf_vs_player_onfoot',
    'newoin\detect_weapon',
    'nightvision_revealer',
    'nosach\watchman\watch_melee_attack_1',
    'rusik\nosalis\vsref_dlc01_fake_run',
    'subscripts\mls\uni_karma_surrender',
    'subscripts\monk\vsref_middle_worm_death_by_fire_dlc1',
    'subscripts\monk\vsref_middle_worm_death_dlc1',
    'subscripts\monk\vsref_worm_effects_dlc1',
    'temp\aqua_effects',
    'temp\aqua_part_dead',
    'temp\blind_v3',
    'temp\blind_v4',
    'temp\cable_idle',
    'temp\cable_swing',
    'temp\check_state',
    'temp\crossbowman_melee',
    'temp\danger_checker',
    'temp\eat_behavior',
    'temp\forest_wolf',
    'temp\kosh\falling_nosalis',
    'temp\l04_catacombs_ach',
    'temp\l06_intro_fake_listen',
    'temp\lin_lent_1',
    'temp\mdn\vsref_torch_control',
    'temp\medium_hit_impulse',
    'temp\no_combat_control',
    'temp\roger\watchman_enemy_long_attack',
    'temp\roger\watchman_enemy_short_attack',
    'temp\rusik\tmp_vsref_nos_activity_novosib',
    'temp\sleep_behavior',
    'temp\sound_manager_1',
    'temp\sound_manager_dlc1',
    'temp\sound_support',
    'temp\special_sound_attack_signal',
    'temp\strong_hit_impulse',
    'temp\strong_hit_impulse_catfish',
    'temp\temp_dismantable',
    'temp\teo\vsref_firefly_controls',
    'temp\teo\vsref_firefly_res',
    'temp\teo\vsref_firefly_scare',
    'temp\teo\vsref_rat_idle',
    'temp\vs_destructable',
    'temp\walk_around_behavior',
    'temp\watch_behavior',
    'temp\watchman_woman_short_attack',
    'temp\weak_hit_impulse',
    'temp\wolf_combat_behavior',
    'temp\yazoo\attach_feedback',
    'temp\yazoo\cannibal_anim_death',
    'temp\yazoo\cannibal_anim_death_ent',
    'tony\debug_technical',
    'vs\boat\boat_marker',
    'vs\levels\08_desert\ha_cham_oasis',
    'vs\levels\08_desert\oil_kingdom_neutral_kill',
    'vs\levels\12_valley\sawmill_canister_picker',
    'vs\levels\2033\anim_wires',
    'vs\levels\dlc2\cap_trip',
    'vs\levels\dlc2\cap_trip_final',
    'vs\levels\dlc2\map_marker',
    'vs\levels\dlc2\motor_boat_marker',
    'vs\levels\dlc2\sb_npc_10',
    'vs\levels\dlc2\sb_npc_3',
    'vs\levels\dlc2\sb_npc_4',
    'vs\levels\dlc2\sb_npc_5',
    'vs\levels\dlc2\sb_npc_6',
    'vs\levels\dlc2\sb_npc_7',
    'vs\levels\dlc2\sb_npc_8',
    'vs\levels\dlc2\sb_npc_9',
    'vs\levels\dlc\monster_long_destroy',
    'vs\levels\l04_plane\demon_death',
    'vs\levels\l13_1\snow_fall_part',
    'vs\loot\loot_ammo_model',
    'vs\loot\loot_boxes_2use',
    'vs\loot\loot_boxes_filterbox',
    'vs\loot\loot_corpse_random_manager',
    'vs\loot\loot_corpse_random_manager_monster',
    'vs\loot\loot_corpse_random_manager_snow',
    'vs\loot\loot_group_plane_ammo',
    'vs\loot\loot_item__destroy_on_signal',
    'vs\loot\loot_monster',
    'vs\loot\loot_monster_dead',
    'vs\loot\loot_monster_dead_npc',
    'vs\loot\loot_monster_entity',
    'vs\loot\loot_npc_wpn__destroy_on_signal',
    'vs\loot\loot_pickup_status_token',
    'vs\loot\loot_resourses_model',
    'vs\loot\story_loot_dlc2',
    'vs\loot\story_loot_postcards',
    'vs\loot\story_loot_postcards_loader',
    'vs\loot\story_loot_toys',
    'vs\loot\story_loot_toys_train',
    'vs\monster\flies_friends',
    'vs\monster\l10\aqua_male_swim_part',
    'vs\monster\l10\aqua_script_hit',
    'vs\npc\m3\biom_monster_logic\monster_biom_attacker',
    'vs\npc\m3\biom_monster_logic\monster_biom_horde',
    'vs\npc\m3\biom_monster_logic\monster_biom_lair',
    'vs\npc\m3\demon_taxi',
    'vs\npc\m3\motion_detector_register',
    'vs\npc\m3\role_machine_gunner',
    'vs\npc\m3\role_shield_v2',
    'vs\npc\m3\slide_effect_sand',
    'vs\npc\melee_kill_zombie_control',
    'vs\npc\miller_railgun_reload_mag_props',
    'vs\npc\npc_carbid_lamp_clone',
    'vs\npc\npc_electric_lamp_clone',
    'vs\npc\npc_fonarik_clone',
    'vs\npc\npc_fonarik_helm',
    'vs\npc\npc_fonarik_lamp',
    'vs\npc\npc_melee_kill_m3_mk3',
    'vs\npc\qte_global_control',
    'vs\npc\qte_zombie_control',
    'vs\player\bullets_dirt',
    'vs\player\crouch_control',
    'vs\player\crouch_control_dlc1',
    'vs\player\death',
    'vs\player\death_dlc1',
    'vs\player\device_feedback',
    'vs\player\diary_props_dlc2',
    'vs\player\end_level',
    'vs\player\fire_zone',
    'vs\player\flamethrower_burning_effect_on_gasmask',
    'vs\player\general_rules_dlc2',
    'vs\player\jumpover',
    'vs\player\jumpover_dlc1',
    'vs\player\jumpover_dlc2',
    'vs\player\karma',
    'vs\player\karma_dlc1',
    'vs\player\karma_dlc2',
    'vs\player\level_map_dlc2',
    'vs\player\metro3\achievement_m3',
    'vs\player\metro3\achievement_m3_3_0',
    'vs\player\metro3\achievement_m3_4_0',
    'vs\player\metro3\ambient_music',
    'vs\player\metro3\ambient_music_dlc2',
    'vs\player\metro3\bino_spot_marker',
    'vs\player\metro3\biom_weather_mgr',
    'vs\player\metro3\bracer',
    'vs\player\metro3\death_dlc2',
    'vs\player\metro3\demon_taxi_player_pt',
    'vs\player\metro3\diary_props',
    'vs\player\metro3\diary_props_dlc1',
    'vs\player\metro3\dlc_level_map',
    'vs\player\metro3\end_level_rotation',
    'vs\player\metro3\fastdrop_check_dagger',
    'vs\player\metro3\fastdrop_check_decoy',
    'vs\player\metro3\fastdrop_check_dynamite',
    'vs\player\metro3\fastdrop_check_molotov',
    'vs\player\metro3\gas_zone',
    'vs\player\metro3\general_rules',
    'vs\player\metro3\general_rules_dlc1',
    'vs\player\metro3\health_part',
    'vs\player\metro3\hidden_decal_nv',
    'vs\player\metro3\input_capture',
    'vs\player\metro3\lair_crows_sound',
    'vs\player\metro3\level_map',
    'vs\player\metro3\localized_signes',
    'vs\player\metro3\new_game_plus',
    'vs\player\metro3\outfit',
    'vs\player\metro3\outfit_dlc1',
    'vs\player\metro3\photomode',
    'vs\player\metro3\player_arm_arachnid',
    'vs\player\metro3\player_demon_reactions',
    'vs\player\metro3\player_in_transport_fear',
    'vs\player\metro3\separate_hud',
    'vs\player\metro3\tutorial_hints',
    'vs\player\metro3\tutorial_hints_dlc1',
    'vs\player\no_electro_anomaly',
    'vs\player\nv_highlights',
    'vs\player\outfit_dlc2',
    'vs\player\player_hit_by_nosalis',
    'vs\player\player_melee_hit',
    'vs\player\player_movement',
    'vs\player\player_movement_dlc1',
    'vs\player\player_movement_dlc2',
    'vs\player\player_walk_on_slime',
    'vs\player\rad_zone',
    'vs\player\rad_zone_dlc1',
    'vs\player\rad_zone_dlc2',
    'vs\player\rain_zone',
    'vs\player\rain_zone_dlc1',
    'vs\player\rain_zone_dlc2',
    'vs\player\ranger_mode',
    'vs\player\rucksack',
    'vs\player\rucksack_dlc2',
    'vs\player\safe',
    'vs\player\sound_manager_dlc2',
    'vs\player\tutorial_hints_dlc2',
    'vs\player\walkie_talkie_dlc1',
    'vs\player\walkie_talkie_dlc2',
    'vs\player\water_death',
    'vs\player\water_death_dlc1',
    'vs\player\water_death_dlc2',
    'vs\player\weapon',
    'vs\player\weapon_dlc1',
    'vs\player\weapon_dlc2',
    'vs\player\wpn_dirt',
    'vs\weapon\armory_shake',
    'vs\weapon\weapon_move',
    'vs\weapon\weapon_upgrade_use',
    'vsref_seagull_sounds',
    'web_nospider_ach',
    'yz\basis_aftercloned',
    'yz\destr_electro_bamper',
    'yz\npc_radio_yz',
    'zhukov\anomaly_ara_light_reaction',
    'zhukov\vent_manager',
    
    // levels Exodus (15)
    'm3\000',
    'm3\01_dead_moscow',
    'm3\05_winter',
    'm3\06_bridge',
    'm3\06_spring',
    'm3\07_yamantau',
    'm3\08_desert',
    'm3\08_summer',
    'm3\12_autumn',
    'm3\12_valley',
    'm3\12_valley_benchmark',
    'm3\13_deadcity',
    'm3\14_outro',
    'm3\dlc_1_deadcity',
    'm3\dlc_2_vladivostok',
    
    // static data Exodus (229)
    'static_data\cea8466e_cea8466e_e13ce87d', // static_data\player_timer_upgrade__g.config.entity.timer
    'static_data\0cbbe8a6_0cbbe8a6_7b9fa65a', // static_data\o_hlamp__g.config.entity.hanging_lamp
    'static_data\46985674_46985674_653d893f', // static_data\effect__g.config.entity.effect
    'static_data\84260b0e_84260b0e_f89effd9', // static_data\proxy__g.config.entity.proxy
    'static_data\7a4790c5_7a4790c5_88fa6dcd', // static_data\metal_detector_upgrade__g.config.entity.metal_detector
    'static_data\46985674_46985674_c418c458', // static_data\effect__g.config.entity.dummy
    'static_data\55dfa468_55dfa468_5954aedc', // static_data\weapon_item_lamp_backlight__g.config.entity.weapon_item_lamp_backlight
    'static_data\2f8eb5ec_2f8eb5ec_1e1242fd', // static_data\device_upgrade__g.config.entity.design
    'static_data\8f22a0ed_8f22a0ed_f39a543a', // static_data\woman__g.config.entity.woman
    'static_data\6f437257_6f437257_b5006f58', // static_data\compass__g.config.entity.compass
    'static_data\2f8eb5ec_2f8eb5ec_5fef82a8', // static_data\device_upgrade__g.config.entity.pendulum
    'static_data\ffa46402_ffa46402_34e2d62b', // static_data\costume_upgrade__g.config.entity.luminosity
    'static_data\0f10b43b_0f10b43b_dd755ca5', // static_data\o_entity__g.config.entity.entity
    'static_data\49406b31_49406b31_7d5fe28c', // static_data\weapon_item_lamp__g.config.entity.weapon_item_lamp
    'static_data\ffa46402_ffa46402_84cac891', // static_data\costume_upgrade__g.config.entity.speed
    'static_data\4f830e36_4f830e36_463aefa7', // static_data\weapon_item_vr_attach__g.config.entity.vr_attach_base
    'static_data\ffa46402_ffa46402_34cbc89b', // static_data\costume_upgrade__g.config.entity.armor
    'static_data\ffa46402_ffa46402_7c890c93', // static_data\costume_upgrade__g.config.entity.empty_torchlight
    'static_data\ffa46402_ffa46402_0969e2c7', // static_data\costume_upgrade__g.config.entity.upgrade_extended_equip
    'static_data\ffa46402_ffa46402_70f3f4fd', // static_data\costume_upgrade__g.config.entity.bulletproof
    'static_data\68e1bde3_68e1bde3_4b4462a8', // static_data\player__g.config.entity.player
    'static_data\ffa46402_ffa46402_200d5a4b', // static_data\costume_upgrade__g.config.entity.heavy_armor
    'static_data\2301c4ef_2301c4ef_539d30e8', // static_data\staticprop__g.config.entity.static
    'static_data\2f8eb5ec_2f8eb5ec_6baa2d68', // static_data\device_upgrade__g.config.entity.radio
    'static_data\0b13293a_0b13293a_2a17ea39', // static_data\visor__g.config.entity.visor0
    'static_data\b42ea669_b42ea669_6dfe6770', // static_data\patrol_point__g.config.entity.patrol_point
    'static_data\a4c5f75e_a4c5f75e_2967cf72', // static_data\weapon_item_vr__g.config.entity.vr_upgrade_base
    'static_data\ffa46402_ffa46402_5439fa54', // static_data\costume_upgrade__g.config.entity.upgrade_helm
    'static_data\6b6195dd_6b6195dd_4d679c88', // static_data\pulsometer_upgrade__g.config.entity.pulsometer_test
    'static_data\2f8eb5ec_2f8eb5ec_352817bb', // static_data\device_upgrade__g.config.entity.battery
    'static_data\0b13293a_0b13293a_b31ebb83', // static_data\visor__g.config.entity.visor3
    'static_data\2f8eb5ec_2f8eb5ec_91e4b507', // static_data\device_upgrade__g.config.entity.device_upgrade_base
    'static_data\7b68434a_7b68434a_c9cb2f05', // static_data\weapon_item_speedloader__g.config.entity.weapon_speedloader_base
    'static_data\267ce390_267ce390_0f8b071c', // static_data\weapon_item_silencer__g.config.entity.weapon_silencer_base
    'static_data\a7f76a1e_a7f76a1e_d8e29a66', // static_data\o_helpertext__g.config.entity.text_helper
    'static_data\4bb2c43b_4bb2c43b_17afd638', // static_data\weapon_item_optic__g.config.entity.weapon_scope_base
    'static_data\284a7374_284a7374_a8b5885d', // static_data\weapon_item_preset__g.config.entity.weapon_preset_base
    'static_data\2f8eb5ec_2f8eb5ec_edba7e8e', // static_data\device_upgrade__g.config.entity.color
    'static_data\ffa46402_ffa46402_48a28bef', // static_data\costume_upgrade__g.config.entity.empty_unloading
    'static_data\ffa46402_ffa46402_db8cedbd', // static_data\costume_upgrade__g.config.entity.spartain_tier
    'static_data\ffa46402_ffa46402_edc6c0d6', // static_data\costume_upgrade__g.config.entity.empty_helmet
    'static_data\ffa46402_ffa46402_50714e59', // static_data\costume_upgrade__g.config.entity.upgrade_ammunition
    'static_data\366b8331_366b8331_bb29fa6a', // static_data\wristwatch__g.config.entity.wristwatch
    'static_data\2f8eb5ec_2f8eb5ec_990d2d99', // static_data\device_upgrade__g.config.entity.geiger_test
    'static_data\ffa46402_ffa46402_1d0edc3c', // static_data\costume_upgrade__g.config.entity.upgrade_battery
    'static_data\0b13293a_0b13293a_c4198b15', // static_data\visor__g.config.entity.visor2
    'static_data\ebc6700d_ebc6700d_a62f6772', // static_data\weapon_item__g.config.entity.weapon_item_inh
    'static_data\2f8eb5ec_2f8eb5ec_008c4699', // static_data\device_upgrade__g.config.entity.empty_clock
    'static_data\b42bc33c_b42bc33c_bccc106d', // static_data\wick_visual__g.config.entity.wick_visual
    'static_data\e408c128_e408c128_0b4a1c66', // static_data\lighter_visual__g.config.entity.lighter_visual
    'static_data\6904b9b8_6904b9b8_29903747', // static_data\map_pad_upgrade__g.config.entity.map_visual
    'static_data\2f8eb5ec_2f8eb5ec_206637c7', // static_data\device_upgrade__g.config.entity.power
    'static_data\2f8eb5ec_2f8eb5ec_efb57251', // static_data\device_upgrade__g.config.entity.charger_visual
    'static_data\3f38477d_3f38477d_821729ff', // static_data\weapon_item_ammo__g.config.entity.weapon_ammo_base
    'static_data\ffa46402_ffa46402_01d006f7', // static_data\costume_upgrade__g.config.entity.empty_armor
    'static_data\ffa46402_ffa46402_549d17cb', // static_data\costume_upgrade__g.config.entity.upgrade_glass
    'static_data\512ec1ff_512ec1ff_01f9b4ed', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_ng
    'static_data\ffa46402_ffa46402_ca279a0e', // static_data\costume_upgrade__g.config.entity.upgrade_googles
    'static_data\ffa46402_ffa46402_b6818095', // static_data\costume_upgrade__g.config.entity.diversionary_tier
    'static_data\ffa46402_ffa46402_626e34b5', // static_data\costume_upgrade__g.config.entity.chemical_tier
    'static_data\daef9a87_daef9a87_46c1cfcf', // static_data\scripted_entity__g.config.entity.scripted_entity
    'static_data\2f8eb5ec_2f8eb5ec_8f0af8c0', // static_data\device_upgrade__g.config.entity.empty_bracer
    'static_data\622c05c7_622c05c7_8002ce0b', // static_data\motion_sensor_adv_upgrade__g.config.entity.motion_detector_advanced
    'static_data\ffa46402_ffa46402_88ea5ee8', // static_data\costume_upgrade__g.config.entity.upgrade_filter
    'static_data\ffa46402_ffa46402_f114f214', // static_data\costume_upgrade__g.config.entity.light_armor
    'static_data\27fdf76a_27fdf76a_efdd81f5', // static_data\weapon_item_laser__g.config.entity.weapon_laser_base
    'static_data\ffa46402_ffa46402_e7dca934', // static_data\costume_upgrade__g.config.entity.upgrade_power
    'static_data\ffa46402_ffa46402_ae73249c', // static_data\costume_upgrade__g.config.entity.empty_unloading_spartain
    'static_data\cc785558_cc785558_a64f9847', // static_data\device_upgrade_lamp__g.config.entity.upgrade_lamp
    'static_data\ffa46402_ffa46402_ae9140ed', // static_data\costume_upgrade__g.config.entity.upgrade_extended_fastdrop
    'static_data\874c8770_874c8770_87c03a41', // static_data\motion_sensor_upgrade__g.config.entity.motion_detector
    'static_data\0b13293a_0b13293a_5d10daaf', // static_data\visor__g.config.entity.visor1
    'static_data\8d41ffd3_8d41ffd3_5702e2dc', // static_data\effectm__g.config.entity.effectm
    'static_data\05275e6e_05275e6e_4f2ab9f8', // static_data\o_basezone__g.config.entity.restrictor
    'static_data\54571bfa_54571bfa_fd805e46', // static_data\visualscript__g.config.entity.vs
    'static_data\b75365b6_b75365b6_04113d62', // static_data\weapon_tihar__g.config.entity.wpn_tihar_ng
    'static_data\27dcdaf2_27dcdaf2_862a2a69', // static_data\o_aipoint__g.config.entity.ai_point
    'static_data\d91a939e_d91a939e_bc62ab9b', // static_data\ammo__g.config.entity.ammo_545x39_fmj
    'static_data\eeffe3be_eeffe3be_0106f86f', // static_data\o_interest__g.config.entity.interest
    'static_data\9151aa6f_9151aa6f_f7eb5312', // static_data\npc_fx__g.config.entity.npc_friend_fx
    'static_data\71871a27_71871a27_b786199b', // static_data\weapon_ak_74__g.config.entity.wpn_ak_74_ng
    'static_data\23b85b2c_23b85b2c_cd459813', // static_data\hands_for_drezina__g.config.entity.hands_for_drezina
    'static_data\2fa04ac0_2fa04ac0_f5e357cf', // static_data\kulemet__g.config.entity.kulemet
    'static_data\2fa04ac0_2fa04ac0_a5b4bb3b', // static_data\kulemet__g.config.entity.kulemet_bashnya
    'static_data\0e154f91_0e154f91_a51e31d5', // static_data\watchman__g.config.entity.watchman
    'static_data\f1cacfad_f1cacfad_e00b321e', // static_data\woman_combat__g.config.entity.woman_combat
    'static_data\d91a939e_d91a939e_172a1e1e', // static_data\ammo__g.config.entity.ammo_12x70mm
    'static_data\1029375d_1029375d_cee38249', // static_data\weapon_uboynicheg__g.config.entity.wpn_uboynicheg_ng
    'static_data\9b9f0f1f_9b9f0f1f_8422e434', // static_data\drezina_hand__g.config.entity.drezina_hand
    'static_data\a0f671ac_a0f671ac_9631ae92', // static_data\staticprop_movable__g.config.entity.static_movable
    'static_data\c7457ba3_c7457ba3_e4e0a4e8', // static_data\ladder__g.config.entity.ladder
    'static_data\b713a3c2_b713a3c2_94b67c89', // static_data\medkit__g.config.entity.medkit
    'static_data\b574ea5d_b574ea5d_7f21d1cc', // static_data\o_explosion__g.config.entity.explosion
    'static_data\d91a939e_d91a939e_81767601', // static_data\ammo__g.config.entity.ammo_044_ap
    'static_data\d1554b72_d1554b72_ef79c3e9', // static_data\arm_devices__g.config.entity.arm_devices
    'static_data\1c36400d_1c36400d_2e1a7721', // static_data\weapon_macheta__g.config.entity.wpn_macheta
    'static_data\d91a939e_d91a939e_f2c05bb2', // static_data\ammo__g.config.entity.ammo_044_fmj
    'static_data\26ead86b_26ead86b_aba8a130', // static_data\binoculars__g.config.entity.binoculars
    'static_data\d91a939e_d91a939e_a366b3de', // static_data\ammo__g.config.entity.ammo_15mm_sabot
    'static_data\cfc909c3_cfc909c3_158a14cc', // static_data\gasmask__g.config.entity.gasmask
    'static_data\d91a939e_d91a939e_9ca45a39', // static_data\ammo__g.config.entity.ammo_762x39_fmj
    'static_data\d91a939e_d91a939e_c8aefcf3', // static_data\ammo__g.config.entity.ammo_15mm
    'static_data\d91a939e_d91a939e_5ab4373b', // static_data\ammo__g.config.entity.ammo_12x70mm_fire
    'static_data\45215999_45215999_68518ffc', // static_data\player_map__g.config.entity.player_map
    'static_data\9151aa6f_9151aa6f_fe5690a7', // static_data\npc_fx__g.config.entity.npc_enemy_fx
    'static_data\d384ff84_d384ff84_b9e02daf', // static_data\helsing_arrow_breakable__g.config.entity.wpn_arrow_breakable
    'static_data\aa177401_aa177401_53549294', // static_data\staticprop_breakable__g.config.entity.static_breakable
    'static_data\d91a939e_d91a939e_564ccb2b', // static_data\ammo__g.config.entity.ammo_12x70mm_flamethrower
    'static_data\d91a939e_d91a939e_4c6bd212', // static_data\ammo__g.config.entity.ammo_money
    'static_data\7740aebc_7740aebc_f83af38a', // static_data\weapon_flare__g.config.entity.wpn_flare
    'static_data\d91a939e_d91a939e_a117d05d', // static_data\ammo__g.config.entity.ammo_762x39_mg
    'static_data\15583d02_15583d02_e8690f4b', // static_data\helsing_arrow__g.config.entity.wpn_arrow
    'static_data\d384caf6_d384caf6_a34ce4cf', // static_data\kid__g.config.entity.kid
    'static_data\d91a939e_d91a939e_c5340d54', // static_data\ammo__g.config.entity.ammo_flamethrower_alt
    'static_data\b0584545_b0584545_d6b5ee2f', // static_data\weapon_flame_dynamite__g.config.entity.wpn_flame_grenage_npc
    'static_data\1f193ba1_1f193ba1_eb42f4ad', // static_data\weapon_dagger__g.config.entity.wpn_dagger
    'static_data\5aeb1d81_5aeb1d81_cd2679e2', // static_data\players_hands__g.config.entity.players_hands
    'static_data\2c0b059f_2c0b059f_0faedad4', // static_data\lurker__g.config.entity.lurker
    'static_data\a691b02a_a691b02a_7596bfd6', // static_data\weapon_ubludok__g.config.entity.wpn_ubludok_ng
    'static_data\3e209ab1_3e209ab1_4702ede0', // static_data\ai_vision_helper_entity__g.config.entity.ai_vision_helper_entity
    'static_data\41403263_41403263_d8f5d8f9', // static_data\weapon_vyhlop__g.config.entity.wpn_vyhlop_ng
    'static_data\9861566b_9861566b_171b0b5d', // static_data\weapon_decoy__g.config.entity.wpn_decoy
    'static_data\151dc865_151dc865_65d5e65c', // static_data\rat__g.config.entity.rat
    'static_data\b5b0650c_b5b0650c_f970a493', // static_data\weapon_gatling__g.config.entity.wpn_gatling_ng
    'static_data\b616569a_b616569a_bf923686', // static_data\weapon_dynamite__g.config.entity.wpn_dynamite
    'static_data\8f3c989b_8f3c989b_ac9947d0', // static_data\filter__g.config.entity.filter
    'static_data\b0584545_b0584545_a9e80d6b', // static_data\weapon_flame_dynamite__g.config.entity.wpn_flame_grenage
    'static_data\836e7adb_836e7adb_f3a654e2', // static_data\web__g.config.entity.web
    'static_data\57174204_57174204_69bf2010', // static_data\weapon_ashot__g.config.entity.wpn_ashot_ng
    'static_data\178b7bf7_178b7bf7_674355ce', // static_data\dog__g.config.entity.dog
    'static_data\83c583e4_83c583e4_7f56f186', // static_data\weapon_revolver__g.config.entity.wpn_revolver_ng
    'static_data\9feecd57_9feecd57_fde89fd0', // static_data\torchlight_upgradable__g.config.entity.torchlight_upgradable
    'static_data\e13d62a8_e13d62a8_3b7e7fa7', // static_data\lighter__g.config.entity.lighter
    'static_data\4015029a_4015029a_80e3ed61', // static_data\force_field__g.config.entity.force_field
    'static_data\72ad9e96_72ad9e96_a8ee8399', // static_data\charger__g.config.entity.charger
    'static_data\91551e3d_91551e3d_ea3b54d6', // static_data\weapon_ventil__g.config.entity.wpn_ventil_ng
    'static_data\512ec1ff_512ec1ff_9dfeff0a', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_ak_small_loot
    'static_data\e8ca57b3_e8ca57b3_229f6c22', // static_data\o_waterzone__g.config.entity.waterzone
    'static_data\b616569a_b616569a_7cbc1a60', // static_data\weapon_dynamite__g.config.entity.wpn_anomaly
    'static_data\4a3d9517_4a3d9517_368561c0', // static_data\harpy__g.config.entity.harpy
    'static_data\d91a939e_d91a939e_4888e3fc', // static_data\ammo__g.config.entity.craft_consumable
    'static_data\d91a939e_d91a939e_a4771d40', // static_data\ammo__g.config.entity.craft_metal
    'static_data\d91a939e_d91a939e_825c61e6', // static_data\ammo__g.config.entity.craft_chemical
    'static_data\512ec1ff_512ec1ff_673f3eef', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_ubo_std_loot
    'static_data\512ec1ff_512ec1ff_b2dd7f37', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_ubludok_small_loot
    'static_data\512ec1ff_512ec1ff_cdb521c3', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_revolver_small_loot
    'static_data\512ec1ff_512ec1ff_8f6d4d3b', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_helsing_ext_loot
    'static_data\512ec1ff_512ec1ff_04d8b73a', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_tihar_small_loot
    'static_data\512ec1ff_512ec1ff_9ebc5b7d', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_ubo_small_loot
    'static_data\512ec1ff_512ec1ff_44c9dec3', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_helsing_ext_altammo_loot
    'static_data\b616569a_b616569a_e281426a', // static_data\weapon_dynamite__g.config.entity.wpn_dynamite_paint
    'static_data\512ec1ff_512ec1ff_c45cb88f', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_tihar_small_altammo_loot
    'static_data\512ec1ff_512ec1ff_c0afb75f', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_helsing_small_altammo_loot
    'static_data\512ec1ff_512ec1ff_02dba022', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_helsing_small_loot
    'static_data\5060e530_5060e530_c2df9dd4', // static_data\weapon_flamethrower__g.config.entity.wpn_flamethrower_ng
    'static_data\512ec1ff_512ec1ff_b520aee9', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_ventil_loot
    'static_data\c721f4a2_c721f4a2_7f7b108f', // static_data\wb_weapon_holder__g.config.entity.wb_weapon_holder
    'static_data\a87e4855_a87e4855_cc6528bb', // static_data\tape__g.config.entity.tape
    'static_data\8083cc79_8083cc79_6b5f68ab', // static_data\weapon_helsing__g.config.entity.wpn_helsing_ng
    'static_data\d5452dd1_d5452dd1_121fc71c', // static_data\nightvision__g.config.entity.nightvision
    'static_data\03016a46_03016a46_0be6b917', // static_data\soft_entity__g.config.entity.soft_entity
    'static_data\b616569a_b616569a_fb9a5996', // static_data\weapon_dynamite__g.config.entity.wpn_dynamite_npc
    'static_data\ab85f30a_ab85f30a_7ae71d7d', // static_data\flying_creature__g.config.entity.flying_creature
    'static_data\b0584545_b0584545_b53a5bf3', // static_data\weapon_flame_dynamite__g.config.entity.wpn_cannibal_grenage
    'static_data\c7ffb39b_c7ffb39b_8500f35c', // static_data\hud_object__g.config.entity.hud_object
    'static_data\97be3554_97be3554_7680fe82', // static_data\treadmill_tile__g.config.entity.treadmill_tile
    'static_data\aca1607a_aca1607a_a446b32b', // static_data\rail_entity__g.config.entity.rail_entity
    'static_data\b616569a_b616569a_abf295fc', // static_data\weapon_dynamite__g.config.entity.wpn_humanimal_stuff
    'static_data\90c81990_90c81990_dabcb7af', // static_data\humanimal__g.config.entity.humanimal
    'static_data\9aa9d11f_9aa9d11f_b90c0e54', // static_data\rabbit__g.config.entity.rabbit
    'static_data\6d8c45b1_6d8c45b1_c6873bf5', // static_data\modifier__g.config.entity.modifier
    'static_data\28ad273b_28ad273b_41ba2438', // static_data\waves_emitter__g.config.entity.waves_emitter
    'static_data\8120f174_8120f174_9e9d1a5f', // static_data\drezina_moto__g.config.entity.drezina_moto
    'static_data\0f10b43b_0f10b43b_7a3f1ba4', // static_data\o_entity__g.config.entity.station
    'static_data\eefb6efe_eefb6efe_8ae00e10', // static_data\boat__g.config.entity.boat
    'static_data\b616569a_b616569a_c0be2c6e', // static_data\weapon_dynamite__g.config.entity.wpn_dynamite_small
    'static_data\c9bd76c3_c9bd76c3_13fe6bcc', // static_data\catfish__g.config.entity.catfish
    'static_data\39b5ac32_39b5ac32_eb164a07', // static_data\aqua_male_small__g.config.entity.aqua_male_small
    'static_data\29b629e8_29b629e8_2151fab9', // static_data\aqua_female__g.config.entity.aqua_female
    'static_data\c6bc8b79_c6bc8b79_7867f577', // static_data\anomaly__g.config.entity.simple_anomaly
    'static_data\b616569a_b616569a_cb6ad99b', // static_data\weapon_dynamite__g.config.entity.wpn_aqua_sputum
    'static_data\77af1ab6_77af1ab6_df684305', // static_data\o_scaling_entity__g.config.entity.scaling_entity
    'static_data\f1d7d478_f1d7d478_5adcaa3c', // static_data\cannibal__g.config.entity.cannibal
    'static_data\b0584545_b0584545_3a2bc827', // static_data\weapon_flame_dynamite__g.config.entity.wpn_flame_good
    'static_data\6881c5cb_6881c5cb_a8772a30', // static_data\woman_strip__g.config.entity.woman_strip
    'static_data\65655829_65655829_bf264526', // static_data\arahind__g.config.entity.arahind
    'static_data\b616569a_b616569a_3e7d6c51', // static_data\weapon_dynamite__g.config.entity.wpn_spider_sputum
    'static_data\4c81e4fd_4c81e4fd_e78a9ab9', // static_data\wallmark__g.config.entity.wallmark
    'static_data\16f5ef2e_16f5ef2e_aef34af2', // static_data\o_anim_entity__g.config.entity.anim_entity
    'static_data\a639a92e_a639a92e_db4e575c', // static_data\snake__g.config.entity.small_worm
    'static_data\24c51a9c_24c51a9c_fe860793', // static_data\vehicle__g.config.entity.vehicle
    'static_data\0cbbe8a6_0cbbe8a6_c34945d2', // static_data\o_hlamp__g.config.entity.reflected_light
    'static_data\492f7c22_492f7c22_359788f5', // static_data\clock__g.config.entity.clock
    'static_data\28005254_28005254_0ba58d1f', // static_data\grizly__g.config.entity.grizly
    'static_data\4b08d4c5_4b08d4c5_2f13b42b', // static_data\deer__g.config.entity.deer
    'static_data\3a55b380_3a55b380_a67be6c8', // static_data\flexible_entity__g.config.entity.flexible_entity
    'static_data\a639a92e_a639a92e_e8a340ef', // static_data\snake__g.config.entity.worm
    'static_data\55d34afc_55d34afc_296bbe2b', // static_data\blind__g.config.entity.blind
    'static_data\2fa04ac0_2fa04ac0_feeb65dd', // static_data\kulemet__g.config.entity.kulemet_bashnya_nopierce
    'static_data\a639a92e_a639a92e_f747b16e', // static_data\snake__g.config.entity.middle_worm
    'static_data\b616569a_b616569a_74a2b13e', // static_data\weapon_dynamite__g.config.entity.wpn_water_worm_sputum
    'static_data\81146306_81146306_5b577e09', // static_data\nosalis__g.config.entity.nosalis
    'static_data\4250e763_4250e763_0824495c', // static_data\darkchild__g.config.entity.darkchild
    'static_data\8cb82001_8cb82001_ac8e1d45', // static_data\worm__g.config.entity.water_worm
    'static_data\c863fe1f_c863fe1f_bc2cc366', // static_data\staticprop_aux_effect__g.config.entity.static_aux_effect
    'static_data\cfc909c3_cfc909c3_276629a3', // static_data\gasmask__g.config.entity.gasmask_dlc2
    'static_data\ebef0bb1_ebef0bb1_ad74c76b', // static_data\entity_aux_effect__g.config.entity.entity_aux_effect
    'static_data\a639a92e_a639a92e_ac8e1d45', // static_data\snake__g.config.entity.water_worm
    'static_data\f67770b6_f67770b6_aafa363a', // static_data\sea_cucumber__g.config.entity.sea_cucumber
    'static_data\512ec1ff_512ec1ff_008f3c23', // static_data\weapon_item_magazine__g.config.entity.weapon_magazine_flamethrower_small_loot
    'static_data\d1554b72_d1554b72_39246caa', // static_data\arm_devices__g.config.entity.arm_device_dlc1
    'static_data\b616569a_b616569a_d079e014', // static_data\weapon_dynamite__g.config.entity.wpn_water_worm_sputum_dlc_1
    'static_data\5060e530_5060e530_24d010f7', // static_data\weapon_flamethrower__g.config.entity.wpn_flamethrower_ng_npc
    'static_data\8975ca01_8975ca01_c4ad6108', // static_data\hud_light__g.config.entity.hud_light
    'static_data\b616569a_b616569a_a429ee92', // static_data\weapon_dynamite__g.config.entity.wpn_dynamite_yz
    'static_data\71244b62_71244b62_2f185157', // static_data\weapon_kolya__g.config.entity.wpn_colt_2033
    'static_data\81cff352_81cff352_fd770785', // static_data\flock__g.config.entity.flock
    'static_data\1ec322f6_1ec322f6_10eb47dc', // static_data\weapon_ak_sammy__g.config.entity.wpn_ak_sammy
    'static_data\b616569a_b616569a_3d191509', // static_data\weapon_dynamite__g.config.entity.wpn_emp_spider_sputum
    'static_data\2988e6ed_2988e6ed_36350dc6', // static_data\bending_tree__g.config.entity.bending_tree
    'static_data\e13d62a8_e13d62a8_83f0363b', // static_data\lighter__g.config.entity.lightter_sam
    'static_data\f3bc855f_f3bc855f_a21ccb0e', // static_data\scripted_lian__g.config.entity.scripted_lian
    'static_data\eefb6efe_eefb6efe_92f3c647', // static_data\boat__g.config.entity.motor_boat
    'static_data\4a3d9517_4a3d9517_7c923983', // static_data\harpy__g.config.entity.mantademon
    'static_data\b616569a_b616569a_7c2c8a6d', // static_data\weapon_dynamite__g.config.entity.wpn_dynamite_newoin_no_ai_sound
    'static_data\6f5aa5da_6f5aa5da_0b41c534', // static_data\frog__g.config.entity.frog
    'static_data\b616569a_b616569a_57b9497a', // static_data\weapon_dynamite__g.config.entity.wpn_dynamite_newoin
    'static_data\b0584545_b0584545_19aff9d6', // static_data\weapon_flame_dynamite__g.config.entity.wpn_flame_low_time
    'static_data\a691b02a_a691b02a_2f185157'  // static_data\weapon_ubludok__g.config.entity.wpn_colt_2033
  ];


function GuessName(hash : Longint) : String;
var
  I : Integer;
begin
  Result := '';
  for I := Low(knownscripts) to High(knownscripts) do
    if GetStringCrc('content\scripts\' + knownscripts[I] + '.bin') = hash then
    begin
      Result := knownscripts[I];
      Break;
    end;
end;

procedure SplitScripts(const filename : String);
var
  hash, len : Longint;
  scripts, one : TFileStream;
  name, path : String;
  buffer : array of byte;
begin
  scripts := TFileStream.Create(filename, fmOpenRead);
  while scripts.Position < scripts.Size do
  begin
    scripts.ReadBuffer(hash, sizeof(hash));
    scripts.ReadBuffer(len, sizeof(len));

    SetLength(buffer, len);
    scripts.Read(buffer[0], len);

    name := GuessName(hash);
    if name = '' then
      name := '#' + IntToHex(hash, 8);
    Write(name, ' length ', len); Writeln;
    
    path := 'scripts\' + name + '.bin';
    ForceDirectories(ExtractFilePath(path));

    one := TFileStream.Create(path, fmCreate);
    one.WriteBuffer(buffer[0], len);
    one.Free;
  end;
  scripts.Free;
end;

procedure MergeDir(f : TFileStream; const dir : String);
var
  script : TFileStream;
  buffer : array of Byte;
  crc, size : Longint;
  sr : TSearchRec;
begin
  if FindFirst(dir+'\*', faDirectory, sr) = 0 then
  begin
    repeat
      if not ((sr.Name = '.') or (sr.Name = '..')) then
        MergeDir(f, dir + '\' + sr.Name)
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  if FindFirst(dir+'\*.bin', faAnyFile xor faDirectory, sr) = 0 then
  begin
    repeat                                
      script := TFileStream.Create(dir+'\'+sr.Name, fmOpenRead);
      SetLength(buffer, script.Size);
      script.ReadBuffer(buffer[0], script.Size);
      script.Free;

      if sr.Name[1] = '#' then
        crc := StrToInt('$' + Copy(sr.Name, 2, 8))
      else
        crc := GetStringCrc('content\'+dir+'\'+sr.Name);
      size := Length(buffer);

      f.WriteBuffer(crc, Sizeof(crc));
      f.WriteBuffer(size, Sizeof(size));
      f.WriteBuffer(buffer[0], size);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure MergeScripts(const filename : String);
var
  scripts : TFileStream;
begin
  scripts := TFileStream.Create(filename, fmCreate);
  MergeDir(scripts, 'scripts');
  scripts.Free;
end;

var
  filename : String;
begin
  filename := 'scripts.bin';
  
  if ParamCount >= 2 then
    filename := ParamStr(2);

  if (ParamCount < 1) or (ParamStr(1) = '-d') then
    SplitScripts(filename)
  else if ParamStr(1) = '-c' then
    MergeScripts(filename);
end.