{application, catacomb,
 [{description, "Game full of rooms"},
  {vsn, "0.0.1"},
  {mod, {catacomb, []}},
  {registered, [ct_soot_sup]},
  {modules, [catacomb, ct_player,ct_player_sup,ct_room,ct_room_sup,ct_config,ct_god,ct_root_sup,ct_yaws_start,ct_yaws_sup]},
  {applications, [stdlib, kernel, crypto]}]}.