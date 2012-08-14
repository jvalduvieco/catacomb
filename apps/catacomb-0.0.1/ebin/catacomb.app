{application, catacomb,
 [{description, "Game full of rooms"},
  {vsn, "0.0.1"},
  {mod, {catacomb, []}},
  {registered, [ct_soot_sup]},
  {modules, [catacomb, ct_player]},
  {applications, [stdlib, kernel, crypto]}]}.