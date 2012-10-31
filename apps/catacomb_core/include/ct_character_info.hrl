%%% Character info

-record(ct_character_info,{
	id,
	name,
	max_life_points,
	life_points,
	level,
	experience_points,
	location,
	inventory
	}).


-record(player_state,{id,
	my_pid,
	client,
	name,
	max_life_points,
	life_points,
	level,
	experience_points,
	room,
	room_exits,
	params=[],
	feedback_fun}).
