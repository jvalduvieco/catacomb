%% AI state
-record(ai_state,{
    pid,
		player,
		room_name,
		room_exits,
		players_seen = [],
		last_player_unseen,
		behaviour_on_room_enter = fun(State) -> State end, %% behaviour on event
		behaviour_on_player_seen = fun(State) -> State end, %% behaviour on event
		behaviour_on_player_unseen = fun(State) -> State end, %% behaviour on event
		behaviour_data %% storage where behaviours can store data for their own use
	}).

%% AI behaviour definition
-record(ai_specs,{
		name,
		behaviour_on_room_enter = fun(State) -> State end,
		behaviour_on_player_seen = fun(State) -> State end,
		behaviour_on_player_unseen = fun(State) -> State end
	}).
