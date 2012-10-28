%% AI state
-record(ai_state,{
		player,
		room_name,
		room_exits,
		players_seen = [],
		last_player_unseen,
		behaviour = fun(State, _AiPid) -> State end, %% behaviour executed once when AI is spawned (if you want to execute every N seconds, add a timer and recursive call to ct_ai:start_moving)
		behaviour_on_room_enter = fun(State) -> State end, %% behaviour on event
		behaviour_on_player_seen = fun(State) -> State end, %% behaviour on event
		behaviour_on_player_unseen = fun(State) -> State end, %% behaviour on event
		behaviour_data %% storage where behaviours can store data for their own use
	}).

%% AI behaviour definition
-record(ai_specs,{
		name,
		behaviour = fun(State, _AiPid) -> State end,
		behaviour_on_room_enter = fun(State) -> State end,
		behaviour_on_player_seen = fun(State) -> State end,
		behaviour_on_player_unseen = fun(State) -> State end
	}).
