-module(ct_ai_behaviours).
-export([random_movements/2]).

-include ("ct_ai.hrl").

random_movements(State, AiPid) ->
	%io:format("moving ai...~n"),
	%% select rand exit
	Exits = State#ai_state.room_exits,
	RandomExit = lists:nth(random:uniform(length(Exits)), Exits),
	%io:format("RandomExit: ~p~n", [RandomExit]),
	%% move player
	ct_player:go(State#ai_state.player, RandomExit),
	%% wait
	%io:format("waiting...~n"),
	timer:sleep(2000),
	ct_ai:start_moving(AiPid),

	State.
