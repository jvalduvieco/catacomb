-module(ct_ai_behaviours).
-export([random_movements/1]).

-include ("ct_ai.hrl").

random_movements(State) ->
  apply_after(Time, Module, Function, Arguments).

random_movements(State) ->
  timer:sleep(1000+random:uniform(3000)),
  %io:format("moving ai...~n"),
	%% select rand exit
	Exits = State#ai_state.room_exits,
	RandomExit = lists:nth(random:uniform(length(Exits)), Exits),
	%io:format("RandomExit: ~p~n", [RandomExit]),
	%% move player
	ct_player:go(State#ai_state.player, RandomExit),
	%% wait
	%io:format("waiting...~n"),

	State.
