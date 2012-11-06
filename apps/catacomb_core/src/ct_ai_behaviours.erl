-module(ct_ai_behaviours).
-export([random_movements/1,random_movements_fun/1]).

-include ("ct_ai.hrl").

random_movements(State) ->
  %% random regards
  L = random:uniform(10), % 10% change of regards
  if
    L =:= 1 -> ct_player:talk(State#ai_state.player, <<"Hi buddy!">>);
    true -> false
  end,

  %% random movement
  Pid = State#ai_state.pid,
  Fun = fun(StateV) -> ct_ai_behaviours:random_movements_fun(StateV) end,
  timer:apply_after(1000+random:uniform(3000), ct_ai, do_fun, [Pid, Fun]),
  State.

random_movements_fun(State) ->
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
