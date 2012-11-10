-module(ct_player_heartbeat).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([heartbeat_start/1, heartbeat/2]).
-export([init/1, stopped/2, live/2]).
-export([code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,terminate/3]).

start_link(Player) ->
  gen_fsm:start_link(ct_player_heartbeat, Player, []).

heartbeat_start(Pid) ->
  gen_fsm:send_event(Pid, {start}).

heartbeat(Pid, LastTimeDiff) ->
  gen_fsm:send_event(Pid, {heartbeat, LastTimeDiff}).

init(Player) ->
  {ok, stopped, {Player, none, none}}.

stopped({start}, {Player, none, none}) ->
  lager:debug("starting heartbeat player..."),
  {next_state, live, {Player, none, none}}.

live({heartbeat, NewLastTimeDiff}, {Player, none, none}) ->
  NewLastTimestamp = now(),
  lager:debug("first heartbeat"),
  {next_state, live, {Player, NewLastTimestamp, NewLastTimeDiff}, 12000};
live({heartbeat, NewLastTimeDiff}, {Player, LastTimestamp, LastTimeDiff}) ->
  NewLastTimestamp = now(),
  Dif = timer:now_diff(NewLastTimestamp, LastTimestamp),
  lager:debug("heartbeat lts: ~p, ltdif: ~p, nts: ~p, ntdif: ~p, dif: ~p", [LastTimestamp, LastTimeDiff, NewLastTimestamp, NewLastTimeDiff, Dif]),
  {next_state, live, {Player, NewLastTimestamp, NewLastTimeDiff}, 12000};
live(timeout, {Player, LastTimestamp, _LastTimeDiff}) ->
  lager:debug("Heartbeat timeout. Stopping player."),
  ct_player:stop(Player),
  %% debug info
  NewLastTimestamp = now(),
  Dif = timer:now_diff(NewLastTimestamp, LastTimestamp),
  lager:debug("heartbeat dif: ~p", [Dif]),
  {stop, heartbeat_timeout, none}.

handle_event(stop, _StateName, StateData) ->
  {stop, normal, StateData}.

handle_info({'EXIT', _Pid, _Reason}, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
  {next_state,StateName,StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
  ok.

