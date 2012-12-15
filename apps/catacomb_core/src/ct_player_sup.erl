-module(ct_player_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1,start_player/2,get_handler/1]).

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
	lager:info("~s has started (~w)~n", [?MODULE,self()]),
    ets:new(player_id_to_pid, [set, named_table,public]),
	%% Declare a simple_one_for_one supervisor as this king of supervisor is ideal for workers.
	%% All childrem must be started dynamically and are copies of the same module.
	WorkerSpecs = {ct_player, {ct_player, start_link, []}, temporary, 2000, worker,[ct_player]},
	StartSpecs = {{simple_one_for_one, 0, 1},[WorkerSpecs]},
    {ok, StartSpecs}.

%% Starts an individual player
start_player(CharacterSpecs,FeedbackData) ->
	{ok,Pid}=supervisor:start_child({global,?MODULE}, [CharacterSpecs]),
  Player=ct_player:get_handler(Pid),
  PlayerId=ct_player:get_public_id(Player),
  ct_player:set_feedback_data(Player,FeedbackData),
  [{obj,CharacterData}]=CharacterSpecs,
  X=proplists:get_value(<<"coord_x">>, CharacterData, none),
  Y=proplists:get_value(<<"coord_y">>, CharacterData, none),
  ct_player:set_room(Player,[X,Y]),
  ets:insert(player_id_to_pid,{PlayerId,Player}),

  {ok,Player}.
get_handler(PlayerId) ->
    case ets:lookup(player_id_to_pid,PlayerId) of
        [] ->
            {error,undefined};
        [{_,PlayerHandle}] ->
            {ok,PlayerHandle}
    end.
