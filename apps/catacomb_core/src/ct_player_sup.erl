-module(ct_player_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1,start_player/1]).


start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
	lager:info("~s has started (~w)~n", [?MODULE,self()]),
	%% Declare a simple_one_for_one supervisor as this king of supervisor is ideal for workers.
	%% All childrem must be started dynamically and are copies of the same module.
	WorkerSpecs = {ct_player, {ct_player, start_link, []}, temporary, 2000, worker,[ct_player]},
	StartSpecs = {{simple_one_for_one, 0, 1},[WorkerSpecs]},
    {ok, StartSpecs}.

%% Starts an individual player
start_player(CharacterSpecs) ->
	{ok,Pid}=supervisor:start_child({global,?MODULE}, [CharacterSpecs]),
    Player=ct_player:get_handler(Pid),
    [{obj,CharacterData}]=CharacterSpecs,
    X=proplists:get_value(<<"coord_x">>, CharacterData, none),
    Y=proplists:get_value(<<"coord_y">>, CharacterData, none),
    ct_player:set_room(Player,[X,Y]),
    {ok,Player}.