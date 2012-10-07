-module(ct_player_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1,start_player/3]).


start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
	%% Declare a simple_one_for_one supervisor as this king of supervisor is ideal for workers.
	%% All childrem must be started dynamically and are copies of the same module.
	WorkerSpecs = {ct_player, {ct_player, start_link, []}, temporary, 2000, worker,[ct_player]},
	StartSpecs = {{simple_one_for_one, 0, 1},[WorkerSpecs]},
    {ok, StartSpecs}.

%% Starts an individual player
start_player(Name, ClientPid,Params) ->
    {ok,Pid}=supervisor:start_child({global,?MODULE}, [Name, [ClientPid|Params]]),
    Player=ct_player:get_handler(Pid),
    {ok,Player}.