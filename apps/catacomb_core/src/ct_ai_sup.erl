-module(ct_ai_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1,start_ai/1]).

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
	io:format("~s has started (~w)~n", [?MODULE,self()]),
	%% Declare a simple_one_for_one supervisor as this king of supervisor is ideal for workers.
	%% All childrem must be started dynamically and are copies of the same module.
	WorkerSpecs = {ct_ia, {ct_ai, start_link, []}, temporary, 2000, worker,[ct_ai]},
	StartSpecs = {{simple_one_for_one, 0, 1},[WorkerSpecs]},
    {ok, StartSpecs}.

%% Starts an individual player
start_ai(AiSpecs) ->
	{ok,AiPlayer}=supervisor:start_child({global,?MODULE}, [AiSpecs]),
	{ok,AiPlayer}.