-module(ct_ai_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1,start_ai/1,start_ai_random_movements/1]).

-include ("ct_ai.hrl").

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
	{ok,Pid}=supervisor:start_child({global,?MODULE}, [AiSpecs]),
	AiPlayer=ct_player:get_handler(Pid),
	timer:sleep(1000), %% wait initialization
	ct_ai:start_moving(Pid),
	{ok,Pid,AiPlayer}.

start_ai_random_movements(Name) ->
	Fun = fun(State, AiPid) -> ct_ai_behaviours:random_movements(State, AiPid) end,
	AiSpecs=#ai_specs{name=Name, behaviour=Fun},
	start_ai(AiSpecs).
