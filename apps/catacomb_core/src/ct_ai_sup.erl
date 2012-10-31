-module(ct_ai_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1,start_ai/1,start_ai_random_movements/1, start_N_ai/2]).

-include ("include/ct_ai.hrl").
-include ("include/ct_character_info.hrl").

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.

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
-spec start_ai(_) -> {'ok', pid(), #player_state{}}.

start_ai(AiSpecs) ->
	{ok,Pid}=supervisor:start_child({global,?MODULE}, [AiSpecs]),
	AiPlayer=ct_player:get_handler(Pid),
	%timer:sleep(1000), %% wait initialization
	{ok,Pid,AiPlayer}.

-spec start_ai_random_movements(_) -> {'ok', pid(), #player_state{}}.

start_ai_random_movements(Name) ->
	Fun = fun(State) -> ct_ai_behaviours:random_movements(State) end,
	AiSpecs=#ai_specs{name=Name, behaviour_on_room_enter=Fun},
	start_ai(AiSpecs).

-spec start_N_ai(_,non_neg_integer()) -> 'ok'.

start_N_ai(_, 0) -> ok;
start_N_ai(Name, N) ->
	FinalName = Name ++ integer_to_list(N),
	start_ai_random_movements(FinalName),
	start_N_ai(Name, N-1).