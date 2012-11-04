-module(ct_session_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-export([get_new_session_pid/0]).

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
	lager:info("~s has started (~w)~n", [?MODULE,self()]),
	WorkerSpecs = {ct_session, {ct_session, start_link, []}, temporary, 2000, worker,[ct_session]},
	StartSpecs = {{simple_one_for_one, 0, 1},[WorkerSpecs]},
    {ok, StartSpecs}.

%% Starts a new session
get_new_session_pid() ->
	{ok,Pid}=supervisor:start_child({global,?MODULE}, []),
	{ok,Pid}.