-module(ct_yaws_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
	lager:info("~s has started (~w)~n", [?MODULE,self()]),
	WorkerSpecs = {ct_yaws_start, {ct_yaws_start, start, []}, permanent, 2000, worker,[ct_yaws_start]},
	StartSpecs = {{one_for_all, 0, 1},[WorkerSpecs]},
    {ok, StartSpecs}.