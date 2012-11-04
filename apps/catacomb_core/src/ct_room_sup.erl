-module(ct_room_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-export([create_room/1,get_pid/1]).

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
	ets:new(coordToPid, [set, named_table,public]),
	%% Declare a simple_one_for_one supervisor as this king of supervisor is ideal for workers.
	%% All childrem must be started dynamically and are copies of the same module.
	lager:info("~s has started (~w)~n", [?MODULE,self()]),
	WorkerSpecs = {{global,ct_room}, {ct_room, start_link, []}, temporary, 2000, worker,[ct_room]},
	StartSpecs = {{simple_one_for_one, 0, 1},[WorkerSpecs]},
    {ok, StartSpecs}.

%% Starts an individual player
create_room([X,Y]) ->
	case ets:lookup(coordToPid,list_to_atom([X,Y])) of
		[] ->
			{ok,Pid}=supervisor:start_child({global,?MODULE}, [X,Y]),
			{ok,{room,Pid}};
		[{_,Pid}] ->
			{ok,Pid}
	end.
	
get_pid([X,Y]) ->
	case ets:lookup(coordToPid,list_to_atom([X,Y])) of
		[] ->
			{error,undefined};
		[{_,Pid}] ->
			{ok,Pid}
	end.
