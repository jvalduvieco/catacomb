-module(catacomb_core).
-behaviour(application).
-export([start/2, stop/1]).

-spec start('normal',[]) -> {'error',_} | {'ok',pid(),[]}.
start(normal,[]) ->
	emysql:add_pool(ct_auth_pool, 1,
		"catacomb", "pass", "localhost", 3306,
		"catacomb", utf8),

    case ct_root_sup:start_link() of
        {ok, Pid} ->
        	ct_god:init_map(),
            {ok, Pid, []};
        Error -> Error
    end.

-spec stop(_) -> 'ok'.
stop(_) -> ok.
