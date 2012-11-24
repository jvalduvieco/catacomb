-module(catacomb_core).
-behaviour(application).
-export([start/2, stop/1]).

start(normal,[]) ->
	emysql:add_pool(ct_auth_pool, 1,
		"catacomb", "pass", "localhost", 3306,
		"catacomb", utf8),
  lager:set_loglevel(lager_console_backend, debug),
  case ct_root_sup:start_link() of
      {ok, Pid} ->
        ct_god:init_map(),
          {ok, Pid, []};
      Error -> Error
  end.

stop(_) -> ok.
