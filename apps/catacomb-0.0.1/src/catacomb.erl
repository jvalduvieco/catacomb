-module(catacomb).
-behaviour(application).
-export([start/2, stop/1]).

start(normal,[]) ->
    case ct_root_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid, []};
        Error -> Error
    end.

stop(_) -> ok.