-module(catacomb).
-behaviour(application).
-export([start/3, stop/1]).

start(normal, X,Y) ->
    case ct_root_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid, []};
        Error -> Error
    end.

stop(_) -> ok.