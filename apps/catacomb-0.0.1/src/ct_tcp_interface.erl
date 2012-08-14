-module(ct_tcp_interface).

-export([start/0, loop/1]).

% echo_server specific code
start() ->
	socket_server:start(?MODULE, 7000, {?MODULE, loop}).
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            loop(Socket);
        {error, closed} ->
            ok
    end.