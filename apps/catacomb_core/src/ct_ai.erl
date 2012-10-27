-module(ct_ai).
-behaviour(gen_server).

-export([start_link/1,stop/0]).
-export([]).
-export([init/1,handle_cast/2,handle_call/3,terminate/2,code_change/3,handle_info/2]).

start_link(AiSpecs) ->
    gen_server:start_link(?MODULE, AiSpecs, []).

%% Internal functions
init(_AiSpecs) ->
	AiCharacter=[
		{<<"id">>, 32908230982},
		{<<"name">>, <<"HAL">>},
		{<<"max_life_points">>, 1000000},
		{<<"life_points">>, 150000},
		{<<"level">>, 60},
		{<<"experience_points">>, 200000},
		{<<"coord_x">>, 1},
		{<<"coord_y">>, 1}
	],
	{ok, AiPlayer} = ct_player_sup:start_player([{obj,AiCharacter}]),
	ct_player:set_client(AiPlayer,self()),

	State={AiPlayer},
	io:format("ct_ia has started (~w)~n", [self()]),
    {ok, State}.
stop() -> gen_server:cast(?MODULE, stop).
%% User Callbacks
handle_cast({some_atom, _Var}, State) ->
    {noreply, State};
handle_cast(Something, State) ->
	io:format("AI receives cast: ~p~n", [Something]),
    {noreply, State}.
handle_call(Something,_From,State) -> 
	io:format("AI receives call: ~p~n", [Something]),
	{reply,State,State}.

%% System callbacks
terminate(_Reason, State) -> {ok,State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.