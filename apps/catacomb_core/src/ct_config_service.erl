-module(ct_config_service).
-behaviour(gen_server).
%% This code is adapted from:
%% http://pdincau.wordpress.com/2011/06/28/an-easy-way-to-handle-configuration-parameters-in-erlang/
-export([start_link/0,stop/0]).
-export([init/1, handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).

-export([lookup/1, update/2]).

-record(state, {conf}).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%% Client API
lookup(Tag) ->
  gen_server:call({global,?SERVER}, {lookup, Tag}).
update(Tag, Value) ->
  gen_server:call({global,?SERVER}, {update, {Tag, Value}}).

%% Internal functions
init([]) ->
	lager:info("ct_config_service has started (~w)~n", [self()]),
  {ok, Conf} = file:consult("configuration.cfg"),
  {ok, #state{conf=Conf}}.

%% Callbacks
handle_call({lookup, Tag}, _From, State) ->
  Reply = case lists:keyfind(Tag, 1, State#state.conf) of
    {Tag, Value} ->
      Value;
    false ->
      {error, noinstance}
  end,
  lager:debug("ct_config_service: lookup ~p -> ~p~n", [Tag,Reply]),
  {reply, Reply, State};

handle_call({update, {Tag, Value}}, _From, State) ->
  lager:debug("ct_config_service: update ~p -> ~p~n", [Tag,Value]),
  NewConf = lists:keyreplace(Tag, 1, State#state.conf, {Tag, Value}),
  Reply = ok,
  {reply, Reply, State#state{conf=NewConf}}.

stop() -> gen_server:cast({global,?MODULE}, stop).

%% System callbacks
terminate(_Reason, State) -> {ok,State}.
handle_cast(stop, State) -> {stop, normal, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.

