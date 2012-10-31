-module(ct_translation_tools).
-export([from_client/1,to_client/1,get_type/1,get_value/2]).

%%% JSON translation tools. 

%% Decode data from JSON text to erlang proplists.
%% ktj_parse:parse(JSON).
%% {{obj,[{<<"body">>,
%%         [{obj,[{<<"speed">>,2.3},
%%                {<<"life_points">>,2222},
%%                {<<"max_life_points">>,32323},
%%                {<<"level">>,33},
%%                {<<"name">>,<<"char1">>}]},
%%          {obj,[{<<"speed">>,2.3},
%%                {<<"life_points">>,2222},
%%                {<<"max_life_points">>,32323},
%%                {<<"level">>,23},
%%                {<<"name">>,<<"char2">>}]}]},
%%        {<<"type">>,<<"CharListResponse">>}]},
%% 
%% Returns 	{ok, JSON_data_propslist}
%%			{error, Error_info}
-spec from_client(_) -> {'ok',any()} | {'error','bad_args'}.
from_client(Data) ->
  {JSONObj,_,_} = ktj_parse:parse(Data),
  case JSONObj of
    {obj,_} ->
      {ok,JSONObj};
    _ ->
      {error,bad_args}
  end.

%% Encodes data from Erlang proplists to JSON text
-spec to_client('false' | 'null' | 'true' | binary() | ['false' | 'null' | 'true' | binary() | ['false' | 'null' | 'true' | binary() | [any()] | number() | {_,_}] | number() | {'obj',[any()]}] | number() | {'obj',[{_,_}]}) -> {'ok',string()}.
to_client(Data)->
	JSONText = ktj_encode:encode(Data),
  {ok,lists:flatten(JSONText)}.

-spec get_type({'obj',[any()]}) -> any().
get_type(Data) ->
	{obj,Header}=Data,
	proplists:get_value(<<"type">>,Header,none).
-spec get_value(_,{'obj',[any()]}) -> any().
get_value(Key,Data) ->
	{obj,Header}=Data,
	{obj,Body}=proplists:get_value(<<"body">>,Header,none),
	proplists:get_value(Key,Body,none).