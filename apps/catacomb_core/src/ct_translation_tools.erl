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
from_client(Data) ->
	{JSONObj,_,_} = ktj_parse:parse(Data),
	Result = case JSONObj of
		{obj,_} ->
			{ok,JSONObj};
		_ ->
			{error,bad_args}
	end,
	Result.

%% Encodes data from Erlang proplists to JSON text
to_client(Data)->
	JSONText = ktj_encode:encode(Data),
	Result = case JSONText of
		{error,_} ->
			JSONText; % {error,{"Expected object seperator",58,0,8}}
		_ ->
			{ok,lists:flatten(JSONText)}
	end,
	Result.

get_type(Data) ->
	{obj,Header}=Data,
	proplists:get_value(<<"type">>,Header,none).
get_value(Key,Data) ->
	{obj,Header}=Data,
	{obj,Body}=proplists:get_value(<<"body">>,Header,none),
	proplists:get_value(Key,Body,none).