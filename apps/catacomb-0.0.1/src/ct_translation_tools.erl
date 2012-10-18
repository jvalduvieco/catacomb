-module(ct_translation_tools).
-export([from_client/1,to_client/1]).

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
	{JSONObj,_,_} = ktj_parse:parse(JSONext),
	Result = case JSONObj of
		{obj,_} ->
			{ok,JSONObj};
		_ ->
			{error,bad_args}
	end,
	Result.

%% Encodes data from Erlang proplists to JSON text
to_client(Data)->
	JSONText = JSONText:parse(JSONErr),
	Result = case JSONObj of
		{error,_} ->
			Result; % {error,{"Expected object seperator",58,0,8}}
		_ ->
			{ok,Result}
	end,
	Result.