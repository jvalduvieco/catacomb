-module(ct_translation_tools).
-export([from_client/1,to_client/1]).

-include().
from_client(Data) ->
	{ok,Data}.
to_client(Data)->
	{ok,Data}