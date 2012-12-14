-module(ct_yaws_gateway_feedback).

%% API
-export([send/2]).

send(Pid,Feedback)->
  {ok,Response}=ct_translation_tools:to_client(Feedback),
  yaws_api:websocket_send(Pid,{text, list_to_binary(Response)}).