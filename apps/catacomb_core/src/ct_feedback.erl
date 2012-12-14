-module(ct_feedback).

%%% records
-record(feedback_data,{module=undefined,
  gateway_pid=undefined
}).

%% API
-export([set_feedback_module/2,set_gateway_pid/2,send/2]).


set_feedback_module(Module,undefined) ->
  set_feedback_module(Module,#feedback_data{});
set_feedback_module(Module,FeedbackData) ->
  NewFeedbackData=FeedbackData#feedback_data{module=Module},
  NewFeedbackData.

set_gateway_pid(Gateway,undefined) ->
  set_gateway_pid(Gateway,#feedback_data{});
set_gateway_pid(Gateway,FeedbackData) ->
  NewFeedbackData=FeedbackData#feedback_data{gateway_pid=Gateway},
  NewFeedbackData.

send(Feedback, #feedback_data{} = FeedbackData) ->
  Pid=FeedbackData#feedback_data.gateway_pid,
  Module=FeedbackData#feedback_data.module,
  Result=Module:send(Pid,Feedback),
  Result.