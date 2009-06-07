-module(ubt_calc).
-export([timeout/2]).
-compile(export_all).

timeout({OldEstimatedRTT, OldDevRTT, OldTimeOut}, SampleRTT) ->
    EstimatedRTT = estmated_rtt(OldEstimatedRTT, SampleRTT),
    DevRTT = dev_rtt(OldDevRTT, OldEstimatedRTT, SampleRTT),
    TimeOut = timeout_interval(EstimatedRTT, DevRTT),
    {EstimatedRTT, DevRTT, TimeOut}.

estmated_rtt(EstimatedRTT, SampleRTT) ->
    % a = 0.125
    (1 - 0.125) * EstimatedRTT + 0.125 * SampleRTT.

dev_rtt(DevRTT, EstimatedRTT, SampleRTT) ->
    % b = 0.25
    (1 - 0.25) * DevRTT + 0.25 * abs(SampleRTT - EstimatedRTT).

timeout_interval(EstimatedRTT, DevRTT) ->
    EstimatedRTT + 4 * DevRTT.
    



