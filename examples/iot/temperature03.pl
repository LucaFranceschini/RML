:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, temp_sample(Time)) :- deep_subdict(_{'timestamp':Time,'temperature':_}, _event).
match(_event, check_sample(Time1, Time2)) :- match(_event, temp_sample(Time2)), =<((Time2-Time1), 1.005).
match(_, any).
trace_expression('Main', Main) :- CheckSamples=gen(['time1'], var(time2, ((check_sample(var(time1), var(time2)):eps)*app(CheckSamples, [var('time2')])))), Main=var(time1, ((temp_sample(var(time1)):eps)*app(CheckSamples, [var('time1')]))).
