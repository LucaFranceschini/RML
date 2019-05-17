:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, clock_tick(Time)) :- deep_subdict(_{'clock':Time}, _event).
match(_event, clock_tick_within(Max)) :- match(_event, clock_tick(Time)), =<(Time, Max).
match(_event, temp) :- deep_subdict(_{'temperature':_}, _event).
match(_, any).
trace_expression('Main', Main) :- CheckTime=gen(['max'], star((clock_tick_within(var(max)):eps))), Delay=gen(['delay'], var(time, ((clock_tick(var(time)):eps)*app(CheckTime, [(var('time')+var('delay'))])))), Main=star((app(Delay, [0.9])*(temp:eps))).
