:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, eventType) :- deep_subdict(_{'name':"pop",'event':"func_pre"}, Event).
match(_, any).
trace_expression('Main', Main) :- Main=app(Repeat, [3]), Repeat=gen(['n'], guarded((var('n')>0), (eventType:app(Repeat, [(var('n')-1)])), eps)).
