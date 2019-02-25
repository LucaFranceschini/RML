:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, eventType) :- deep_subdict(_{'name':"pop",'event':"func_pre"}, Event).
match(Event, size(Res)) :- deep_subdict(_{'result':Res,'name':"size",'event':"func_post"}, Event).
match(_, any).
trace_expression('Main', Main) :- Main=var(s, (size(var(s)):app(Repeat, [var('s')]))), Repeat=gen(['n'], guarded((var('n')>0), (eventType:app(Repeat, [(var('n')-1)])), eps)).
