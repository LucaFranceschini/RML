:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, push(Val)) :- deep_subdict(_{'args':[Val],'name':"push",'event':"func_pre"}, Event).
match(Event, pop(Val)) :- deep_subdict(_{'result':Val,'name':"pop",'event':"func_post"}, Event).
match(_, any).
trace_expression('Main', Main) :- Main=(eps\/var(val, (push(var(val)):(Main*(pop(var(val)):Main))))).
