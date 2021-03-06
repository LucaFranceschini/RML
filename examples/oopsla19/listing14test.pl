:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, push(Val)) :- deep_subdict(_{'args':[Val],'name':"mypush",'event':"func_pre"}, Event).
match(Event, pop(Val)) :- deep_subdict(_{'res':Val,'name':"mypop",'event':"func_post"}, Event).
match(Event, relevant) :- match(Event, push(_)).
match(Event, relevant) :- match(Event, pop(_)).
match(_, any).
trace_expression('Main', Main) :- Main=(((relevant>>optional(var(val, ((push(var(val)):eps)*(Main*((pop(var(val)):eps)*Main))))));1)).
