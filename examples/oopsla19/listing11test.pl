:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, acquire(Id)) :- deep_subdict(_{'args':[Id|_],'name':"acquire",'event':"func_pre"}, Event).
match(Event, release(Id)) :- deep_subdict(_{'args':[Id|_],'name':"release",'event':"func_pre"}, Event).
match(Event, use(Id)) :- deep_subdict(_{'args':[Id|_],'name':"use",'event':"func_pre"}, Event).
match(Event, relevant) :- match(Event, acquire(_)).
match(Event, relevant) :- match(Event, release(_)).
match(Event, relevant) :- match(Event, use(_)).
match(_, any).
trace_expression('Main', Main) :- Main=(((relevant>>optional(var(id, ((acquire(var(id)):eps)*(Main|(star((use(var(id)):eps))*(release(var(id)):eps)))))));1)).
