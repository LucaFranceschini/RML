:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, acquire(Id)) :- deep_subdict(_{'args':[Id|_],'name':"acquire",'event':"func_pre"}, Event).
match(Event, release(Id)) :- deep_subdict(_{'args':[Id|_],'name':"release",'event':"func_pre"}, Event).
match(Event, use(Id)) :- deep_subdict(_{'args':[Id|_],'name':"use",'event':"func_pre"}, Event).
match(Event, filter) :- match(Event, acquire(_)).
match(Event, filter) :- match(Event, release(_)).
match(Event, filter) :- match(Event, use(_)).
match(_, any).
trace_expression('Main', Main) :- Main=(((filter>>optional(var(id, (acquire(var(id)):(star((Main|use(var(id))))*(release(var(id)):eps))))));1)).
