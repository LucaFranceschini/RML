:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, hasNext(Res)) :- deep_subdict(_{'result':Res,'name':"hasNext",'event':"func_post"}, Event).
match(Event, next) :- deep_subdict(_{'name':"next",'event':"func_pre"}, Event).
trace_expression('Main', Main) :- Main=(hasNext(var(false))\/(hasNext(var(true)):(next:Main))).
