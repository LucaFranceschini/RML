:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, hasNext(Res)) :- deep_subdict(_{'result':Res,'name':"hasNext",'event':"func_post"}, Event).
match(Event, next) :- deep_subdict(_{'name':"next",'event':"func_pre"}, Event).
match(Event, it) :- match(Event, hasNext(_)).
match(Event, it) :- match(Event, next).
match(Event, col) :- match(Event, nonStruct).
match(Event, col) :- match(Event, hasNext('false')).
match(_, any).
trace_expression('Main', Main) :- Collection=(star(nonStruct)*(hasNext('false'):eps)), Iterator=((hasNext('false'):eps)\/(hasNext('true'):(next:Iterator))), Main=((((it>>Iterator);1))/\(((col>>Collection);1))).
