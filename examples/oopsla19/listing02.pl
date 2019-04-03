:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, hasNext(Target, Res)) :- deep_subdict(_{'result':Res,'targetId':Target,'name':"hasNext",'event':"func_post"}, Event).
match(Event, next(Target)) :- deep_subdict(_{'targetId':Target,'name':"next",'event':"func_pre"}, Event).
match(_, any).
trace_expression('Main', Main) :- Iterator1=((hasNext(1, 'false'):eps)\/(hasNext(1, 'true'):(next(1):Iterator1))), Iterator2=((hasNext(2, 'false'):eps)\/(hasNext(2, 'true'):(next(2):Iterator2))), Main=(Iterator1|Iterator2).
