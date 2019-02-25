:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, hasNext(Id, Res)) :- deep_subdict(_{'result':Res,'targetId':Id,'name':"hasNext",'event':"func_post"}, Event).
match(Event, next(Id)) :- deep_subdict(_{'targetId':Id,'name':"next",'event':"func_pre"}, Event).
match(Event, iterator(Id)) :- deep_subdict(_{'resultId':Id,'name':"iterator",'event':"func_post"}, Event).
match(_, any).
trace_expression('Main', Main) :- Main=(eps\/var(id, (iterator(var(id)):(star((Main|(hasNext(var(id), 'true'):(next(var(id)):eps))))*(hasNext(var(id), 'false'):eps))))).
