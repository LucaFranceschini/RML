:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, acquire(Id)) :- deep_subdict(_{'resultId':Id,'name':"iterator",'event':"func_post"}, Event).
match(Event, release(Id)) :- deep_subdict(_{'result':'false','targetId':Id,'name':"hasNext",'event':"func_post"}, Event).
match(Event, use(Id)) :- deep_subdict(_{'result':'true','targetId':Id,'name':"hasNext",'event':"func_post"}, Event).
match(Event, use(Id)) :- deep_subdict(_{'targetId':Id,'name':"next",'event':"func_pre"}, Event).
match(_, any).
trace_expression('Main', Main) :- Main=(eps\/var(id, (acquire(var(id)):(star((Main|use(var(id))))*(release(var(id)):eps))))).
