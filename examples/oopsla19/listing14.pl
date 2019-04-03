:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, acquire(Id)) :- deep_subdict(_{'result':Id,'name':"fs.open",'event':"func_post"}, Event).
match(Event, release(Id)) :- deep_subdict(_{'args':[Id],'name':"fs.close",'event':"func_pre"}, Event).
match(Event, use(Id)) :- deep_subdict(_{'args':[var(id)|_],'name':"fs.read",'event':"func_pre"}, Event).
match(Event, use(Id)) :- deep_subdict(_{'args':[var(id)|_],'name':"fs.write",'event':"func_pre"}, Event).
match(Event, use(Id)) :- deep_subdict(_{'args':[var(id)|_],'name':"fs.fchmod",'event':"func_pre"}, Event).
match(Event, neq(Id)) :- not(match(Event, acquire(var(id)))), not(match(Event, use(var(id)))), not(match(Event, release(var(id)))).
match(_, any).
trace_expression('Main', Main) :- Main=(eps\/var(id, (acquire(var(id)):(star(((star(neq(var(id)))/\Main)|use(var(id))))*(release(var(id)):eps))))).
