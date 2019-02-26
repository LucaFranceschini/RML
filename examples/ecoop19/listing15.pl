:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, acquire(Id)) :- deep_subdict(_{'result':Id,'name':"fs.open",'event':"func_post"}, Event).
match(Event, release(Id)) :- deep_subdict(_{'args':[Id],'name':"fs.close",'event':"func_pre"}, Event).
match(Event, use(Id)) :- deep_subdict(_{'args':[var(id)|_],'name':"fs.read",'event':"func_pre"}, Event).
match(Event, use(Id)) :- deep_subdict(_{'args':[var(id)|_],'name':"fs.write",'event':"func_pre"}, Event).
match(Event, use(Id)) :- deep_subdict(_{'args':[var(id)|_],'name':"fs.fchmod",'event':"func_pre"}, Event).
match(Event, acquire) :- match(Event, acquire(_)).
match(Event, resource(Id)) :- match(Event, acquire(var(id))).
match(Event, resource(Id)) :- match(Event, release(var(id))).
match(Event, acquireOrRelease) :- match(Event, resource(_)).
match(_, any).
trace_expression('Main', Main) :- Resources=(eps\/var(id, (acquire(var(id)):(star((Resources|use(var(id))))*(release(var(id)):eps))))), Exclusive=(eps\/var(id, (acquire(var(id)):((((resource(var(id))>(release(var(id)):1));1))/\(((acquire>Exclusive);1)))))), Main=(Resources/\(((acquireOrRelease>>Exclusive);1))).
