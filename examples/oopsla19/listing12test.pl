:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, acquire(Id)) :- deep_subdict(_{'targetId':Id,'name':"acquire",'event':"func_pre"}, Event).
match(Event, release(Id)) :- deep_subdict(_{'targetId':Id,'name':"release",'event':"func_pre"}, Event).
match(Event, use(Id)) :- deep_subdict(_{'targetId':Id,'name':"use",'event':"func_pre"}, Event).
match(Event, event(Id)) :- match(Event, acquire(var(id))).
match(Event, event(Id)) :- match(Event, use(var(id))).
match(Event, event(Id)) :- match(Event, release(var(id))).
match(Event, acqRel(Id)) :- match(Event, acquire(var(id))).
match(Event, acqRel(Id)) :- match(Event, release(var(id))).
match(Event, filter) :- match(Event, acquire(_)).
match(Event, filter) :- match(Event, release(_)).
match(Event, filter) :- match(Event, use(_)).
match(_, any).
trace_expression('Main', Main) :- Main=optional(var(id, (acquire(var(id)):((star((Main|use(var(id))))*(release(var(id)):eps))/\(((acqRel(var(id))>>(release(var(id)):1));1)))))).
