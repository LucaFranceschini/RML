:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, acquire(Id)) :- deep_subdict(_{'targetId':Id,'name':"acquire",'event':"func_pre"}, Event).
match(Event, release(Id)) :- deep_subdict(_{'targetId':Id,'name':"release",'event':"func_pre"}, Event).
match(Event, use(Id)) :- deep_subdict(_{'targetId':Id,'name':"use",'event':"func_pre"}, Event).
match(Event, event(Id)) :- match(Event, acquire(Id)).
match(Event, event(Id)) :- match(Event, use(Id)).
match(Event, event(Id)) :- match(Event, release(Id)).
match(Event, filter) :- match(Event, acquire(_)).
match(Event, filter) :- match(Event, release(_)).
match(Event, filter) :- match(Event, use(_)).
match(_, any).
trace_expression('Main', Main) :- Main=(((filter>>optional(var(id, (acquire(var(id)):(app(Resource, [var('id')])/\Main)))));1)), Resource=gen(['id'], (((event(var(id))>>(star(use(var(id)))*(release(var(id)):1)));1))).
