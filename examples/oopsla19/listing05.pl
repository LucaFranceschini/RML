:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, eventType) :- deep_subdict(_{'name':"foo",'event':"func_post"}, Event).
match(_, any).
trace_expression('Main', Main) :- Spec1=(eventType:Spec1), Spec2=star(evenType), Spec3=(eventType:eps), Spec4=(evenType:1), Main=Spec4.
