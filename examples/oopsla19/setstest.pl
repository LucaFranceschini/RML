:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, acquire(El)) :- deep_subdict(_{'res':'true','args':[El],'name':"insert",'event':"func_post"}, Event).
match(Event, release(El)) :- deep_subdict(_{'res':'true','args':[El],'name':"remove",'event':"func_post"}, Event).
match(Event, use(El)) :- deep_subdict(_{'res':'false','args':[El],'name':"insert",'event':"func_post"}, Event).
match(Event, toCheck(El)) :- match(Event, acquire(El)).
match(Event, toCheck(El)) :- match(Event, release(El)).
match(Event, toCheck(El)) :- match(Event, no_rm(El)).
match(Event, no_rm(El)) :- deep_subdict(_{'res':'false','args':[El],'name':"remove",'event':"func_post"}, Event).
match(Event, no_rm) :- match(Event, no_rm(_)).
match(Event, filter) :- match(Event, toCheck(_)).
match(Event, filter) :- match(Event, use(_)).
match(_, any).
trace_expression('Main', Main) :- Main=(((filter>>optional((star((no_rm:eps))*var(el, ((acquire(var(el)):eps)*((Main|(star((use(var(el)):eps))*(release(var(el)):eps)))/\(((toCheck(var(el))>>((release(var(el)):eps)*1));1))))))));1)).
