:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, acquire(El)) :- deep_subdict(_{'result':'true','args':[El],'name':"add",'event':"func_post"}, Event).
match(Event, release(El)) :- deep_subdict(_{'result':'true','args':[El],'name':"remove",'event':"func_post"}, Event).
match(Event, use(El)) :- deep_subdict(_{'result':'false','args':[El],'name':"add",'event':"func_post"}, Event).
match(Event, toCheck(El)) :- match(Event, acquire(El)).
match(Event, toCheck(El)) :- match(Event, release(El)).
match(Event, toCheck(El)) :- match(Event, no_rm(El)).
match(Event, no_rm(El)) :- deep_subdict(_{'result':'false','args':[El],'name':"remove",'event':"func_post"}, Event).
match(Event, no_rm) :- match(Event, no_rml(_)).
match(_, any).
trace_expression('Main', Main) :- Main=optional((star((no_rm:eps))*var(el, ((acquire(var(el)):eps)*((Main|(star((use(var(el)):eps))*(release(var(el)):eps)))/\(((toCheck(var(el))>>((release(var(el)):eps)*1));1))))))).
