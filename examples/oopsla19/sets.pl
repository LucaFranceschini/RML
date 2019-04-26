:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, acquire(El)) :- deep_subdict(_{'result':'true','args':[El],'name':"add",'event':"func_post"}, Event).
match(Event, release(El)) :- deep_subdict(_{'result':'true','args':[El],'name':"remove",'event':"func_post"}, Event).
match(Event, use(El)) :- deep_subdict(_{'result':'false','args':[El],'name':"add",'event':"func_post"}, Event).
match(Event, toCheck(El)) :- match(Event, acquire(var(el))).
match(Event, toCheck(El)) :- match(Event, release(var(el))).
match(Event, toCheck(El)) :- match(Event, no_rm(var(el))).
match(Event, no_rm(El)) :- deep_subdict(_{'result':'false','args':[El],'name':"remove",'event':"func_post"}, Event).
match(Event, no_rm) :- match(Event, no_rml(_)).
match(_, any).
trace_expression('Main', Main) :- Main=optional((star(no_rm)*var(el, (acquire(var(el)):((Main|(star(use(var(el)))*(release(var(el)):eps)))/\(((toCheck(var(el))>>(release(var(el)):1));1))))))).
