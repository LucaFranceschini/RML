:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, acquire(El)) :- deep_subdict(_{'result':'true','args':[El],'name':"add",'event':"func_post"}, Event).
match(Event, release(El)) :- deep_subdict(_{'result':'true','args':[El],'name':"remove",'event':"func_post"}, Event).
match(Event, use(El)) :- deep_subdict(_{'result':'false','args':[El],'name':"add",'event':"func_post"}, Event).
match(Event, acqRel(El)) :- match(Event, acquire(var(el))).
match(Event, acqRel(El)) :- match(Event, release(var(el))).
match(Event, no_rm) :- deep_subdict(_{'result':'false','args':[El],'name':"remove",'event':"func_post"}, Event).
match(_, any).
trace_expression('Main', Main) :- Main=optional((var(el, (acquire(var(el)):((star((Main|use(var(el))))*(release(var(el)):eps))/\(((acqRel(var(el))>>(release(var(el)):1));1)))))\/(no_rm:Main))).
