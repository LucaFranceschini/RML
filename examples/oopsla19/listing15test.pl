:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, push(Val)) :- deep_subdict(_{'args':[Val],'name':"mypush",'event':"func_pre"}, Event).
match(Event, pop(Val)) :- deep_subdict(_{'res':Val,'name':"mypop",'event':"func_post"}, Event).
match(Event, size(S)) :- deep_subdict(_{'res':S,'name':"size",'event':"func_post"}, Event).
match(Event, filter) :- match(Event, push(_)).
match(Event, filter) :- match(Event, pop(_)).
match(Event, filter) :- match(Event, size(_)).
match(_, any).
trace_expression('Main', Main) :- Stack=gen(['s'], (star(size(var(s)))*optional(var(val, (push(var(val)):(app(Stack, [(var('s')+1)])*(pop(var(val)):app(Stack, [var('s')])))))))), Main=(((filter>>app(Stack, [0]));1)).
