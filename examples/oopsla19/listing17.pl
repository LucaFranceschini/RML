:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, push(Val)) :- deep_subdict(_{'args':[Val],'name':"push",'event':"func_pre"}, Event).
match(Event, pop(Val)) :- deep_subdict(_{'result':Val,'name':"pop",'event':"func_post"}, Event).
match(Event, size(S)) :- deep_subdict(_{'result':S,'name':"size",'event':"func_post"}, Event).
match(_, any).
trace_expression('Main', Main) :- Stack=gen(['s'], (star(size(var(s)))*(guarded((var('s')==0), eps, 0)\/var(val, (push(var(val)):(app(Stack, [(var('s')+1)])*(pop(var(val)):app(Stack, [var('s')])))))))), Main=app(Stack, [0]).
