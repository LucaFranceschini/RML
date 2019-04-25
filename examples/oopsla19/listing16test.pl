:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, new(Id)) :- deep_subdict(_{'res':Id,'name':"mynew",'event':"func_post"}, Event).
match(Event, free(Id)) :- deep_subdict(_{'args':[Id],'name':"myfree",'event':"func_pre"}, Event).
match(Event, push(Id, Val)) :- deep_subdict(_{'args':[Id, Val],'name':"mypush",'event':"func_pre"}, Event).
match(Event, pop(Id, Val)) :- deep_subdict(_{'res':Val,'args':[Id],'name':"mypop",'event':"func_post"}, Event).
match(Event, size(Id, S)) :- deep_subdict(_{'res':S,'args':[Id],'name':"mysize",'event':"func_post"}, Event).
match(Event, filter) :- match(Event, new(_)).
match(Event, filter) :- match(Event, free(_)).
match(Event, filter) :- match(Event, push(_, _)).
match(Event, filter) :- match(Event, pop(_, _)).
match(Event, filter) :- match(Event, size(_, _)).
match(_, any).
trace_expression('Main', Main) :- Stack=gen(['id', 's'], (star(size(var(id), var(s)))*optional(var(val, (push(var(id), var(val)):(app(Stack, [var('id'), (var('s')+1)])*(pop(var(id), var(val)):app(Stack, [var('id'), var('s')])))))))), Main=(((filter>>optional(var(id, (new(var(id)):(Main|(app(Stack, [var('id'), 0])*(free(var(id)):eps)))))));1)).
