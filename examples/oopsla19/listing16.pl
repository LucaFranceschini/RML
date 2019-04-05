:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, new(Id)) :- deep_subdict(_{'resultId':Id,'class':"stack",'event':"alloc"}, Event).
match(Event, free(Id)) :- deep_subdict(_{'argsId':[Id],'event':"dealloc"}, Event).
match(Event, push(Id, Val)) :- deep_subdict(_{'args':[Val],'targetId':Id,'name':"push",'event':"func_pre"}, Event).
match(Event, pop(Id, Val)) :- deep_subdict(_{'result':Val,'targetId':Id,'name':"pop",'event':"func_post"}, Event).
match(Event, size(Id, S)) :- deep_subdict(_{'result':S,'targetId':Id,'name':"size",'event':"func_post"}, Event).
match(_, any).
trace_expression('Main', Main) :- Stack=gen(['id', 's'], optional((star(size(var(id), var(s)))*var(val, (push(var(id), var(val)):(app(Stack, [var('id'), (var('s')+1)])*(pop(var(id), var(val)):app(Stack, [var('id'), var('s')])))))))), Main=optional(var(id, (new(var(id)):(Main|(app(Stack, [var('id'), 0])*(free(var(id)):eps)))))).
