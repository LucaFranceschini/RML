:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, enq(Val)) :- deep_subdict(_{'args':[Val],'name':"enqueue",'event':"func_pre"}, Event).
match(Event, deq(Val)) :- deep_subdict(_{'res':Val,'name':"dequeue",'event':"func_post"}, Event).
match(_, any).
trace_expression('Main', Main) :- Main=clos(var(val, ((enq(var(val)):eps)*((star((enq(var(val)):eps))*(deq(var(val)):eps))|Main)))).
