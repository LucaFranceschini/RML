:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, enq(Val)) :- deep_subdict(_{'args':[Val],'name':"enqueue",'event':"func_pre"}, Event).
match(Event, deq(Val)) :- deep_subdict(_{'res':Val,'name':"dequeue",'event':"func_post"}, Event).
match(Event, deq) :- match(Event, deq(_)).
match(Event, filter) :- match(Event, enq(_)).
match(Event, filter) :- match(Event, deq).
match(_, any).
trace_expression('Main', Main) :- Main=(((filter>>optional(var(val, (enq(var(val)):((((deq>>(deq(var(val)):1));1))/\(deq(var(val)):eps|Main))))));1)).

