:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, enq(Val)) :- deep_subdict(_{'args':[Val],'name':"enqueue",'event':"func_pre"}, Event).
match(Event, deq(Val)) :- deep_subdict(_{'res':Val,'name':"dequeue",'event':"func_post"}, Event).
match(Event, deq) :- match(Event, deq(_)).
match(Event, relevant) :- match(Event, enq(_)).
match(Event, relevant) :- match(Event, deq).
match(_, any).
trace_expression('Main', Main) :- DeqAfter=gen(['n', 'val'], guarded((var('n')>0), ((deq:eps)*app(DeqAfter, [(var('n')-1), var('val')])), ((deq(var(val)):eps)*1))), Queue=gen(['s'], (var(val, ((enq(var(val)):eps)*((((deq>>app(DeqAfter, [var('s'), var('val')]));1))/\app(Queue, [(var('s')+1)]))))\/guarded((var('s')>0), ((deq:eps)*app(Queue, [(var('s')-1)])), eps))), Main=(((relevant>>app(Queue, [0]));1)).
