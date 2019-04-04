:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, enq(Val)) :- deep_subdict(_{'args':[Val],'name':"enqueue",'event':"func_pre"}, Event).
match(Event, deq(Val)) :- deep_subdict(_{'result':Val,'name':"dequeue",'event':"func_post"}, Event).
match(Event, size(S)) :- deep_subdict(_{'result':S,'name':"size",'event':"func_post"}, Event).
match(Event, deq) :- match(Event, deq(_)).
match(_, any).
trace_expression('Main', Main) :- DeqAfter=gen(['n', 'val'], guarded((var('n')>0), (deq:app(DeqAfter, [(var('n')-1), var('val')])), (deq(var(val)):1))), Queue=gen(['s'], ((var(val, (enq(var(val)):((((deq>>app(DeqAfter, [var('s'), var('val')]));1))/\app(Queue, [(var('s')+1)]))))\/guarded((var('s')>0), (deq:app(Queue, [(var('s')-1)])), eps))\/(size(var(s)):app(Queue, [var('s')])))), Main=app(Queue, [0]).