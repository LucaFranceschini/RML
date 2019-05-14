:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, enq(Val)) :- deep_subdict(_{'args':[Val],'name':"enqueue",'event':"func_pre"}, Event).
match(Event, deq(Val)) :- deep_subdict(_{'res':Val,'name':"dequeue",'event':"func_post"}, Event).
match(Event, deq_geq(Val, Min)) :- match(Event, deq(Val)), >=(Val, Min).
match(Event, deq) :- match(Event, deq(_)).
match(Event, relevant) :- match(Event, deq).
match(Event, relevant) :- match(Event, enq(_)).
match(_, any).
trace_expression('Main', Main) :- Queue1=var(val, ((enq(var(val)):eps)*((star((enq(var(val)):eps))*(deq(var(val)):eps))|Queue1))), Queue2=(var(val, ((enq(var(val)):eps)*(var(val2, (((deq_geq(var(val2), var(val))>>((deq(var(val)):eps)*1));1)))/\Queue2)))\/((deq:eps)*Queue2)), Main=(((relevant>>clos((Queue1/\Queue2)));1)).
