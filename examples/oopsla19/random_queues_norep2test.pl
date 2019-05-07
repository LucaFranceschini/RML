:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, enq(Val)) :- deep_subdict(_{'args':[Val],'name':"enqueue",'event':"func_pre"}, Event).
match(Event, deq(Val)) :- deep_subdict(_{'res':Val,'name':"dequeue",'event':"func_post"}, Event).
match(Event, is_val(Val)) :- match(Event, enq(Val)).
match(Event, is_val(Val)) :- match(Event, deq(Val)).
match(Event, is_not_val(Val)) :- not(match(Event, is_val(Val))).
match(Event, relevant) :- match(Event, enq(_)).
match(Event, relevant) :- match(Event, deq(_)).
match(_, any).
trace_expression('Main', Main) :- Main=clos((((relevant>>var(val, ((enq(var(val)):eps)*((((is_val(var(val))>>(star((enq(var(val)):eps))*((deq(var(val)):eps)*star((plus((enq(var(val)):eps))*(deq(var(val)):eps))))));1))/\(((is_not_val(var(val))>>Main);1))))));1))).
