:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, available(Total)) :- deep_subdict(_{'res':Total,'name':"available",'event':"func_post"}, Event).
match(Event, use(Total, Used)) :- deep_subdict(_{'args':[Total, Used],'name':"use",'event':"func_pre"}, Event).
match(Event, relevant) :- match(Event, available(_)).
match(Event, relevant) :- match(Event, use(_, _)).
match(_, any).
trace_expression('Main', Main) :- Use=gen(['total'], guarded((var('total')>0), var(used, ((use(var(total), var(used)):eps)*app(Use, [(var('total')-var('used'))]))), eps)), Main=(((relevant>>var(total, clos(((available(var(total)):eps)*app(Use, [var('total')])))));1)).
