:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, available(Total)) :- deep_subdict(_{'total':Total}, Event).
match(Event, use(Total, Used)) :- deep_subdict(_{'used':Used,'total':Total}, Event).
match(_, any).
trace_expression('Main', Main) :- Use=gen(['total'], guarded((var('total')>0), (use(var(total), var(used)):app(Use, [(var('total')-var('used'))])), eps)), Main=var(total, clos((available(var(total)):app(Use, [var('total')])))).
