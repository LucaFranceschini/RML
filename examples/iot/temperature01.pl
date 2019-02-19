:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, temp_sensor(Temp)) :- deep_subdict(_{'temperature':Temp}, Event).
match(_, any).
trace_expression('Main', Main) :- Main=app(CheckTemp, [20]), CheckTemp=gen(['temp'], guarded((((var('temp')>=10),(var('temp')=<50))), var(temp, (temp_sensor(var(temp)):(eps\/app(CheckTemp, [var('temp')])))), 0)).
