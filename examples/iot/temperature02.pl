:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, temp_sensor(Temp)) :- deep_subdict(_{'temperature':Temp}, Event).
match(_, any).
trace_expression('Main', Main) :- Main=app(Check, [16.85, 40]), Check=gen(['min', 'max'], star(var(temp, with(temp_sensor(var(temp)), eps, (((var('temp')>=var('min')),(var('temp')=<var('max')))))))).
