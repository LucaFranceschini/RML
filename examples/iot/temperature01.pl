:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, temp_sensor(Temp, Min, Max)) :- deep_subdict(_{'temperature':Temp}, Event), ','((Temp>=Min), (Temp=<Max)).
match(_, any).
trace_expression('Main', Main) :- Main=app(Check, [25.4, 25.7]), Check=gen(['min', 'max'], star(var(temp, (temp_sensor(var(temp), var(min), var(max)):eps)))).
