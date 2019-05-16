:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, temp_sensor_in_range(Temp, Min, Max)) :- deep_subdict(_{'temperature':Temp}, _event), ','((Min=<Temp), (Temp=<Max)).
match(_, any).
trace_expression('Main', Main) :- Main=app(CheckRange, [0, 40]), CheckRange=gen(['min', 'max'], star(var(temp, (temp_sensor_in_range(var(temp), var(min), var(max)):eps)))).
