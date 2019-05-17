:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, temp_sensor_in_range(Min, Max)) :- deep_subdict(_{'temperature':Temp}, _event), ','((Min=<Temp), (Temp=<Max)).
match(_, any).
trace_expression('Main', Main) :- Main=app(CheckRange, [22, 24]), CheckRange=gen(['min', 'max'], star((temp_sensor_in_range(var(min), var(max)):eps))).
