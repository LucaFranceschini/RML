:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, temp_sensor_in_range(Min, Max)) :- deep_subdict(_{'temperature':Temp}, _event), ','((Min=<Temp), (Temp=<Max)).
match(_event, relevant) :- deep_subdict(_{'temperature':_}, _event).
match(_, any).
trace_expression('Main', Main) :- Main=((relevant>>app(CheckRange, [22, 25]));1), CheckRange=gen(['min', 'max'], star((temp_sensor_in_range(var(min), var(max)):eps))).
