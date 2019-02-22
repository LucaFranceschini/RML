:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, temp_sensor(Temp)) :- deep_subdict(_{'temperature':Temp}, Event).
match(_, any).
trace_expression('Main', Main) :- Main=app(CheckTemp, [10,40]), CheckTemp=gen(['min','max'],star(var(temp,with(temp_sensor(var('temp')),eps,((var('temp')>=var('min')),(var('temp')=<var('max'))))))).



									    
