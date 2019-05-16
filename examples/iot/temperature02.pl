:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, temp_sensor(Temp, Time)) :- deep_subdict(_{'timestamp':Time,'temperature':Temp}, _event).
match(_event, check_var(Temp1, Time1, Temp2, Time2)) :- match(_event, temp_sensor(Temp2, Time2)), ','(','((Delta=((Temp2-Temp1)/(Time2-Time1))), (-(1)=<Delta)), (Delta=<1)).
match(_, any).
trace_expression('Main', Main) :- CheckVars=gen(['temp1', 'time1'], var(temp2, var(time2, ((check_var(var(temp1), var(time1), var(temp2), var(time2)):eps)*app(CheckVars, [var('temp2'), var('time2')]))))), Main=var(temp1, var(time1, ((temp_sensor(var(temp1), var(time1)):eps)*app(CheckVars, [var('temp1'), var('time1')])))).
