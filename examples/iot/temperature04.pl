:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, temp_sensor(Temp, Time)) :- deep_subdict(_{'timestamp':Time,'temperature':Temp}, _event).
match(_event, temp_sensor_in_range(Temp, Min, Max)) :- deep_subdict(_{'temperature':Temp}, _event), ','((Min=<Temp), (Temp=<Max)).
match(_event, temp_sample(Time)) :- match(_event, temp_sensor(_, Time)).
match(_event, check_var(Temp1, Time1, Temp2, Time2, Max)) :- match(_event, temp_sensor(Temp2, Time2)), ','(','((Delta=((Temp2-Temp1)/(Time2-Time1))), (-(Max)=<Delta)), (Delta=<Max)).
match(_event, check_sample(Time1, Time2, Max)) :- match(_event, temp_sample(Time2)), =<((Time2-Time1), Max).
match(_, any).
trace_expression('Main', Main) :- CheckRange=gen(['min', 'max'], star(var(temp, (temp_sensor_in_range(var(temp), var(min), var(max)):eps)))), CheckVars=gen(['temp1', 'time1', 'max'], var(temp2, var(time2, ((check_var(var(temp1), var(time1), var(temp2), var(time2), var(max)):eps)*app(CheckVars, [var('temp2'), var('time2'), var('max')]))))), CheckVar=gen(['max'], var(temp1, var(time1, ((temp_sensor(var(temp1), var(time1)):eps)*app(CheckVars, [var('temp1'), var('time1'), var('max')]))))), CheckSamples=gen(['time1', 'max'], var(time2, ((check_sample(var(time1), var(time2), var(max)):eps)*app(CheckSamples, [var('time2'), var('max')])))), CheckSample=gen(['max'], var(time1, ((temp_sample(var(time1)):eps)*app(CheckSamples, [var('time1'), var('max')])))), Main=((app(CheckRange, [25, 26])/\app(CheckVar, [1]))/\app(CheckSample, [1.5])).
