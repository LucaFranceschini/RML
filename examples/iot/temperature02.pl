:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, temp_sensor(Temp, Time)) :- deep_subdict(_{'timestamp':Time,'temperature':Temp}, Event).
match(Event, check(Temp1, Time1, Temp2, Time2)) :- match(Event, temp_sensor(Temp2, Time2)), =<(Temp1, (3-4)).
match(_, any).
trace_expression('Main', Main) :- Check=gen(['temp1', 'time1'], star(var(temp2, var(time2, (check(var(temp1), var(time1), var(temp2), var(time2)):eps))))), Main=var(temp1, var(time1, ((temp_sensor(var(temp1), var(time1)):eps)*app(Check, [var('temp1'), var('time1')])))).
