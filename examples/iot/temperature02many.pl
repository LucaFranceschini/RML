:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, is_id(Id)) :- deep_subdict(_{'id':Id}, _event).
match(_event, temp_sensor(Id, Temp, Time)) :- deep_subdict(_{'timestamp':Time,'temperature':Temp,'id':Id}, _event).
match(_event, check_var(Id, Temp1, Time1, Temp2, Time2)) :- match(_event, temp_sensor(Id, Temp2, Time2)), ','(','((Delta=((Temp2-Temp1)/(Time2-Time1))), (-(0.5)=<Delta)), (Delta=<0.5)).
match(_, any).
trace_expression('Main', Main) :- CheckVars=gen(['id', 'temp1', 'time1'], var(temp2, var(time2, ((check_var(var(id), var(temp1), var(time1), var(temp2), var(time2)):eps)*app(CheckVars, [var('id'), var('temp2'), var('time2')]))))), Main=var(id, var(temp1, var(time1, ((temp_sensor(var(id), var(temp1), var(time1)):eps)*((is_id(var(id))>>app(CheckVars, [var('id'), var('temp1'), var('time1')]));Main))))).
