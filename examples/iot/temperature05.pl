:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(_event, temp_sensor(Temp, Time)) :- deep_subdict(_{'timestamp':Time,'temperature':Temp}, _event).
match(_, any).
trace_expression('Main', Main) :- CheckAverageVar=gen(['samples', 'max', 'temp1', 'time1', 'total', 'count'], guarded((var('count')<var('samples')), var(temp2, var(time2, ((temp_sensor(var(temp2), var(time2)):eps)*app(CheckAverageVar, [var('samples'), var('max'), var('temp2'), var('time2'), (var('total')+((var('temp2')-var('temp1'))/(var('time2')-var('time1')))), (var('count')+1)])))), guarded(','((-(var('max'))=<(var('total')/var('samples'))), ((var('total')/var('samples'))=<var('max'))), eps, 0))), AverageVar=gen(['samples', 'max'], var(temp, var(time, ((temp_sensor(var(temp), var(time)):eps)*app(CheckAverageVar, [var('samples'), var('max'), var('temp'), var('time'), 0, 0]))))), Main=star(app(AverageVar, [4, 10])).
