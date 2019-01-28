:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, hasNext(Res)) :- deep_subdict(_{'result':Res,'name':"hasNext",'event':"func_post"}, Event).
match(Event, next) :- deep_subdict(_{'name':"next",'event':"func_pre"}, Event).
match(Event, nonStruct) :- deep_subdict(_{'name':"contains",'event':"func_pre"}, Event).
match(Event, nonStruct) :- deep_subdict(_{'name':"containsAll",'event':"func_pre"}, Event).
match(Event, notIt) :- not(match(Event, hasNext(_))).
match(Event, notIt) :- not(match(Event, next)).
match(Event, notCol) :- not(match(Event, nonStruct)).
match(Event, notCol) :- not(match(Event, hasNext(var(false)))).
trace_expression('Collection', Collection) :- Collection=(nonStruct:(hasNext(var(false)):eps)), Iterator=(hasNext(var(false)):(rop:(hasNext(var(true)):(next:Iterator)))), Main=((Iterator*(huffleop:(notIt:eps)))*(ndop:(Collection*(huffleop:(notCol:eps))))).
