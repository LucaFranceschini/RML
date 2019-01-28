:- module('spec', [trace_expression/2, match/2]).
:- use_module(monitor('deep_subdict')).
match(Event, hasNext(Res)) :- deep_subdict(_{'result':Res,'name':"hasNext",'event':"func_post"}, Event).
match(Event, next) :- deep_subdict(_{'name':"next",'event':"func_pre"}, Event).
match(Event, nonStruct) :- deep_subdict(_{'name':"contains",'event':"func_pre"}, Event).
match(Event, nonStruct) :- deep_subdict(_{'name':"containsAll",'event':"func_pre"}, Event).
match(Event, notIt) :- not(match(Event, hasNext(_))), not(match(Event, next)).
match(Event, notCol) :- not(match(Event, nonStruct)), not(match(Event, hasNext(var(false)))).
trace_expression('NotIt', NotIt) :- NotIt=(eps\/(notIt:NotIt)), NotCol=(eps\/(notCol:NotCol)), NonStruct=(eps\/(nonStruct:NonStruct)), Collection=(NonStruct*(hasNext(var(false)):eps)), Iterator=((hasNext(var(false)):eps)\/(hasNext(var(true)):(next:Iterator))), Main=((Iterator|NotIt)/\(Collection|NotCol)).
