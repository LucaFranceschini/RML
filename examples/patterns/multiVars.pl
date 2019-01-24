%%% Test for multiple variable declarations  %%%

:- module(spec,[trace_expression/2, match/2]).

:- use_module(node(func_match)).

match(Json, enq(Elem)) :- func_pre(Json, 'enq', _Id, [Elem], _TargetId).


%% full solution, traces also the enqueued elements
%% uses a guarded expression, to avoid it a new event type must be defined to match dequeuing from queue of size>1
trace_expression('main', var([x,y],enq(var(x)):enq(var(y)):enq(var(x)):enq(var(y)):eps)).

%% a simple test
%% trace_expression(main,T0),next(T0,_{event:func_pre,name:enq,args:[1],id:0,targetId:0},T1), next(T1,_{event:func_pre,name:enq,args:[2],id:0,targetId:0},T2),next(T2,_{event:func_pre,name:enq,args:[1],id:0,targetId:0},T3), next(T3,_{event:func_pre,name:enq,args:[2],id:0,targetId:0},T4).
