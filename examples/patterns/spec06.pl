%%% Specification for queues  %%%

:- module(spec,[trace_expression/2, match/2]).

:- use_module(node(func_match)).

match(Json, filter) :- match(Json, enq).

match(Json, filter) :- match(Json, deq).

match(Json, enq(Elem)) :- func_pre(Json, 'enq', _Id, [Elem], _TargetId).

match(Json, deq(Elem)) :- func_post(Json, 'deq', _Args, Elem).
     
match(Json, enq) :- func_pre_name(Json, 'enq').

match(Json, deq) :- func_post_name(Json , 'deq').

%% full solution, traces also the enqueued elements
%% uses a guarded expression, to avoid it a new event type must be defined to match dequeuing from queue of size>1
trace_expression('test2', filter >>  app(Queue,0)) :- 
    Ndeq = gen(n,eps\/guarded((var(n)>1),deq:app(Ndeq,var(n)-1),deq(var(el)):1)), 
    Queue = gen(size,
		eps\/
		var(el,enq(var(el)):
				   ((deq >> app(Ndeq,var(size)+1)) /\ app(Queue,var(size)+1)))\/
		guarded(var(size)>0,deq:app(Queue,var(size)-1),0)
	       ).

trace_expression('test0', filter >> Queue) :-  %% simpler solution with shuffle
    Queue = eps\/(enq:((eps\/(deq:eps))|Queue)).

trace_expression('test1', filter >>  app(Queue,0)) :- %% more advanced solution, adaptable to check also enqueued elements
    Queue = gen(size,
		eps\/
		(enq:app(Queue,var(size)+1))\/
		guarded(var(size)>0,deq:app(Queue,var(size)-1),0)
	       ).

%% a simple test
%% trace_expression(test0,T0),next(T0,_{event:func_pre,name:enq,args:[1],id:0,targetId:0},T1), next(T1,_{event:func_pre,name:enq,args:[2],id:0,targetId:0},T2),next(T2,_{event:func_post,name:deq,args:[],res:1},T3),next(T3,_{event:func_pre,name:enq,args:[3],id:0,targetId:0},T4),next(T4,_{event:func_post,name:deq,args:[],res:1},T5),next(T5,_{event:func_post,name:deq,args:[],res:1},T6),next(T6,_{event:func_pre,name:enq,args:[1],id:0,targetId:0},T7).

%% trace_expression(test1,T0),next(T0,_{event:func_pre,name:enq,args:[1],id:0,targetId:0},T1), next(T1,_{event:func_pre,name:enq,args:[2],id:0,targetId:0},T2),next(T2,_{event:func_post,name:deq,args:[],res:1},T3),next(T3,_{event:func_pre,name:enq,args:[3],id:0,targetId:0},T4),next(T4,_{event:func_post,name:deq,args:[],res:1},T5),next(T5,_{event:func_post,name:deq,args:[],res:1},T6),next(T6,_{event:func_pre,name:enq,args:[1],id:0,targetId:0},T7).

%% trace_expression(test2,T0),next(T0,_{event:func_pre,name:enq,args:[1],id:0,targetId:0},T1), next(T1,_{event:func_pre,name:enq,args:[2],id:0,targetId:0},T2),next(T2,_{event:func_post,name:deq,args:[],res:1},T3),next(T3,_{event:func_pre,name:enq,args:[3],id:0,targetId:0},T4),next(T4,_{event:func_post,name:deq,args:[],res:2},T5),next(T5,_{event:func_post,name:deq,args:[],res:3},T6),next(T6,_{event:func_pre,name:enq,args:[4],id:0,targetId:0},T7),next(T7,_{event:func_post,name:deq,args:[],res:4},T8).

