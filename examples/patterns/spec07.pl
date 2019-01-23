%%% Specification for stacks  %%%

:- module(spec,[trace_expression/2, match/2]).

:- use_module(node(func_match)).

match(Json, filter) :- match(Json, push).

match(Json, filter) :- match(Json, pop).

match(Json, push(Elem)) :- func_pre(Json, 'push', _Id, [Elem], _TargetId).

match(Json, pop(Elem)) :- func_post(Json, 'pop', _Args, Elem).
     
match(Json, push) :- func_pre_name(Json, 'push').

match(Json, pop) :- func_post_name(Json , 'pop').

gt0(N) :- integer(N), N >0.

%% full solution, traces also the  elements
%% uses a guarded expression
trace_expression('test1', filter >>  app(Stack,0)) :- 
    PushPop = gen(n,eps\/((push:app(PushPop,var(n)+1))\/guarded(var(n)>0,pop:app(PushPop,var(n)-1),pop(var(el)):1))),
    Stack = gen(size,
		eps\/
		var(el,push(var(el)):(app(PushPop,0) /\ app(Stack,var(size)+1)))\/
		guarded(var(size)>0,pop:app(Stack,var(size)-1),0)). 

trace_expression('test0', filter >> Stack) :-  %% simpler solution with shuffle
    Stack = eps\/(push:((eps\/(pop:eps))|Stack)).

%% a simple test
%% trace_expression(test0,T0),next(T0,_{event:func_pre,name:push,args:[1],id:0,targetId:0},T1), next(T1,_{event:func_pre,name:push,args:[2],id:0,targetId:0},T2),next(T2,_{event:func_post,name:pop,args:[],res:1},T3),next(T3,_{event:func_pre,name:push,args:[3],id:0,targetId:0},T4),next(T4,_{event:func_post,name:pop,args:[],res:1},T5),next(T5,_{event:func_post,name:pop,args:[],res:1},T6),next(T6,_{event:func_pre,name:push,args:[1],id:0,targetId:0},T7).

%% trace_expression(test1,T0),next(T0,_{event:func_pre,name:push,args:[1],id:0,targetId:0},T1), next(T1,_{event:func_pre,name:push,args:[2],id:0,targetId:0},T2),next(T2,_{event:func_post,name:pop,args:[],res:2},T3),next(T3,_{event:func_pre,name:push,args:[3],id:0,targetId:0},T4),next(T4,_{event:func_post,name:pop,args:[],res:3},T5),next(T5,_{event:func_post,name:pop,args:[],res:1},T6),next(T6,_{event:func_pre,name:push,args:[4],id:0,targetId:0},T7),next(T7,_{event:func_post,name:pop,args:[],res:4},T8).

