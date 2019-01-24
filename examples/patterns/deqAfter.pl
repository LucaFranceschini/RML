%%% generic DeqAfter as specified in listing 20 of ecoop19 paper  %%%
%%% this is a test for generics with multiple parameters                %%%

:- module(spec,[trace_expression/2, match/2]).

:- use_module(node(func_match)).

match(Json, deq(Elem)) :- func_post(Json, 'deq', _Args, Elem).

match(Json, deq) :- func_post_name(Json , 'deq').

trace_expression(main, DeqAfter) :- 
    DeqAfter = gen([n,val],eps\/guarded((var(n)>1),deq:app(DeqAfter,[var(n)-1,var(val)]),deq(var(val)):1)).

%% a simple test
%% ok
%% trace_expression(main,G),next(app(G,[4/2+1,40+2]),_{event:func_post,name:deq,args:[],res:1},T1), next(T1,_{event:func_post,name:deq,args:[],res:2},T2),next(T2,_{event:func_post,name:deq,args:[],res:42},T3).
%% ok
%% trace_expression(main,G),next(app(G,[4/2,40+2]),_{event:func_post,name:deq,args:[],res:1},T1), next(T1,_{event:func_post,name:deq,args:[],res:42},T2).
