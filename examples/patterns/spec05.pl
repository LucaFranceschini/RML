%%% Generic specification to deal with states with integer parameters    %%%

:- module(spec,[trace_expression/2, match/2]).

:- use_module(node(func_match)).

match(Json, filter) :- match(Json, end(0)).

match(Json, filter) :- func_post_name(Json , 'consume').

match(Json, consume(Size1,Size2)) :- func_post(Json, 'consume', _Args, Bytes), Bytes >=0, Size2 is Size1-Bytes, Size2 >=0.

match(Json, end(0)) :- func_pre_name(Json , 'end').

%% this spec enforces that acquire(resId) cannot follow another acquire(resId) without a release(resId) in between
trace_expression('test', filter >>  app(Main,42)) :- %%% end must occur only if all 42 bytes have been consumed
    Main = gen(size,var(newSize,consume(var(size),var(newSize)):app(Main,var(newSize)))\/(end(var(size)):eps)).

%% a simple test
%% trace_expression(_,T0),next(T0,_{event:func_post,name:consume,args:[],res:41},T1),next(T1,_{event:func_post,name:consume,args:[],res:1},T2),next(T2,_{event:func_pre,name:end,args:[],id:0,targetId:0},T3).
