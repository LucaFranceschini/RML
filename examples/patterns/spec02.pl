:- module(spec,[trace_expression/2, match/2]).

:- use_module(node(func_match)).

match(Json, filter) :- func_pre_name(Json , Name), member(Name,['open','consume','close']).

match(Json, filter) :- func_post_name(Json , 'open').
     
match(Json, open(ResId)) :- func_post(Json, 'open', _Args, _Res, ResId).

match(Json, consume(TargetId)) :- func_pre(Json, 'consume', _Id, _Args, _ArgIds, TargetId).

match(Json, close(TargetId)) :- func_pre(Json, 'close', _Id, _Args, _ArgIds, TargetId).

match(Json, target(ResId)) :- match(Json,open(ResId));match(Json,consume(ResId));match(Json,close(ResId)).

%% this spec enforces that open can be called more than once, but only after resId has been closed 
trace_expression('test', filter >> Main) :-
    Main = var(resId, open(var(resId)) : (target(var(resId))>>Cons;Main)),
    Cons = (close(var(resId)) : (eps\/(open(var(resId)):Cons))) \/ (consume(var(resId)) : Cons). 

%% a simple test    
%% trace_expression(_,T0),next(T0,_{event:func_post,name:open,args:[],res:0,resultId:41},T1),next(T1,_{event:func_post,name:open,args:[],res:0,resultId:42},T2),next(T2,_{event:func_pre,name:close,args:[],argIds:[],id:0,targetId:41},T3),next(T3,_{event:func_post,name:open,args:[],res:0,resultId:41},T4),next(T4,_{event:func_pre,name:consum,args:[],argIds:[],id:0,targetId:42},T5).
