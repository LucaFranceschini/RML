%%% Parametric specification for optimal monitoring  %%% 
%%% of exclusive access to multiple resources           %%%

%%% Parametricity allows simultaneous access        %%%
%%% to an arbitrary number of resources                 %%%

%%% Monitoring is optimal for both time and space  %%%
%%% Time is linear in the size of the trace               %%%
%%% Space is linear in the maximum number of      %%%
%%% simultaneously accessed resources                 %%%

:- module(spec,[trace_expression/2, match/2]).

:- use_module(node(func_match)).

match(Json, filter) :- func_pre_names(Json , ['use','release']).

match(Json, filter) :- func_post_name(Json , 'acquire').

match(Json, acquire_release) :- func_post_name(Json , 'acquire').
     
match(Json, acquire_release) :- func_pre_name(Json , 'release').

match(Json, acquire(Res)) :- func_post(Json, 'acquire', _Args, Res).

match(Json, acquire) :- match(Json, acquire(_)).

match(Json, use(ResourceId)) :- func_pre(Json, 'use', _Id, [ResourceId|_], _TargetId).

match(Json, release(ResourceId)) :- func_pre(Json, 'release', _Id, [ResourceId|_], _TargetId).

match(Json, release) :- match(Json, release(_)).

match(Json, resource(ResId)) :- match(Json,acquire(ResId));match(Json,use(ResId));match(Json,release(ResId)).

%% this spec enforces that acquire(resId) cannot follow another acquire(resId) without a release(resId) in between
trace_expression('test', filter >> (MainFlow /\ Exclusion)) :-
    MainFlow = var(resId, acquire(var(resId)) : (Use | MainFlow)), 
    Use = (release(var(resId)) : eps) \/ (use(var(resId)) : Use), 
    Exclusion = acquire_release >> AcquireReleaseFlow,
    AcquireReleaseFlow = var(resId, acquire(var(resId)) : ((resource(var(resId))>>(release(var(resId)):1)) /\ (AnyRelease*AcquireReleaseFlow))),
    AnyRelease = eps\/(release:AnyRelease).  %%% this is yet another kind of filter

%% a simple test    
%% trace_expression(_,T0),next(T0,_{event:func_post,name:acquire,args:[],res:41},T1),next(T1,_{event:func_post,name:acquire,args:[],res:42},T2),next(T2,_{event:func_pre,name:release,args:[41],id:0,targetId:0},T3),next(T3,_{event:func_post,name:acquire,args:[],res:41},T4),next(T4,_{event:func_pre,name:release,args:[41],id:0,targetId:0},T5),next(T5,_{event:func_pre,name:release,args:[42],id:0,targetId:0},T6),next(T6,_{event:func_post,name:acquire,args:[],res:41},T7),next(T7,_{event:func_pre,name:use,args:[41],id:0,targetId:0},T8),next(T8,_{event:func_post,name:acquire,args:[],res:42},T9),next(T9,_{event:func_pre,name:release,args:[42],id:0,targetId:0},T10),next(T10,_{event:func_pre,name:release,args:[41],id:0,targetId:0},T11).
