%%% example on the use of the prefix closure operator, based on listing 6 of the ecoop19 paper

:- module(spec,[trace_expression/2, match/2]).

:- use_module(node(func_match)).
:- use_module(node(trace_expressions_semantics)).
     
match(Json, hasNext(Res)) :- func_post(Json, 'hasNext', _Args, Res).
match(Json, next) :- func_pre(Json, 'next', _Id, _Args, _ArgIds, _TargetId).
match(Json, exit) :- func_pre(Json, 'exit', _Id, _Args, _ArgIds, _TargetId).

%% non conclusive spec for iterators

iterator(Iterator) :- Iterator = (hasNext(false):eps) \/ (hasNext(true):(next:Iterator)).

trace_expression('non conclusive',Iterator*(exit:eps)) :- iterator(Iterator).

%% partially conclusive version

trace_expression('partially conclusive',clos(Iterator)*(exit:eps)) :- iterator(Iterator).

%% some tests    
%% ok
%% trace_expression('non conclusive',T0),next(T0,_{event:func_post,name:hasNext,args:[],res:false},T1),next(T1,_{event:func_pre,name:exit,args:[],argIds:[],id:null,targetId:null},T2).
%% fails
%% trace_expression('non conclusive',T0),next(T0,_{event:func_post,name:hasNext,args:[],res:true},T1),next(T1,_{event:func_pre,name:exit,args:[],argIds:[],id:null,targetId:null},T2).
%% ok
%% trace_expression('partially conclusive',T0),next(T0,_{event:func_post,name:hasNext,args:[],res:false},T1),next(T1,_{event:func_pre,name:exit,args:[],argIds:[],id:null,targetId:null},T2).
%% ok
%% trace_expression('partially conclusive',T0),next(T0,_{event:func_post,name:hasNext,args:[],res:true},T1),next(T1,_{event:func_pre,name:exit,args:[],argIds:[],id:null,targetId:null},T2).
