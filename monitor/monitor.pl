:- use_module(library(http/json)).
:- use_module(trace_expressions_semantics).
:- initialization(main).

%% arguments
%% - a specification file
%% - a log file containing the trace
%% optional
%% - --silent

main :-
	current_prolog_flag(argv, [SpecFile, TraceFile|_]),
	use_module(SpecFile),
	trace_expression(_, TraceExp),
	read_trace(TraceFile, Events),
	(verify(Events, TraceExp, 1) ->
		(log('Execution terminated correctly\n'), halt(0)) ;
		(log('Trace did not match specification\n'), halt(1))).

main :-
	(current_prolog_flag(argv, [_, _])
	 -> write('internal error\n')
	 ;  write('expected args: <spec file> <trace file>\n')
	),
	halt(1).

% true if --silent flag was given
silent :- current_prolog_flag(argv, [_, _, '--silent']).

% only print if not in silent mode
log(X) :- silent -> true ; write(X).
lognl  :- silent -> true ; nl.

read_trace(TraceFile, Events) :-
    catch(
    	open(TraceFile, read, TraceStream),
    	_,
    	(write('trace file not found'), nl, halt(1))),
    read_events(TraceStream, Events),
    close(TraceStream).

read_events(TraceStream, []) :-
    at_end_of_stream(TraceStream).

read_events(TraceStream, [Event|Events]) :-
    \+ at_end_of_stream(TraceStream),
    json_read_dict(TraceStream, Event),
    read_events(TraceStream, Events).

verify([], TE, _) :- may_halt(TE) ->
	true ;
	(log('Unexpected end of trace\n'), false).

verify([E|Es], TE, N) :-
	dict_pairs(E, _, Fields),
	(next(TE, E, TE2)
	 -> (log('matched event #'), log(N), lognl, N2 is N+1, verify(Es, TE2, N2))
	 ;  (log('ERROR on event '), log(Fields), lognl, false)
	).
