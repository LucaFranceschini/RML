:- use_module(library(http/json)).
:- use_module(trace_expressions_semantics).
:- initialization(main).

%% arguments
%% - a specification file
%% - a log file containing the trace
%% optional
%% - --silent
%% - --reject

main :-
	current_prolog_flag(argv, [SpecFile, TraceFile | _]), !,
	catch(
		(use_module(SpecFile), trace_expression(_, TraceExp)),
		_,
		(write('File not found\n'), halt(1))),
	catch(
		read_trace(TraceFile, Events),
		_,
		(write('Illegal JSON object\n'), halt(1))),
	(verify(Events, TraceExp, 1) -> Accepted=true ; Accepted=false),
	(reject -> negate(Accepted, Result) ; Result=Accepted),
	(Result=true ->
		(log('Execution terminated correctly\n'), halt(0)) ;
		(log('Trace did not match specification\n'), halt(1))).

main :-
	write('expected args: <spec file> <trace file>\n'),
	halt(1).

negate(false, true).
negate(true, false).

% true if --silent flag was given
silent :-
	current_prolog_flag(argv, [_, _ | Arguments]),
	member('--silent', Arguments).

% true if --reject flag was given
reject :-
	current_prolog_flag(argv, [_, _ | Arguments]),
	member('--reject', Arguments).

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
