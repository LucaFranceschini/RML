:- use_module(library(http/json)).
:- use_module(trace_expressions_semantics).
:- initialization(main).

%% arguments
%% - a specification file
%% - a log file containing the trace

main :-
	current_prolog_flag(argv, [SpecFile, TraceFile]),
	use_module(SpecFile),
	read_trace(TraceFile),
	halt.
main :-
	write('expected args: <spec file> <trace file>'), nl,
	halt(1).

read_trace(TraceFile) :-
    open(TraceFile, read, TraceStream),
    read_events(TraceStream, Events),
    close(TraceStream),
    trace_expression(_, TraceExp),
    verify(Events, TraceExp, 1).

read_events(TraceStream, []) :-
    at_end_of_stream(TraceStream).

read_events(TraceStream, [Event|Events]) :-
    \+ at_end_of_stream(TraceStream),
    json_read_dict(TraceStream, Event),
    read_events(TraceStream, Events).

verify([], TE, _) :-
	may_halt(TE)
	-> write('Execution terminated correctly\n')
	;  write('Unexpected end of trace\n').

verify([E|Es], TE, N) :-
	next(TE, E, TE2)
	-> (write('matched event #'), write(N), nl, N2 is N+1, verify(Es, TE2, N2))
	;  (write('ERROR on event '), write(E)).
