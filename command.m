%---------------------------------------------------------------------------%
:- module command.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module int, list.
:- import_module solutions.

:- import_module classic.
:- import_module types.
:- import_module tangram.
:- import_module utils.

main(!IO) :-
    get_classic_tangram(Figures),
    (combine_list_with_trace(Figures, Out, Trace); Out = [], Trace = []),
    io.write_string("Result:\n", !IO),
    io.write_line(Out, !IO),
    io.write_string("Trace:\n", !IO),
    write_traversal(Trace, !IO)
    .

%---------------------------------------------------------------------------%