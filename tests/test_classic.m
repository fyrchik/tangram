:- module test_classic.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module int, list.
:- import_module pprint.

:- import_module types.
:- import_module classic.
:- import_module tangram.

main(!IO) :-
    TestCases = [
        [ % tangram can be combined into a square :)
            step(2, 0), left(90), step(2, 0), left(90),
            step(2, 0), left(90), step(2, 0), left(90)
        ]
    ],
    Result = negated_filter(test_single, TestCases),
    (
        if Result = []
        then
            io.write_string("ok\n", !IO)
        else
            pprint.write(80, to_doc(Result), !IO)
    ).

:- pred test_single(figure::in) is semidet.
test_single(Result) :-
    get_tangram_components(Figures),
    combine_list(Figures, Result).

%---------------------------------------------------------------------------%