:- module combine_list.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module list.
:- import_module types.
:- import_module utils.
:- import_module tangram.

main(!IO) :-
    % Result is a first element of a testcase.
    % Note: it must be in a normalized form.
    TestCases = [
        [
            [step(2,0), left(90), step(1,0), left(90), step(2,0), left(90), step(1,0), left(90)],
            [step(1,0), left(90), step(1,0), left(90), step(1,0), left(90), step(1,0), left(90)],
            [step(1,0), left(90), step(1,0), left(90), step(1,0), left(90), step(1,0), left(90)]
        ]
    ],
    Result = negated_filter(test_single, TestCases),
    (
        if Result = []
        then
            io.write_string("ok\n", !IO)
        else
            io.write_string("fail\n", !IO),
            write_traversal3(Result, !IO)
    ).

:- pred write_traversal3(list(list(figure))::in, io::di, io::uo) is det.
write_traversal3([], !IO).
write_traversal3([H|T], !IO) :-
    io.write_string("[\n", !IO),
    write_traversal(H, !IO),
    io.write_string("]\n", !IO).

:- pred test_single(list(list(elem))::in) is semidet.
test_single([Result | Elems]) :- tangram.combine_list(Elems, Result).

%---------------------------------------------------------------------------%