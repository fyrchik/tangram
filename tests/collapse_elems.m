%---------------------------------------------------------------------------%
:- module collapse_elems.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module list.
:- import_module types.
:- import_module utils.

main(!IO) :-
    TestCases = [
        [
            [turn(1), turn(2)],
            [turn(3)]
        ],
        [
            [step(0,1), step(1,0)],
            [step(1,1)]
        ],
        [
            [turn(1), step(1,2), step(1,1), step(1,0), turn(2), turn(3), step(3,2)],
            [turn(1), step(3,3), turn(5), step(3,2)]
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
    io.write_string("]\n", !IO),
    write_traversal3(T, !IO).

:- pred test_single(list(figure)::in) is semidet.
test_single([Input, Result]) :-
    Result = collapse_elems(Input).

%---------------------------------------------------------------------------%