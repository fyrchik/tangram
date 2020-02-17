%---------------------------------------------------------------------------%
:- module add_turns.

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
        [left(90), right(90), turn(0)],
        [left(180), left(180), turn(0)],
        [right(180), right(180), turn(0)],
        [left(90), left(10), left(100)],
        [left(45), right(40), left(5)],
        [right(180), right(180), turn(0)]
    ],
    Result = negated_filter(test_single, TestCases),
    (
        if Result = []
        then
            io.write_string("ok\n", !IO)
        else
            io.write_string("fail\n", !IO),
            write_traversal(Result, !IO)
    ).

:- pred test_single(list(elem)::in) is semidet.
test_single([First, Second, Result]) :-
    add_turns(First, Second, Result).

%---------------------------------------------------------------------------%