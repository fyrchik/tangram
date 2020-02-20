%---------------------------------------------------------------------------%
:- module arithmetic.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module list.
:- import_module types.

main(!IO) :-
    TestCases = [
        [step(1,1), step(1,0)],
        [step(2,2), step(1,2)],
        [step(1,2), step(1,1)],
        [step(2,2), step(1,1)],
        [step(4,1), step(1,1)],
        [step(3,-1), step(2,0)],
        [step(3,1), step(4,-1)],
        [step(6,7), step(7,1)]
    ],
    Result = negated_filter(test_single, TestCases),
    (
        if Result = []
        then
            io.write_string("ok\n", !IO)
        else
            io.write_string("fail\n", !IO),
            io.write_line(Result, !IO)
    ).

:- pred test_single(list(elem)::in) is semidet.
test_single([A, B]) :- A > B.

%---------------------------------------------------------------------------%