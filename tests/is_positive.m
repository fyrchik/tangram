%---------------------------------------------------------------------------%
:- module is_positive.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module int, list.
:- import_module types.

main(!IO) :-
    TestCasesPositive = [
        step(1,0),
        step(0,1),
        step(1,1),
        step(3,1),
        step(1,-1),
        step(-1,2),
        step(-1,6)
    ],
    ResultPositive = negated_filter(is_positive, TestCasesPositive),
    TestCasesNegative = append([step(0,0)], map(negate, TestCasesPositive)),
    ResultNegative = filter(is_positive, TestCasesNegative),
    (
        if ResultPositive = [], ResultNegative = []
        then
            io.write_string("ok\n", !IO)
        else
            io.write_string("fail\n", !IO),
            io.write_string("False negatives:\n", !IO),
            io.write_line(ResultPositive, !IO),
            io.write_string("False positives:\n", !IO),
            io.write_line(ResultNegative, !IO)
    ).

:- func negate(elem) = elem is det.
negate(step(A,B)) = step(-A,-B).
negate(turn(D)) = turn(D).
%---------------------------------------------------------------------------%