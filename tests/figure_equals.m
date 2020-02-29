%---------------------------------------------------------------------------%
:- module figure_equals.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module list.
:- import_module types.

main(!IO) :-
    TestCasesPositive = [
        [
            [step(0, 1), left(135), step(1, 0), left(135), step(0, 1), left(90)],
            [step(1, 0), left(135), step(0, 1), left(90), step(0, 1), left(135)]
        ]
    ],
    testPositive(TestCasesPositive, ResultPositive),

    TestCasesNegative = [
        [
            [step(0, 1), left(135), step(1, 0), left(135), step(0, 1), left(90)],
            [step(1, 0), left(135), step(0, 1), left(135), step(0, 1), left(90)]
        ]
    ],
    testNegative(TestCasesNegative, ResultNegative),

    (
        if ResultNegative = [], ResultNegative = []
        then
            io.write_string("ok\n", !IO)
        else
            io.write_string("fail positive\n", !IO),
            io.write_line(ResultPositive, !IO),
            io.write_string("fail negative\n", !IO),
            io.write_line(ResultNegative, !IO)
    ).

:- pred testPositive(list(list(figure))::in, list(list(figure))::out) is det.
testPositive(TestCases, Failed) :-
    Failed = negated_filter(
        (pred(Figures::in) is semidet :-
            Figures = [A, B],
            figure_equals(A, B)
        ),
        TestCases
    ).

:- pred testNegative(list(list(figure))::in, list(list(figure))::out) is det.
testNegative(TestCases, Failed) :-
    Failed = negated_filter(
        (pred(Figures::in) is semidet :-
            Figures = [A, B],
            not figure_equals(A, B)
        ),
        TestCases
    ).

%---------------------------------------------------------------------------%