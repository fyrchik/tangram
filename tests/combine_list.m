:- module combine_list.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module int, list.
:- import_module pprint.

:- import_module types.
:- import_module utils.
:- import_module tangram.

main(!IO) :-
    % Result is a first element of a testcase.
    % Note: it must be in a normalized form.
    TestCases = [
        [ % combine 2 equal squares into a rectangle
            [step(2,0), left(90), step(1,0), left(90), step(2,0), left(90), step(1,0), left(90)],
            [step(1,0), left(90), step(1,0), left(90), step(1,0), left(90), step(1,0), left(90)],
            [step(1,0), left(90), step(1,0), left(90), step(1,0), left(90), step(1,0), left(90)]
        ],
        [ % combine 2 equal triangles into a parallelogram
            [step(1,0), left(135), step(0,2), left(45), step(1,0), left(135), step(0,2), left(45)],
            [step(1,0), left(135), step(0,2), left(135), step(1,0), left(90)],
            % TODO this test fails because arguments are not rotated yet
            %[step(1,0), left(135), step(0,2), left(135), step(1,0), left(90)] 
            [step(1,0), left(90), step(1,0), left(135), step(0,2), left(135)]
        ],
        [ % combine 2 equal triangles hypotenuse to catet
          % where the right angle of the second triangle is in the middle of a hypotenuse
            [step(1, 0), turn(90), step(0, 2), turn(135), step(1, 0), turn(-90), step(-1, 2), turn(135), step(1, 0), turn(90)],
            [step(1,0), left(90+45), step(0,2), left(90+45), step(1,0), left(90)],
            [step(1,0), left(90+45), step(0,2), left(90+45), step(1,0), left(90)]
        ],
        [ % combine 2 equal triangles catet to hypotenuse
          % where the right angle of the second triangle is combined with a sharp angle of a first
            [step(1, 0), turn(135), step(-1, 2), turn(-45), step(0, 2), turn(135), step(1, 0), turn(45), step(1, 0), turn(90)],
            [step(1,0), left(90+45), step(0,2), left(90+45), step(1,0), left(90)],
            [step(1,0), left(90+45), step(0,2), left(90+45), step(1,0), left(90)]
        ],
        [ % combine 2 equal triangles into one big triangle
            [step(2, 0), turn(135), step(0, 2), turn(90), step(0, 2), turn(135)],
            [step(1,0), left(90+45), step(0,2), left(90+45), step(1,0), left(90)],
            [step(1,0), left(90+45), step(0,2), left(90+45), step(1,0), left(90)]
        ],
        [ % big square without quarter + quarter = big square
            [step(2,0), left(90), step(2,0), left(90), step(2,0), left(90), step(2,0), left(90)],
            [step(2,0), left(90), step(2,0), left(90), step(1,0), left(90), step(1,0), left(-90), step(1,0), left(90), step(1,0), left(90)],
            [step(1,0), left(90), step(1,0), left(90), step(1,0), left(90), step(1,0), left(90)]
        ],
        [ % 2 complex figures which should be combined into a big rectangle
            [step(5,0), left(90), step(3,0), left(90), step(5,0), left(90), step(3,0), left(90)],
            [
                step(3,0), left(90), step(5,0), left(90), step(3,0), left(90),
                step(2,0), left(90), step(1,0), left(90), step(1,0), right(90),
                step(1,0), right(90), step(3,0), right(90), step(1,0), right(90),
                step(1,0), left(90), step(1,0), left(90), step(2,0), left(90)
            ],
            [
                step(1,0), left(90), step(3,0), left(90), step(1,0), left(90),
                step(1,0), right(90), step(1,0), left(90), step(1,0), left(90),
                step(1,0), right(90), step(1,0), left(90)
            ]
        ]
    ],
    Result = negated_filter(test_single, TestCases),
    (
        if Result = []
        then
            io.write_string("ok\n", !IO)
        else
            write_traversal3(Result, !IO)
    ).

:- pred write_traversal3(list(list(figure))::in, io::di, io::uo) is det.
write_traversal3(List, !IO) :- pprint.write(80, to_doc(List), !IO).

:- pred test_single(list(list(elem))::in) is semidet.
test_single([Result | Elems]) :- combine_list(Elems, Result).

%---------------------------------------------------------------------------%