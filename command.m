%---------------------------------------------------------------------------%
:- module command.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module int, list.
:- import_module solutions.

:- import_module types.
:- import_module tangram.
:- import_module utils.

main(!IO) :-
    % % 1 little square
    % Little_square = [step(0,1), turn(left,90), step(0, 1), turn(left,90), step(0,1), turn(left, 90), step(0, 1)],
    % % 2 little triangles |_
    % Little_triangle = [step(0,1), turn(left,360-45), step(1,0), turn(left,360-45), step(0,1)],
    % % 1 medium triangle |_
    % Medium_triangle = [step(1,0), turn(left,360-45), step(0,2), turn(left,360-45), step(1,0)],
    % % 2 big triangles |_
    % Big_triangle = [step(0,2), turn(left,360-45), step(2,0), turn(left,360-45), step(0,2)],
    % % 1 parallelogram
    % Parallelogram = [step(1,0), turn(left,45), step(0,1), turn(left,45+90), step(1,0), turn(left,45), step(0,1)],

    % 2 equal triangles
    % Fig1 = [step(0,1), turn(360-45), step(1,0), turn(360-45), step(0,1)],
    % Fig2 = [step(0,1), turn(360-45), step(1,0), turn(360-45), step(0,1)],
    % % Result is a square with 2 triangles combined via their hypotenuse.
    % Result = [step(0,1), turn(90), step(0,1), turn(90), step(0,1), turn(90), step(0,1)],
    % io.format("%s + %s = CAT\n", [First, Second], !IO),
    X = [step(1,0), left(90), step(1,0), left(90), step(1,0), left(90), step(1,0), left(90)],
    Y = [step(1,0), left(90), step(1,0), left(90), step(1,0), left(90), step(1,0), left(90)],
    Z = [step(0,2), left(90+45), step(1,0), left(90), step(1,0), left(90+45)],
    solutions(combine_list([X,Y,Z]), Out),
    read_traversal_from_string("[step(1,0), turn(90), step(1,0), turn(90), step(1,0), turn(90), step(1,0), turn(90)].", Result),
    io.write(Result, !IO),
    io.nl(!IO),
    io.format("Result\n", [], !IO),
    write_traversal(Out, !IO)
    .

%---------------------------------------------------------------------------%