%---------------------------------------------------------------------------%
:- module classic.

:- interface.

:- import_module list.

:- import_module types.

:- pred little_square(figure::out) is det.
:- pred little_triangle(figure::out) is det.
:- pred medium_triangle(figure::out) is det.
:- pred big_triangle(figure::out) is det.
:- pred parallelogram(figure::out) is det.

:- pred get_tangram_components(list(figure)::out) is det.

%---------------------------------------------------------------------------%
:- implementation.

little_square([step(0, 1), left(90), step(0, 1), left(90), step(0, 1), left(90), step(0, 1), left(90)]).

little_triangle([step(0, 1), left(135), step(1, 0), left(135), step(0, 1), left(90)]).

medium_triangle([step(1, 0), left(135), step(0, 2), left(135), step(1, 0), left(90)]).

big_triangle([step(0, 2), left(135), step(2, 0), left(135), step(0, 2), left(90)]).

parallelogram([step(1, 0), left(135), step(0, 1), left(45), step(1, 0), left(135), step(0, 1), left(45)]).

get_tangram_components(Out) :-
    little_square(LSq),
    little_triangle(LTr),
    medium_triangle(MTr),
    big_triangle(BTr),
    parallelogram(Pg),
    Out = [LSq, LTr, LTr, MTr, BTr, BTr, Pg].

%---------------------------------------------------------------------------%
