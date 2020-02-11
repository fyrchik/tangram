%---------------------------------------------------------------------------%
:- module tangram.

:- interface.

:- import_module io.
:- import_module list.

:- pred main(io::di, io::uo) is cc_multi.

% elem is discriminated union of 2 operations:
%   step(a, b) -- is a step forward on a * x + b * y, where (x,y) is some basis.
%     This is needed because x and y can be non-reductible to each other via rational number.
%     In a classic tangram we have x = 1 and y = square root of 2 (or some multiple of it, actually 0.5 * sqrt(2)).
%  turn(degree) -- is a turn left for a specified number of degrees.
:- type elem ---> step(int,int); turn(int).

:- pred insert_after(list(elem)::in, list(elem)::in, list(elem)::in, list(elem)::out) is semidet.
:- pred insert_before(list(elem)::in, list(elem)::in, list(elem)::in, list(elem)::out) is semidet.

% left is a turn left specified in degrees.
:- func left(int) = elem.

% right is a turn right specified in degrees.
:- func right(int) = elem.

% add_steps performs 2 steps successively.
:- pred add_steps(elem::in, elem::in, elem::out) is semidet.

% sub_steps performs 1-st step forwards and 2-nd step backwards.
:- pred sub_steps(elem::in, elem::in, elem::out) is semidet.

% add_turns performs 2 turns successively.
:- pred add_turns(elem::in, elem::in, elem::out) is semidet.

% sub_turns performs 1 turn left and the next in the opposite direction.
:- pred sub_turns(elem::in, elem::in, elem::out) is semidet.

% append_turns returns a turn which needs to be performed if
%   2 figures are joined to each other side-by-side and
%   corners a matched.
:- pred append_turns(elem::in, elem::in, elem::out) is semidet.

% invert_turn performs turn in the same direction but
%   as if we were going backwards. 
:- pred invert_turn(elem::in, elem::out) is semidet.

% normalize returns normalized Figure representation.
:- func normalize(list(elem)) = list(elem).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, list, string.

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
    ( 
      if
        X = [left(90), step(1,0), left(90)],
        Y = [step(1,0), left(90), step(1,0), left(90), step(1,0)],
        Z = [left(90+45), step(0,4), left(90+45), step(2,0), left(90), step(2,0)],
        insert_after(X, Y, Z, Result1),
        insert_before(X, Y, Z, Result2)
      then
        io.write(X, !IO),
        io.format("\n", [], !IO),
        io.write(Y, !IO),
        io.format("\n", [], !IO),
        io.write(Z, !IO),
        io.format("\nResult:\n", [], !IO),
        io.write(Result1, !IO),
        io.format("\n", [], !IO),
        io.write(Result2, !IO),
        io.format("\n", [], !IO)
      else
        io.write("no solutions", !IO)
    ).

% elems are combined when we put them next to each other.
% This means:
%   1. Find a side (or `step`) in each of them.
%   2. Collapse 2 lists into one.
%   3. Possibly normalize the resulting list:
%      - turn(0) and step(0,0) can be removed
%      - 2 sucessive steps can be combined
% :- pred combine_at(list(elem)::in, int::in, list(elem)::in, int::in, list(elem)::out) is nondet.
% combine_at([E|Es],I1,F2,I2,F) = X :-
%   if combine_at(Es,)

% insert_after inserts third argument between first and second.
% first argument must end in turn
% second argument must start from step
% third argument must start in turn
insert_after(Es1, [S2,T2|Es2], [E3|Es3], X) :-
  (
    if split_last(Es1, M1, L1),
       append_turns(L1, E3, T),
       split_last(Es3, M3, L3),
       sub_steps(L3, S2, S),
       invert_turn(T2, T2I)
    then
      append(M1, [T], X1),
      append(X1, M3, X2),
      append(X2, [S], X3),
      append(X3, [T2I], X4),
      append(X4, Es2, Y),
      X = normalize(Y)
    else fail
  ).


insert_before(Es1, [S2,T2|Es2], Es3, X) :-
  (
    if split_last(Es3, Mid, S3),
       split_last(Mid, M3, T3),
       append_turns(T3, T2, LastT),
       split_last(Es1, M1, T1),
       invert_turn(T1, FirstT),
       sub_steps(S3, S2, FirstS)
    then
       append(M1, [FirstT, FirstS], X1),
       append(X1, M3, X2),
       append(X2, [LastT|Es2], Y),
       X = normalize(Y)
    else fail
  ).

left(A) = turn(A rem 360).
right(A) = turn((-A) rem 360).

add_steps(step(A1,B1),step(A2,B2), step(A1+A2,B1+B2)).
sub_steps(step(A1,B1), step(A2,B2), step(A,B)) :-
  A >= 0, B >= 0, A = A1 - A2, B = B1 - B2.

add_turns(turn(Deg1),turn(Deg2), turn((Deg1 + Deg2) rem 360)).
append_turns(turn(Deg1), turn(Deg2), turn((Deg1+Deg2+180) rem 360)).
sub_turns(turn(Deg1), turn(Deg2), turn((Deg1-Deg2) rem 360)).
invert_turn(turn(D), turn((-D) rem 360)).

:- pred is_nil(elem::in) is semidet.
is_nil(step(0,0)).
is_nil(turn(0)).

normalize([E1, E2 | Es]) = X :-
  (
    add_turns(E1, E2, E) -> X = normalize([E | Es])
  ; add_steps(E1, E2, E) -> X = normalize([E | Es])
  ; is_nil(E2) -> X = normalize([E1 | Es])
  ; X = [E1 | normalize([E2 | Es])]
  ).
normalize([E]) = [E].
normalize([]) = [].