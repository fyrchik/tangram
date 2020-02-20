%---------------------------------------------------------------------------%
:- module types.

:- interface.

:- import_module list.

% elem is discriminated union of 2 operations:
%   step(a, b) -- is a step forward on a * x + b * y, where (x,y) is some basis.
%     This is needed because x and y can be non-reductible to each other via rational number.
%     In a classic tangram we have x = 1 and y = square root of 2 (or some multiple of it, actually 0.5 * sqrt(2)).
%  turn(degree) -- is a turn left for a specified number of degrees.
:- type elem ---> step(int,int); turn(int).

:- type figure == list(elem).

% left is a turn left specified in degrees.
:- func left(int) = elem.

% right is a turn right specified in degrees.
:- func right(int) = elem.

% is_nil checks if and element of traversal is nil.
:- pred is_nil(elem::in) is semidet.

% normalize_turn returns normalized representation of a turn.
% When applied to steps, argument is returned untouched.
:- func normalize_turn(elem) = elem.

% add_steps performs 2 steps successively.
:- pred add_steps(elem::in, elem::in, elem::out) is semidet.

% sub_steps performs 1-st step forwards and 2-nd step backwards.
:- pred sub_steps(elem::in, elem::in, elem::out) is semidet.

% add_turns performs 2 turns successively.
:- pred add_turns(elem::in, elem::in, elem::out) is semidet.

% sub_turns performs 1 turn left and the next in the opposite direction.
:- pred sub_turns(elem::in, elem::in, elem::out) is semidet.

% is_positive checks if provided step is a net positive.
:- pred is_positive(elem::in) is semidet.

% (>) tests is one first step is more than the second.
:- pred (elem::in) > (elem::in) is semidet.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module int.

normalize_turn(X) = (X = turn(D) -> turn((D+180) mod 360 - 180); X).

is_positive(step(X,Y)) :-
  if X < 0
  then Y >= 0, Y * Y > 2 * X * X
  else (
    Y > 0
  ; Y =< 0, 2 * X * X > Y * Y
  ).

step(A1, A2) > step(B1, B2) :- is_positive(step(A1-B1, A2-B2)).

left(A) = turn(A mod 360).
right(A) = turn((-A) mod 360).

is_nil(step(0,0)).
is_nil(turn(D)) :- D mod 360 = 0.

add_steps(step(A1,B1),step(A2,B2), step(A1+A2,B1+B2)).
sub_steps(step(A1,B1), step(A2,B2), S) :-
  S = step(A1 - A2, B1 - B2),
  (is_nil(S) ; is_positive(S)).

add_turns(turn(Deg1),turn(Deg2), turn((Deg1 + Deg2) mod 360)).
sub_turns(turn(Deg1), turn(Deg2), turn((Deg1 - Deg2) mod 360)).
%---------------------------------------------------------------------------%