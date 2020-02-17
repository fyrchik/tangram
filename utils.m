%---------------------------------------------------------------------------%
:- module utils.

:- interface.

:- import_module io, list.
:- import_module types.

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

% is_nil checks if and element of traversal is nil.
:- pred is_nil(elem::in) is semidet.

% has_nil checks if traversal has nil elements.
:- pred has_nil(list(elem)::in) is semidet.

% remove_nil remove all nil elements from the traversal.
:- func remove_nil(list(elem)) = list(elem).

% write_traversal prints traversal in the readable format.
:- pred write_traversal(list(list(elem))::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module int.

add_steps(step(A1,B1),step(A2,B2), step(A1+A2,B1+B2)).
sub_steps(step(A1,B1), step(A2,B2), step(A,B)) :-
  A >= 0, B >= 0, A = A1 - A2, B = B1 - B2.

add_turns(turn(Deg1),turn(Deg2), turn((Deg1 + Deg2) mod 360)).
append_turns(turn(Deg1), turn(Deg2), turn((Deg1 + Deg2 + 180) mod 360)).
sub_turns(turn(Deg1), turn(Deg2), turn((Deg1 - Deg2) mod 360)).
invert_turn(turn(D), turn((-D) mod 360)).

is_nil(step(0,0)).
is_nil(turn(D)) :- D mod 360 = 0.

has_nil(A) :- any_true(is_nil, A).

remove_nil(A) = negated_filter(is_nil, A).

write_traversal([], !IO) :- io.format("\n", [], !IO).
write_traversal([E | Es], !IO) :-
  io.write(E, !IO),
  io.format("\n", [], !IO),
  write_traversal(Es, !IO).

%---------------------------------------------------------------------------%
