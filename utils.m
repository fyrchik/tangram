%---------------------------------------------------------------------------%
:- module utils.

:- interface.

:- import_module io, list, string.
:- import_module types.

% append_turns returns a turn which needs to be performed if
%   2 figures are joined to each other side-by-side and
%   corners a matched.
:- pred append_turns(elem::in, elem::in, elem::out) is semidet.

% invert_turn performs turn in the same direction but
%   as if we were going backwards. 
:- func invert_turn(elem) = elem is semidet.

% has_nil checks if traversal has nil elements.
:- pred has_nil(figure::in) is semidet.

% remove_nil remove all nil elements from the traversal.
:- func remove_nil(figure) = figure.

% write_traversal prints traversal in the readable format.
:- pred write_traversal(list(figure)::in, io::di, io::uo) is det.

% read_traversal reads traversal from the string.
:- pred read_traversal_from_string(string::in, read_result(figure)::out) is det.

% collapse_elems collapses successive multiple turns or steps into a single turn or step.
:- func collapse_elems(figure) = figure.

% reflect reflects figure which must be normalized.
:- func reflect(figure) = figure.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module int.
:- import_module exception.
:- import_module pprint.

append_turns(turn(Deg1), turn(Deg2), normalize_turn(turn((Deg1 + Deg2 + 180) mod 360))).
invert_turn(turn(D)) = normalize_turn(turn(-D)).

has_nil(A) :- any_true(is_nil, A).

remove_nil(A) = negated_filter(is_nil, A).

collapse_elems([]) = [].
collapse_elems([_] @ List) = List.
collapse_elems([E1 | [E2 | Es] @ Tail]) = Result :-
  (
    add_turns(E1, E2, E) -> Result = collapse_elems([E | Es])
  ; add_steps(E1, E2, E) -> Result = collapse_elems([E | Es])
  ; Result = [E1 | collapse_elems(Tail)]
  ).

reflect([]) = [].
reflect([step(_,_) @ S | Es]) = [S | reverse(Es)].
reflect([turn(_) | _ ]) = throw("non-normalized figure in reflect").

write_traversal(List, !IO) :-
  pprint.write(80, to_doc(List), !IO).


read_traversal_from_string(String, Result) :-
  Pos0 = io.posn(1, 0, 0),
  io.read_from_string("", String, length(String), Result, Pos0, _).

%---------------------------------------------------------------------------%
