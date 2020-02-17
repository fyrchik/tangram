%---------------------------------------------------------------------------%
:- module tangram.

:- interface.

:- import_module io.
:- import_module list.

:- import_module types.

:- pred main(io::di, io::uo) is cc_multi.

% combine succeeds on all possible combinations of first 2 arguments.
:- pred combine(list(elem)::in, list(elem)::in, list(elem)::out) is nondet.

% combine_list succeeds on all possible combinations of arguments from list
% where every list element is used.
:- pred combine_list(list(list(elem))::in, list(elem)::out) is nondet.

:- pred insert_after(list(elem)::in, list(elem)::in, list(elem)::in, list(elem)::out) is semidet.
:- pred insert_before(list(elem)::in, list(elem)::in, list(elem)::in, list(elem)::out) is semidet.

% normalize returns normalized Figure representation.
:- func normalize(list(elem)) = list(elem).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, list, string.
:- import_module utils.
:- import_module solutions.

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
        X = [step(1,0), left(90), step(1,0), left(90), step(1,0), left(90), step(1,0), left(90)],
        Y = [step(1,0), left(90), step(1,0), left(90), step(1,0), left(90), step(1,0), left(90)],
        Z = [step(0,2), left(90+45), step(1,0), left(90), step(1,0), left(90+45)],
        solutions(combine_list([X,Y,Z]), Out)
      then
        read_traversal_from_string("[step(1,0), turn(90), step(1,0), turn(90), step(1,0), turn(90), step(1,0), turn(90)].", Result),
        io.write(Result, !IO),
        io.nl(!IO),
        io.format("Result\n", [], !IO),
        write_traversal(Out, !IO)
      else
        io.write("no solutions", !IO)
    ).

combine_list([], []).
combine_list([H], H).
combine_list([F1 | T], Result) :-
  (
    [F2 | TT] = T,
    combine(F1, F2, F),
    combine_list([F | TT], Result)
  ; combine_list(T, R),
    combine(F1, R, Result)
  ).

% elems are combined when we put them next to each other.
% This means:
%   1. Find a side (or `step`) in each of them.
%   2. Collapse 2 lists into one.
%   3. Normalize the resulting list
combine(A, B, Result) :-
  combine_aux([], A, B, R)
, R2 = collapse_bound_steps(R)
, Result = max_to_head(R2).

:- func max_to_head(list(elem)) = list(elem).
max_to_head(List) = Result :- 
  Ind = find_max_step_index(List),
  (
    if Ind > 0
    then split_upto(Ind, List, Start, End)
       , Result = append(End, Start)
    else Result = List
  ).

:- pred combine_aux(list(elem)::in, list(elem)::in, list(elem)::in, list(elem)::out) is nondet.
combine_aux(_, [], _, _) :- fail.
combine_aux(First, Second, Middle, Result) :-
    insert_after(First, Second, Middle, Result)
  ; insert_before(First, Second, Middle, Result)
  ; (
    [H | T] = Second,
    append(First, [H], Next),
    combine_aux(Next, T, Middle, W),
    Result = normalize(W)
  ).

% insert_after inserts third argument between first and second.
% first argument must end in turn
% second argument must start from step
% third argument must start in step
insert_after(Es1, [S2,T2|Es2], Es3, X) :-
  (
    if split_last(Es1, M1, L1),
       split_last(Es3, Mid3, E3),
       append_turns(L1, E3, T),
       split_last(Mid3, M3, L3),
       sub_steps(L3, S2, S),
       invert_turn(T2, T2I)
    then
      append(M1, [T | M3], X1),
      append(X1, [S, T2I | Es2], Y),
      X = normalize(Y)
    else fail
  ).


insert_before(Es1, [S2,T2|Es2], [S3|Es3], X) :-
  (
    if split_last(Es3, M3, T3),
       append_turns(T3, T2, LastT),
       split_last(Es1, M1, T1),
       invert_turn(T1, FirstT),
       sub_steps(S3, S2, FirstS)
    then
       append(M1, [FirstT, FirstS | M3], X1),
       append(X1, [LastT | Es2], Y),
       X = normalize(Y)
    else fail
  ).

normalize(A) = Result :-
  (
    % remove all nil elements
    B = remove_nil(A),
    % collapse sequences of turns/steps into one turn/step
    C = collapse_elems(B),
    % remove nils again. Note: only nil turns will be removed
    % because no nil steps could appear during previous collapse.
    D = remove_nil(C),
    % only steps will be collapsed here
    F = collapse_elems(D),
    Result = map(normalize_turn, F)
  ).

:- func collapse_bound_steps(list(elem)) = list(elem).
collapse_bound_steps(Figure) = Result :-
  if [First, T | X] = Figure
   , split_last(X, M, Last)
   , First = step(_,_)
   , Last = step(_,_)
   , add_steps(First, Last, S)
  then Result = append(M, [S, T])
  else Result = Figure.

:- func move_step_to_end(list(elem)) = list(elem).
move_step_to_end([]) = [].
move_step_to_end([E | Es]) = Result :-
  if E = turn(_)
  then Result = [E | Es]
  else append(Es, [E], X), Result = X.

:- func normalize_turn(elem) = elem.
normalize_turn(X) = (X = turn(D) -> turn((D+180) mod 360 - 180); X).

:- func collapse_elems(list(elem)) = list(elem).
collapse_elems([]) = [].
collapse_elems([E]) = [E].
collapse_elems([E1, E2 | Es]) = Result :-
  (
    add_turns(E1, E2, E) -> Result = collapse_elems([E | Es])
  ; add_steps(E1, E2, E) -> Result = collapse_elems([E | Es])
  ; Result = [E1 | collapse_elems([E2 | Es])]
  ).

:- func find_max_step_index(list(elem)) = int.
find_max_step_index(List) = Result :-
  Compare =
    (pred(Elem::in, IndCur::in, Next::out, Ind1::in, IndMax::out, Elem1::in, ElemMax::out) is det :-
      Next = IndCur + 1,
      (
        if Elem = step(A,B)
         , Elem1 = step(AMax,BMax)
         , (A > AMax ; A = AMax, B > BMax)
        then IndMax = IndCur, ElemMax = Elem
        else IndMax = Ind1, ElemMax = Elem1
      )
    ),
  foldl3(Compare, List, 0, _, -1, Ind, step(0, 0), _),
  Result = Ind.