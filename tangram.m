%---------------------------------------------------------------------------%
:- module tangram.

:- interface.

:- import_module list.

:- import_module types.

% combine succeeds on all possible combinations of first 2 arguments.
:- pred combine(figure::in, figure::in, figure::out) is nondet.

% combine_list succeeds on all possible combinations of arguments from list
% where every list element is used.
:- pred combine_list(list(figure)::in, figure::out) is nondet.

:- pred insert_after(figure::in, figure::in, figure::in, figure::out) is nondet.
:- pred insert_before(figure::in, figure::in, figure::in, figure::out) is nondet.

% normalize returns normalized Figure representation.
:- func normalize(figure) = figure.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, list, string.
:- import_module utils.

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

:- func max_to_head(figure) = figure.
max_to_head(List) = Result :- 
  Ind = find_max_step_index(List),
  (
    if Ind > 0
    then split_upto(Ind, List, Start, End)
       , Result = append(End, Start)
    else Result = List
  ).

:- pred combine_aux(figure::in, figure::in, figure::in, figure::out) is nondet.
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
insert_after(
    Es1,
    [step(_,_) @ S2, turn(_) @ T2 | Es2],
    [step(_,_) @ S3, turn(_) @ T31|Es3],
    X
  ) :-
  (
    if split_last(Es1, M1, T1),
       split_last(Es3, M3, T3Last),
       append_turns(T1, T31, FirstT)
    then (
      S2 = S3,
      append_turns(T3Last, T2, LastT),
      condense([M1, [FirstT], M3, [LastT], Es2], Y)
    ;
      S2 > S3,
      LastT = reverse_turn(T3Last),
      condense([M1, [FirstT], M3, [LastT, S2 - S3, T2], Es2], Y)
    ;
      S3 > S2,
      LastT = reverse_turn(T2),
      condense([M1, [FirstT], Es3, [S3 - S2, LastT], Es2], Y)
    ),
    X = normalize(Y)
    else fail
  ).


insert_before(
    Es1,
    [step(_,_) @ S2, turn(_) @ T2 | Es2],
    [step(_,_) @ S3, turn(_) @ T31 | Es3],
    X
  ) :-
  (
    if split_last(Es1, M1, T1),
       split_last(Es3, M3, T3Last),
       append_turns(T3Last, T2, LastT)
    then (
      S2 = S3,
      append_turns(T1, T31, FirstT),
      condense([M1, [FirstT], M3, [LastT], Es2], Y)
    ;
      S2 > S3,
      sub_steps(S2, S3, FirstS),
      FirstT = reverse_turn(T31),
      condense([Es1, [FirstS, FirstT], M3, [LastT], Es2], Y)
    ;
      S3 > S2,
      FirstT = reverse_turn(T1),
      sub_steps(S3, S2, FirstS),
      condense([M1, [FirstT, FirstS, T31], M3, [LastT], Es2], Y)
    ),
    X = normalize(Y)
    else fail
  ).

:- func reverse_turn(elem) = elem is semidet.
reverse_turn(turn(D)) = normalize_turn(turn(D-180)).

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

:- func collapse_bound_steps(figure) = figure.
collapse_bound_steps(Figure) = Result :-
  if [step(_,_) @ First, T | X] = Figure
   , split_last(X, M, step(_,_) @ Last)
  then Result = append(M, [First + Last, T])
  else Result = Figure.

:- func move_step_to_end(figure) = figure.
move_step_to_end([]) = [].
move_step_to_end([E | Es] @ List) = Result :-
  if E = turn(_)
  then Result = List
  else append(Es, [E], X), Result = X.

:- func find_max_step_index(figure) = int.
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
