%---------------------------------------------------------------------------%
:- module tangram.

:- interface.

:- import_module list.

:- import_module types.

% combine succeeds on all possible combinations of first 2 arguments.
:- pred combine(figure, figure, figure).
:- mode combine(in, in, out) is nondet.
:- mode combine(in, in, in) is semidet.

% combine_list succeeds on all possible combinations of arguments from list
% where every list element is used.
:- pred combine_list(list(figure), figure).
:- mode combine_list(in, out) is nondet.
:- mode combine_list(in, in) is semidet.

% combine_list_with_trace is combine list but also returns trace as it's last arguments.
% Trace is a sequence of intermediate results by means of which it easy to recovery
% how figures were combined.
:- pred combine_list_with_trace(list(figure), figure, list(figure)).
:- mode combine_list_with_trace(in, out, out) is nondet.
:- mode combine_list_with_trace(in, in, out) is nondet.

:- pred insert_after(figure, figure, figure, figure).
:- mode insert_after(in, in, in, out) is nondet.
:- mode insert_after(in, in, in, in) is semidet.

:- pred insert_before(figure, figure, figure, figure).
:- mode insert_before(in, in, in, out) is nondet.
:- mode insert_before(in, in, in, in) is semidet.

% normalize returns normalized Figure representation.
:- func normalize(figure) = figure.

% remove180 transforms a figure into an equivalent but
% without backturns except possibly as a first element.
:- func remove180(figure) = figure.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module int, list, string.
:- import_module utils.

combine_list_with_trace([], [], []).
combine_list_with_trace([H], H, []).
combine_list_with_trace([Fig1 | Rest], Result, Trace) :-
  (
    [Fig2 | Rest2] = Rest,
    combine(Fig1, Fig2, F),
    combine_list_with_trace([F | Rest2], Result, T),
    Trace = [F | T]
  ; combine_list_with_trace(Rest, R, T),
    combine(Fig1, R, Result),
    Trace = append(T, [Result])
  ).

combine_list(Figures, Result) :- combine_list_with_trace(Figures, Result, _).

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
( combine_aux([], A, B, R)
; combine_aux([], A, reflect(B), R)
)
, R2 = collapse_bound_steps(R)
, R3 = max_to_head(R2)
, G = remove180(R3)
, Result = collapse_elems(G).


:- func max_to_head(figure) = figure.
max_to_head(List) = Result :- 
  Ind = find_max_step_index(List),
  ( Ind =< 0 ->
      Result = List
    ; split_upto(Ind, List, Start, End)
    , Result = append(End, Start)
  ).

:- pred combine_aux(figure::in, figure::in, figure::in, figure::out) is nondet.
combine_aux(First, Second, Middle, Result) :-
    insert_after(First, Second, Middle, Result)
  ; insert_before(First, Second, Middle, Result)
  ; (
    [H | T] = Second,
    Next = append(First, [H]),
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
    split_last(Es1, M1, T1),
    split_last(Es3, M3, T3Last),
    append_turns(T1, T31, FirstT),
    (
      S2 = S3,
      append_turns(T3Last, T2, LastT),
      Y = condense([M1, [FirstT], M3, [LastT], Es2])
    ;
      S2 > S3,
      LastT = reverse_turn(T3Last),
      Y = condense([M1, [FirstT], M3, [LastT, S2 - S3, T2], Es2])
    ;
      S3 > S2,
      LastT = reverse_turn(T2),
      Y = condense([M1, [FirstT], Es3, [S3 - S2, LastT], Es2])
    ),
    X = normalize(Y)
  ).


insert_before(
    Es1,
    [step(_,_) @ S2, turn(_) @ T2 | Es2],
    [step(_,_) @ S3, turn(_) @ T31 | Es3],
    X
  ) :-
  ( split_last(Es1, M1, T1),
    split_last(Es3, M3, T3Last),
    append_turns(T3Last, T2, LastT),
    (
      S2 = S3,
      append_turns(T1, T31, FirstT),
      Y = condense([M1, [FirstT], M3, [LastT], Es2])
    ;
      S2 > S3,
      sub_steps(S2, S3, FirstS),
      FirstT = reverse_turn(T31),
      Y = condense([Es1, [FirstS, FirstT], M3, [LastT], Es2])
    ;
      S3 > S2,
      FirstT = reverse_turn(T1),
      sub_steps(S3, S2, FirstS),
      Y = condense([M1, [FirstT, FirstS, T31], M3, [LastT], Es2])
    ),
    X = normalize(Y)
  ).

:- func reverse_turn(elem) = elem is semidet.
reverse_turn(turn(D)) = normalize_turn(turn(D-180)).

remove180([]) = [].
remove180([_] @ List) = List.
remove180([_,_] @ List) = List.
remove180([turn(_) @ T | [_, _ | _] @ Tail]) = Result :-
  Lst = prepend(T, remove180(Tail)),
  ( Lst = [turn(0) | TailLst] ->
      Result = TailLst
    ; Result = Lst
  ).
remove180([step(_,_) @ Step1 | [Turn1 | [Step2 | Es] @ Tail]]) = Result :-
  Turn1 = turn(_), Step2 = step(_,_) ->
    (
      Turn1 = turn(180) -> (
        Step1 = Step2 ->
        Result = prepend(Turn1, remove180(Es))
      ; Step1 > Step2 ->
        Lst = remove180(Es),
        Step = Step1 - Step2,
        Result = remove180(prepend(Step, Lst))
      ; Lst = remove180(Es),
        Step = Step2 - Step1,
        Result = remove180([Turn1, Step | Lst])
      )
      ; Turn1 = turn(0) -> Result = remove180([Step1 + Step2 | Es])
      ; Lst = remove180(Tail),
        NewList = prepend(Turn1, Lst),
        T = det_head(NewList),
        NewListTail = det_tail(NewList),
        ( T = turn(180) -> Result = remove180([Step1 | NewList])
        ; T = turn(0) -> Result = prepend(Step1, NewListTail)
        ; Result = prepend(Step1, NewList)
        )
    )
  ; Lst = remove180(Tail),
    NewList = prepend(Turn1, Lst),
    ( head(NewList) = turn(180) ->
        Result = remove180([Step1 | NewList])
      ; Result = prepend(Step1, NewList)
    )
  .

:- func prepend(elem, figure) = figure.
prepend(E, []) = [E].
prepend(step(_,_) @ S, [Step | Es] @ List) =
  ( Step = step(_,_) ->
      [S + Step | Es]
    ; [S | List]
  ).
prepend(turn(_) @ T, [Turn | Es] @ List) =
  ( Turn = turn(_) ->
      [T + Turn | Es]
    ; [T | List]
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

:- func collapse_bound_steps(figure) = figure.
collapse_bound_steps(Figure) = Result :-
  if [step(_,_) @ First, T | X] = Figure
   , split_last(X, M, step(_,_) @ Last)
  then Result = append(M, [First + Last, T])
  else Result = Figure.

:- func move_step_to_end(figure) = figure.
move_step_to_end([]) = [].
move_step_to_end([turn(_) | _] @ List) = List.
move_step_to_end([step(_,_) @ S | Es]) = append(Es, [S]).

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
