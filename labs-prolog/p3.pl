:-use_module(library(lists)).

%2

% list_size(+List, ?Size)
list_size([], 0).
list_size([_|T], Size) :-
    list_size(T, A),
    Size is A + 1.

%  list_sum(+List, ?Sum)
list_sum([], 0).
list_sum([H | T], Sum) :-
    list_sum(T, A),
    Sum is H + A.

% list_prod(+List, ?Prod)
list_prod([], 1).
list_prod([H|T], Prod) :-
    list_prod(T, A),
    Prod is A * H.

% inner_product (+List1, +List2, ?Result)
inner_product([], [], 0).
inner_product([H1 | T1], [H2 | T2], Result) :-
    inner_product(T1, T2, A),
    Result is H1 * H2 + A.

% count(+Elem, +List, ?N)
count(_, [], 0).
count(Elem, [Elem | T], N) :-
    count(Elem, T, A),
    N is A + 1.
count(Elem, [H | T], N) :-
    H \= Elem,
    count(Elem, T, A),
    N is A.


%3 

% invert(+List1, ?List2)
invert(List1, List2) :-
    invert_aux(List1, [], List2).

invert_aux([], Acc, Acc).
invert_aux([H| T], Acc, List2):-
    Acc1 = [H | Acc],
    invert_aux(T, Acc1, List2).

% del_one(+Elem, +List1, ?List2)
del_one(_, [], []).
del_one(Elem, [Elem|T], T).
del_one(Elem, [H|T], [H|T2]) :-
    Elem \= H,
    del_one(Elem, T, T2).

% del_all(+Elem, +List1, ?List2)
del_all(_, [], []).
del_all(Elem, [Elem|T], List2) :-
    del_all(Elem, T, List2).
del_all(Elem, [H|T], [H|T2]) :-
    Elem \= H,
    del_all(Elem, T, T2).

% del_all_list(+ListElems, +List1, ?List2)
del_all_list([], A, A).
del_all_list([H|T], List1, List2) :-
    del_all(H, List1, A),
    del_all_list(T, A, List2).


% 4

% list_append(?L1, ?L2, ?L3)
list_append([], R, R).
list_append([H|T], L2, [H|R]) :-
    list_append(T, L2, R).

% list_member(?Elem, ?List)
list_member(Elem, List) :-
    append(_, [Elem | _], List).

% list_last(+List, ?Last)
list_last(List, Last) :-
    append(_, [Last | []], List).

% list_nth(?N, ?List, ?Elem)
list_nth(N, List, Elem) :-
    length(List, Len),
    append(_, [Elem | T], List),
    A is Len - N,
    length(T, A).

%list_append(+ListOfLists, ?List)
list_append(ListOfLists, List) :-
    list_append_aux(ListOfLists, [], List).
    
list_append_aux([], Acc, Acc).
list_append_aux([H | T], Acc, List) :-
    list_append(Acc, H, Acc1),
    list_append_aux(T, Acc1, List).

% list_del(+List, +Elem, ?Res)
list_del(List, Elem, Res) :-
    append(Left, [Elem | Right], List),
    append(Left, Right, Res).
    
% 5

% list_to(+N, ?List)
list_to(N, List) :-
    list_to_aux(N, [], List), !.

list_to_aux(0, Acc, Acc).
list_to_aux(N, Acc, List) :-
    Acc1 = [N | Acc],
    A is N-1,
    list_to_aux(A, Acc1, List).

% list_from_to(+Inf, +Sup, ?List)
list_from_to(Inf, Sup, List) :-
    K is Inf -1,
    list_from_to_aux(K, Sup, [], List), !.

list_from_to_aux(Inf, Inf, Acc, Acc).
list_from_to_aux(Inf, Sup, Acc, List):-
    Acc1 = [Sup | Acc],
    A is Sup-1,
    list_from_to_aux(Inf, A, Acc1, List).


% 7

%is_ordered(+List)
is_ordered([H | T]) :-
    is_ordered_aux(H, T).

is_ordered_aux(_, []).
is_ordered_aux(H, [A | B]) :-
    H =< A,
    is_ordered_aux(A, B).


% insert_ordered(+Value, +List1, ?List2)
insert_ordered(Value, [], [Value]).
insert_ordered(Value, [H | T], [Value, H | T]) :-
    Value =< H.

insert_ordered(Value, [H|T], [H|R]) :-
    Value > H,
    insert_ordered(Value, T, R).
