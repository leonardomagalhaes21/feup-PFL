:-use_module(library(lists)).
:-use_module(library(between)).

% 4

% max(+A, +B, +C, ?Max)

max(A, B, C, Max) :-
    A >= B,
    A >= C,
    Max = A, !.

max(A, B, C, Max) :-
    B >= A,
    B >= C,
    Max = B, !.

max(A, B, C, Max) :-
    C >= A,
    C >= B,
    Max = C, !.

% 5
% print_n(+N, +S)
print_n(0, _) :- !.
print_n(N, S) :-
    A is N -1,
    write(S),
    print_n(A, S).

% print_text(+Text, +Symbol, +Padding) 
print_text(Text, Symbol, Padding) :-
    write(Symbol),
    print_spaces(Padding),
    write_text(Text),
    print_spaces(Padding),
    write(Symbol).

print_spaces(0) :- !.
print_spaces(Padding) :-
    write(' '),
    A is Padding-1,
    print_spaces(A).

write_text([]) :- !.
write_text([H|T]) :-
    put_code(H),
    write_text(T).

% print_banner(+Text, +Symbol, +Padding)
print_banner(Text, Symbol, Padding) :-
    length(Text, N),
    A is (Padding * 2) + 3 + N,
    B is Padding * 2 + 1 + N,
    print_n(A, '*'), nl,
    write('*'),
    print_n(B, ' '),
    write('*'), nl,
    print_text(Text, Symbol, Padding),nl,
    write('*'),
    print_n(B, ' '),
    write('*'), nl,
    print_n(A, '*'), nl.


% n feito este em baixo
read_number(X) :-
    read_number_aux(X, 0, false).

read_number_aux(X, Acc, _) :-
    get_code(C),
    C >= 48, C =< 57,
    Acc1 is  10 * Acc + (C-48),
    read_number_aux(X, Acc1, true).
read_number_aux(Acc, Acc, true).


%read_until_between(+Min, +Max, -Value)
read_until_between(Min, Max, Value) :-
    repeat,
    read_number(Value),
    between(Min,Max,Value),
    !.

%print_full_list(+L)
print_full_list([]) :- !.
print_full_list([H|[]]) :-
    write(H), !.
print_full_list([H|T]) :-
    write(H),
    write(', '),
    print_full_list(T).


% print_matrix(+M)
print_matrix([]) :- !.
print_matrix([H|T]) :-
    write('['), print_full_list(H), write(']'), nl,
    print_matrix(T).