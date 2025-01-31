%1

%add_person(+Gender, +Name)
 add_person(male, Name) :-
    \+ male(Name),
    asserta(male(Name)).

 add_person(female, Name) :-
    \+ female(Name),
    asserta(female(Name)).

% add_parents(+Person, +Parent1, +Parent2)
add_parents(Person, Parent1, Parent2) :-
    \+ parent(Parent1,Person),
    \+ parent(Parent2,Person),
    asserta(parent(Parent1,Person)),
    asserta(parent(Parent2,Person)), !.

add_parents(Person, Parent1, Parent2) :-
    \+ parent(Parent1,Person),
    parent(Parent2,Person),
    asserta(parent(Parent1,Person)), !.

add_parents(Person, Parent1, Parent2) :-
    \+parent(Parent2,Person),
    parent(Parent1,Person),
    asserta(parent(Parent1,Person)), !.

add_parents(Person, Parent1, Parent2) :-
    parent(Parent1,Person),
    parent(Parent2,Person), !.


% 2
double(X, Y):- Y is X*2.

%map(+Pred, +List1, ?List2)
map(_, [], []) :- !.
map(Pred, [H|T], [H2|T2]) :-
    F =.. [Pred, H, H2],
    F,
    map(Pred, T, T2).

% 3
