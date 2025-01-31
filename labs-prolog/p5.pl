%1
:-use_module(library(lists)).

%children(+Person, -Children)
children(Person, Children) :-
    findall(X, (parent(Person, X)), Children).


% children_of(+ListOfPeople, -ListOfPairs)
children_of([], []).
children_of([H | T], [H1 | T1]) :-
    children(H, Children),
    H1 = H-Children,
    children_of(T, T1).

% family(-F)
family(F) :-
    findall(X,
        (parent(X,_) ; parent(_,X)),
        L
    ),
    sort(L,F).

%couple(?C)
couple(C) :-
    parent(X, Child),
    parent(Y, Child),
    X \= Y,
    C = X-Y.


%couples(-List)
couples(List) :-
    findall(X, couple(X), L),
    sort(L, List).



% 2

% teachers(-T)
teachers(T) :-
    setof(X,
        teachers_aux(X),
        T).

teachers_aux(X) :- teaches(_, X).
% students_of(+T, -S)
students_of(T, S) :-    
    findall(X, 
        (teaches(A, T),
        attends(A, X)
        ),
        S
    ).

% teachers_of(+S, -T)
 teachers_of(S, T) :-
        findall(X, 
        (attends(A, S),
        teaches(A, X)
        ),
        T
    ).

% 3

%class(Course, ClassType, DayOfWeek, Time, Duration)

class(pfl, t, '2 Tue', 15, 2).
class(pfl, tp, '2 Tue', 10.5, 2).
class(lbaw, t, '3 Wed', 10.5, 2).
class(lbaw, tp, '3 Wed', 8.5, 2).
class(ipc, t, '4 Thu', 14.5, 1.5).
class(ipc, tp, '4 Thu', 16, 1.5).
class(fsi, t, '1 Mon', 10.5, 2).
class(fsi, tp, '5 Fri', 8.5, 2).
class(rc, t, '5 Fri', 10.5, 2).
class(rc, tp, '1 Mon', 8.5, 2).

% same_day(+Course1, +Course2)
same_day(Course1, Course2) :-
    class(Course1, T1, D, _, _),
    class(Course2, T2, D, _, _),
    T1 \= T2.

% daily_courses(+Day, -Courses)
daily_courses(Day, Courses) :-
    findall(X,
        class(X, _, Day,_,_),
        Courses).

% short_classes(-L)
short_classes(L) :-
    findall(UC-Day/Time,
        (class(UC, _, Day, Time, D),
         D < 2
        ),
        L
        ).

%course_classes(+Course, -Classes)
course_classes(Course, Classes) :-
    findall(Day/Time-Type,
        class(Course, Type, Day, Time, _),
        Classes).


% 4

%flight(origin, destination, company, code, hour, duration)
flight(porto, lisbon, tap, tp1949, 1615, 60).
flight(lisbon, madrid, tap, tp1018, 1805, 75).
flight(lisbon, paris, tap, tp440, 1810, 150).
flight(lisbon, london, tap, tp1366, 1955, 165).
flight(london, lisbon, tap, tp1361, 1630, 160).
flight(porto, madrid, iberia, ib3095, 1640, 80).
flight(madrid, porto, iberia, ib3094, 1545, 80).
flight(madrid, lisbon, iberia, ib3106, 1945, 80).
flight(madrid, paris, iberia, ib3444, 1640, 125).
flight(madrid, london, iberia, ib3166, 1550, 145).
flight(london, madrid, iberia, ib3163, 1030, 140).
flight(porto, frankfurt, lufthansa, lh1177, 1230, 165).

%get_all_nodes(-ListOfAirports)
get_all_nodes(ListOfAirports) :-
    setof(X,
    (faux(X)),ListOfAirports).
faux(X) :- flight(X, _,_,_,_,_).
faux(X) :- flight(_, X,_,_,_,_).


%5
%unifiable(+L1, +Term, -L2)
unifiable(L1, Term, L2) :-
    findall(X,
    (member(X, L1),
     \+ (\+ (X = Term))
    ),
    L2
    ).

