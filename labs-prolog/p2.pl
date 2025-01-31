% 4

% factorial(+N, -F)
factorial(1, 1).
factorial(N, F) :-
    N > 0,
    A is N-1,
    factorial(A, B),
    F is N * B.

% sum_rec(+N, -Sum) 
sum_rec(0, 0).
sum_rec(N, Sum) :-
    N > 0,
    A is N-1,
    sum_rec(A, B),
    Sum is N + B.

% pow_rec(+X, +Y, -P)
pow_rec(_, 0, 1).
pow_rec(X, Y, P) :-
    Y > 0,
    A is Y-1,
    pow_rec(X, A, B),
    P is X * B.

% square_aux(+A, +B, -S)
square_aux(A, 1, A).
square_aux(A, B, S) :-
    B > 0,
    Aux is B - 1,
    square_aux(A, Aux, Aux2),
    S is A + Aux2.


% square_rec(+N, -S)
square_rec(N, S) :-
    square_aux(N, N, S).

% fibonacci(+N, -F)
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, F) :-
    N > 1,
    A is N-1,
    B is N-2,
    fibonacci(A, X),
    fibonacci(B, Y),
    F is X + Y.

% collatz(+N, -S)
collatz(1, 0).
collatz(N, S) :-
    0 is N mod 2,
    A is N // 2,
    collatz(A, K),
    S is K + 1.
collatz(N, S) :-
    A is 3 * N + 1,
    collatz(A, K),
    S is K + 1.

% 5

tail_factorial(N, Result) :-
    factorial(N, 1, Result).

factorial(0, Acc, Acc).
factorial(N, Acc, F) :-
    N > 0,
    B is N-1,
    Acc1 is Acc * N,
    factorial(B, Acc1, F).

tail_sum_rec(N, S) :-
    sum_rec(N, 0, S).

sum_rec(0, Acc, Acc).
sum_rec(N, Acc, Sum) :-
    N > 0,
    A is N-1,
    Acc1 is Acc + N,
    sum_rec(A, Acc1, Sum). 


tail_fibonacci(N, F) :-
    fibonacci(N, 0, 1 ,F).

fibonacci(0, F, _, F).
fibonacci(N, Acc1, Acc2, F) :-
    N > 0,
    A is N - 1,
    NAcc1 is Acc2,
    NAcc2 is Acc2 + Acc1,
    fibonacci(A, NAcc1, NAcc2, F).


% 6

% gcd(+X, +Y, -G)
gcd(X, 0, X).
gcd(X, Y, G) :-
    Y > 0,
    A is X mod Y,
    gcd(Y,A, G).

%  lcm(+X, +Y, -M)
 lcm(X, Y, M) :-
    A is X * Y,
    gcd(X,Y,G),
    M is A // G.


% 7

% ancestor_of(?X, ?Y)
ancestor_of(X, Y) :- parent(X, Y).
ancestor_of(X, Y) :-
    parent(X, A),
    ancestor_of(A, Y).

% descendant_of (?X, ?Y)
descendant_of(X, Y) :- parent(Y, X).
descendant_of(X, Y) :-
    parent(A, X),
    descendant_of(A, Y).

% marriage_years(?X, ?Y, -Years)
marriage_years(X, Y, Years) :-
    divorce(X, Y, A),
    marriage(X, Y, B),
    Years is A - B.

 


born(mitchell, 1973-7-10).
born(jay, 1946-5-23).
born(claire, 1969-11-13).


% before(+X, +Y)
before(X, Y) :-
    X = A-B-C,
    Y = D-E-F,
    D > A.

before(X, Y) :-
    X = A-B-C,
    Y = D-E-F,
    D = A,
    E > B.

before(X, Y) :-
    X = A-B-C,
    Y = D-E-F,
    D = A,
    E = B,
    F > C.

% older(?X, ?Y, ?Older)
older(X, Y, Older) :-
    X \= Y,
    born(Name, Date),
    before(Date, Y),
    before(X, Date),
    \+ ( born(N, D), before(D, Date), before(D, Y), before(X, D)),
    Older = Name.

 oldest(X) :-
    born(Name, Date),
    \+ (born(N, D), before(D, Date)),
    X = Name.

%8

% most_gates(?X)
most_gates(X) :-
    gates(Name, Num),
    \+ (gates(A, N), N > Num),
    X = Name.

% least_gates(?X)
least_gates(X) :-
    gates(Name, Num),
    \+ (gates(A, N), N < Num),
    X = Name.

% gate_diff(?X)
gate_diff(X) :-
    most_gates(A),
    least_gates(B),
    gates(A, N1),
    gates(B, N2),
    X is N1-N2.

% same_team(?X, ?Y)
same_team(X, Y) :-
    team(X, Team),
    team(Y, Team),
    X \= Y.

% is_from_winning_team(?P, ?C)
is_from_winning_team(P, C) :-
    winnerTeam(T, C),
    team(P, T).

% 9

% superior(+X, +Y)
superior(X, Y) :- supervised_by(Y, X).
superior(X, Y) :-
    supervised_by(Y, A),
    superior(X, A).
