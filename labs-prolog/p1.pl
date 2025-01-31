:- dynamic(male/1).
:- dynamic(female/1).
:- dynamic(parent/2).


female(grace).
female(dede).
female(gloria).
female(barb).
female(claire).
female(manny).
female(pameron).
female(haley).
female(lily).
female(calhoun).
female(poppy).

male(frank).
male(jay).
male(javier).
male(merle).
male(phil).
male(mitchell).
male(joe).
male(cameron).
male(bo).
male(dylan).
male(alex).
male(luke).
male(rexford).
male(george).


parent(grace, phil).
parent(frank, phil).
parent(dede, claire).
parent(jay, claire).
parent(dede, mitchell).
parent(jay, mitchell).
parent(jay, joe).
parent(gloria, joe).
parent(gloria, manny).
parent(javier, manny).
parent(barb, cameron).
parent(merle, cameron).
parent(barb, pameron).
parent(merle, pameron).
parent(phil, haley).
parent(claire, haley).
parent(phil, alex).
parent(claire, alex).
parent(phil, luke).
parent(claire, luke).
parent(mitchell, lily).
parent(cameron, lily).
parent(mitchell, rexford).
parent(cameron, rexford).
parent(pameron, calhoun).
parent(bo, calhoun).
parent(dylan, poppy).
parent(haley, poppy).
parent(dylan, george).
parent(haley, george).

% 1.c

father(Father, X) :-
    male(Father),
    parent(Father, X).

mother(Mother, X) :-
    female(Mother),
    parent(Mother, X).

grandparent(Grandparent, X) :-
    parent(Grandparent, Y),
    parent(Y, X).

grandmother(G, X) :-
    female(G),
    grandparent(G, X).

siblings(X, Y) :-
    mother(M, X),
    mother(M, Y),
    father(D, X),
    father(D, Y),
    X \= Y.

halfsiblings(X, Y) :-
    parent(P, X),
    parent(P, Y),
    \+ siblings(X, Y),
    X \= Y.

cousins(X, Y) :-
    parent(P, X),
    parent(P1, Y),
    siblings(P, P1),
    \+ siblings(X, Y),
    X \= Y.

uncle(X,Y) :-
    parent(P, Y),
    siblings(P, X).


marriage(jay,gloria,2008).
marriage(jay, dede, 1968).
divorce(jay, dede, 2003).


% 2

teaches(algorithms, adalberto).
teaches(databases, bernardete).
teaches(compilers, capitolino).
teaches(statistics, dalmindo).
teaches(networks, ermelinda).


attends(algorithms, alberto).
attends(algorithms, bruna).
attends(algorithms, cristina).
attends(algorithms, diogo).
attends(algorithms, eduarda).

attends(databases, antonio).
attends(databases, bruno).
attends(databases, cristina).
attends(databases, duarte).
attends(databases, eduardo).

attends(compilers, alberto).
attends(compilers, bernardo).
attends(compilers, clara).
attends(compilers, diana).
attends(compilers, eurico).

attends(statistics, antonio).
attends(statistics, bruna).
attends(statistics, claudio).
attends(statistics, duarte).
attends(statistics, eva).

attends(networks, alvaro).
attends(networks, beatriz).
attends(networks, claudio).
attends(networks, diana).
attends(networks, eduardo).


% 2 c

% 1
student(X, Y) :-
    teaches(Course, Y),
    attends(Course, X).


% 4
student(Student, X, Y) :-
    student(Student, X),
    student(Student, Y),
    X \= Y.

% 5
colleagues(X, Y) :-
    teaches(_, X),
    teaches(_, Y),
    X \= Y.

colleagues(X, Y) :-
    attends(Course, X),
    attends(Course, Y),
    X \= Y.

% 6
multipleCourses(Student) :-
    attends(Course1, Student),
    attends(Course2, Student),
    Course1 \= Course2.


% 3

pilot(lamb).
pilot(besenyei).
pilot(chambliss).
pilot(maclean).
pilot(mangold).
pilot(jones).
pilot(bonhomme).

team(lamb, breitling).
team(besenyei, redbull).
team(chambliss, redbull).
team(maclean, mediterranean).
team(mangold, cobra).
team(jones, matador).
team(bonhomme, matador).

plane(lamb, mx2).
plane(besenyei, edge540).
plane(chambliss, edge540).
plane(maclean, edge540).
plane(mangold, edge540).
plane(jones, edge540).
plane(bonhomme, edge540).

circuit(istanbul).
circuit(budapest).
circuit(porto).

winner(jones, porto).
winner(mangold, budapest).
winner(mangold, istanbul).

gates(istanbul, 9).
gates(budapest, 6).
gates(porto, 5).

winnerTeam(Team, Race) :-
    winner(Pilot, Race),
    team(Pilot, Team).


% 4

translate(1, 'Integer Overflow').
translate(2, 'Division by zero').
translate(3, 'ID Unknown').

% 5

job(technician, eleuterio).
job(technician, juvenaldo).
job(analyst, leonilde).
job(analyst, marciliano).
job(engineer, osvaldo).
job(engineer, porfirio).
job(engineer, reginaldo).
job(supervisor, sisnando).
job(chief_supervisor, gertrudes).
job(secretary, felismina).
job(director, asdrubal).

supervised_by(technician, engineer).
supervised_by(engineer, supervisor).
supervised_by(analyst, supervisor).
supervised_by(supervisor, chief_supervisor).
supervised_by(chief_supervisor, director).
supervised_by(secretary, director).

% c

direct_supervisor(X, Y) :-
    job(J1, X),
    job(J2, Y),
    supervised_by(J1, J2).

supervised_by_same_job(X,Y) :-
    job(J1, X),
    job(J2, Y),
    supervised_by(J1, J),
    supervised_by(J2, J).

supervises_more_than_one(X) :-
    job(Job, X),
    supervised_by(J1, Job),
    supervised_by(J2, Job),
    J1 \= J2.

supersupervisor(X, Y) :-
    job(J1, X),
    job(J2, Y),
    supervised_by(_J, J1),
    supervised_by(J2, _J).