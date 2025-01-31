%:- use_module(library(lists)).
:-dynamic saga/4, movie/8.

%saga(SagaID, Saga Name, Number of Movies in Saga, Creator)
saga(1, 'Jurassic Park',  6, 'Michael Crichton').
saga(2, 'Indiana Jones',  4, 'George Lucas').
saga(3, 'Star Wars',      9, 'George Lucas').
saga(4, 'Harry Potter',   0, 'J. K. Rowling').
saga(6, 'Jaws',           0, 'Peter Benchley').

%movie(Movie Title, Year of Release, SagaID, Duration, IMDB Score, Director, Composer, Cast)
movie('Jurassic Park',                  1993, 1, 127, 8.2, 'Steven Spielberg', 'John Williams',     ['Sam Neill', 'Jeff Goldblum', 'Laura Dern', 'BD Wong']).
movie('The Lost World: Jurassic Park',  1997, 1, 129, 6.5, 'Steven Spielberg', 'John Williams',     ['Jeff Goldblum', 'Julianne Moore', 'Vince Vaughn', 'Richard Schiff']).
movie('Jurassic Park III',              2001, 1,  92, 5.9, 'Joe Johnston',     'Don Davis',         ['Sam Neill', 'William H. Macy', 'Téa Leoni']).
movie('Jurassic World',                 2015, 1, 124, 6.9, 'Colin Trevorrow',  'Michael Giacchino', ['Chris Pratt', 'Bryce Dallas Howard', 'Irrfan Khan', 'BD Wong']).
movie('Jurassic World: Fallen Kingdom', 2018, 1, 128, 6.1, 'J.A. Bayona',      'Michael Giacchino', ['Chris Pratt', 'Bryce Dallas Howard', 'James Cromwell', 'BD Wong']).
movie('Jurassic World: Dominion',       2022, 1, 147, 5.6, 'Colin Trevorrow',  'Michael Giacchino', ['Chris Pratt', 'Bryce Dallas Howard', 'Campbell Scott', 'BD Wong']).

movie('Raiders of the Lost Ark',       1981, 2, 115, 8.4, 'Steven Spielberg', 'John Williams', ['Harrison Ford', 'Karen Allen', 'John Rhys-Davies']).
movie('The Temple of Doom',            1984, 2, 118, 7.5, 'Steven Spielberg', 'John Williams', ['Harrison Ford', 'Kate Capshaw', 'Ke Huy Quan']).
movie('The Last Crusade',              1989, 2, 127, 8.2, 'Steven Spielberg', 'John Williams', ['Harrison Ford', 'Alison Doody', 'Sean Connery']).
movie('Kingdom of the Crystal Skull',  2008, 2, 122, 6.2, 'Steven Spielberg', 'John Williams', ['Harrison Ford', 'Karen Allen', 'Shia LaBeouf']).

movie('The Phantom Menace',       1999, 3, 136, 6.5, 'George Lucas',     'John Williams', ['Ewan McGregor', 'Liam Neeson', 'Natalie Portman', 'Ian McDiarmid']).
movie('Attack of the Clones',     2002, 3, 142, 6.6, 'George Lucas',     'John Williams', ['Ewan McGregor', 'Hayden Christensen', 'Natalie Portman', 'Christopher Lee']).
movie('Revenge of the Sith',      2005, 3, 140, 7.6, 'George Lucas',     'John Williams', ['Ewan McGregor', 'Hayden Christensen', 'Natalie Portman', 'Christopher Lee']).
movie('A New Hope',               1977, 3, 121, 8.6, 'George Lucas',     'John Williams', ['Harrison Ford', 'Mark Hamill', 'Carrie Fisher', 'Alec Guinness']).
movie('The Empire Strikes Back',  1980, 3, 124, 8.7, 'Irvin Kershner',   'John Williams', ['Harrison Ford', 'Mark Hamill', 'Carrie Fisher', 'Billy Dee Williams']).
movie('Return of the Jedi',       1983, 3, 131, 8.3, 'Richard Marquand', 'John Williams', ['Harrison Ford', 'Mark Hamill', 'Carrie Fisher', 'Ian McDiarmid']).
movie('The Force Awakens',        2015, 3, 138, 7.8, 'J. J. Abrams',     'John Williams', ['Daisy Ridley', 'Harrison Ford', 'Mark Hamill', 'Carrie Fisher']).
movie('The Last Jedi',            2017, 3, 152, 6.9, 'Rian Johnson',     'John Williams', ['Daisy Ridley', 'Mark Hamill', 'Carrie Fisher', 'John Boyega']).
movie('The Rise of Skywalker',    2019, 3, 141, 6.4, 'J. J. Abrams',     'John Williams', ['Daisy Ridley', 'Mark Hamill', 'John Boyega', 'Adam Driver']).



% same_composer(?Movie1, ?Movie2)
same_composer(Movie1, Movie2) :-
    movie(Movie1,_, _, _, _, _, Composer, _),
    movie(Movie2,_, _, _, _, _, Composer, _),
    Movie1 \= Movie2.


% movie_from_saga(?Movie, ?Saga)
movie_from_saga(Movie, Saga) :-
    movie(Movie,_, SID, _, _, _, _, _),
    saga(SID,Saga,_,_).

% saga_longest_movie(?Saga, ?Movie)
saga_longest_movie(Saga, Movie) :-
    movie(Movie,_,SID,Duration,_,_,_,_),
    \+ (movie(_,_,_,D,_,_,_,_), D > Duration),
    saga(SID,Saga,_,_).


% add_movie_to_saga(+Saga, +Movie, +Year, +Duration, +Score, +Director, +Composer, +Cast)
add_movie_to_saga(Saga, Movie, Year, Duration, Score, Director, Composer, Cast) :-
    saga(SID, Saga, N, Creator),
    \+ movie(Movie, Year, SID, Duration, Score, Director, Composer, Cast),
    retractall(saga(SID, Saga, N, Creator)),
    New is N+1,
    assert(saga(SID, Saga, New, Creator)),
    assert(movie(Movie, Year, SID, Duration, Score, Director, Composer, Cast)).



% movies_from_saga(+Saga, -Movies)
movies_from_saga(Saga, Movies) :-
    saga(SID, Saga, _, _),
    find_movies(SID, [], Movies).


find_movies(SID, Acc, Movies) :-
    movie(Movie, Year, SID, _, _, _, _, _),
    \+ member(Year-Movie, Movies),
    find_movies(SID, [Year-Movie|Acc], Movies).




saga_aux([], Acc, Acc) :- !.
saga_aux([H|T], Acc, L) :-
    append(Acc, H, Acc1),
    saga_aux(T, Acc1, L).



%saga_cast(+Saga, -Cast)
saga_cast(Saga, Cast) :-
    findall(
        C,
        (
            saga(SID, Saga, _, _),
            movie(_, _, SID, _, _, _, _, C)
        ),
        L
    ),
    saga_aux(L, [], A),
    sort(A, Cast).



numerate(_, [] ,Acc, Acc) :- !.
numerate(N, [H|T] ,Acc, F) :-
    A is N +1,
    append(Acc, [A-H], Acc1),
    numerate(A, T, Acc1, F).


% sample_cast(+Saga, -Cast)
sample_cast(Saga, Cast) :-
    saga_cast(Saga, C),
    numerate(0, C ,[], L),
    findall(
        X,
        (
            member(I-X, L),
            A is I mod 2,
            A \= 0
        ),
        Cast
    ).


% composer_rating(+Composer, ?AvgScore)
composer_rating(Composer, AvgScore) :-
    findall(
        Score,
        (
            movie(_, _, _, _, Score, _, Composer, _)
        ),
        S
    ),
    sumlist(S, Sum),
    length(S, N),
    AvgScore is Sum/N.



