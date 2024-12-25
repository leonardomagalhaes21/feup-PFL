% Utils

:- consult('board.pl').

default(empty).

char(o, 'O').
char(x, 'X').
char(empty, ' ').

player(1, player1).
player(2, player2).

board([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).

% Display board content
display_item(Item):- 
    char(Item, C), 
    write(C).

display_row([]).
display_row([Item|Items]):-
    write('|'), 
    display_item(Item),
    display_row(Items).

display_rows([], _).
display_rows([Row|Rows], N):- 
    write(N), 
    write(' '), 
    display_row(Row), 
    write('|'), nl, 
    N1 is N + 1, 
    display_rows(Rows, N1).

display_board([]).
display_board(Board):-
    nl,
    write('   A B C D E F G H'), nl,
    write('  +----------------+'), nl,
    display_rows(Board, 1),
    write('  +----------------+'), nl.





sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, S),
    Sum is H + S.

max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, M),
    Max is max(H, M).

cell_player(Board, RowIdx, ColIdx, Player) :-
    nth1(RowIdx, Board, Row),
    nth1(ColIdx, Row, Player).

calculate_largest_group(Board, Player, MaxSize) :- 
    findall(Size, (
        member(RowIdx, [1,2,3,4,5,6,7,8]), 
        member(ColIdx, [1,2,3,4,5,6,7,8]), 
        cell_player(Board, RowIdx, ColIdx, Player), 
        bfs(Board, Player, [[RowIdx, ColIdx]], [], Size)
    ), Sizes), 
    max_list(Sizes, MaxSize).

bfs(_, _, [], _, 0).
bfs(Board, Player, [[Row, Col] | Queue], Visited, Size) :-
    \+ member([Row, Col], Visited),
    cell_player(Board, Row, Col, Player),
    findall(
        [R, C], 
        (neighbor([Row, Col], [R, C]), 
         R > 0, R =< 8, C > 0, C =< 8, 
         cell_player(Board, R, C, Player), 
         \+ member([R, C], Visited)
        ), 
        Neighbors
    ),
    append(Queue, Neighbors, NewQueue),
    bfs(Board, Player, NewQueue, [[Row, Col] | Visited], S),
    Size is S + 1.
bfs(Board, Player, [_ | Queue], Visited, Size) :-
    bfs(Board, Player, Queue, Visited, Size).

neighbor([Row, Col], [Row1, Col]) :- Row1 is Row - 1. % Up
neighbor([Row, Col], [Row1, Col]) :- Row1 is Row + 1. % Down
neighbor([Row, Col], [Row, Col1]) :- Col1 is Col - 1. % Left
neighbor([Row, Col], [Row, Col1]) :- Col1 is Col + 1. % Right

calculate_scores(Board, OScore, XScore) :- 
    calculate_largest_group(Board, o, OScore), 
    calculate_largest_group(Board, x, XScore).




% Main menu predicate
main_menu :-
    write('Welcome to Mabula!'), nl,
    write('1. Start New Game'), nl,
    write('2. Exit'), nl,
    write('Please enter your choice: '),
    read(Choice),
    handle_choice(Choice).

% Handle user choice
handle_choice(1) :-
    write('Starting a new game...'), nl,
    generate_random_board(Board),
    display_board(Board),
    calculate_scores(Board, OScore, XScore),
    write('O Score: '), write(OScore), nl,
    write('X Score: '), write(XScore), nl,
    %...
    main_menu.
handle_choice(2) :-
    write('Exiting...'), nl.
handle_choice(_) :-
    write('Invalid choice, please try again.'), nl,
    main_menu.
    