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



sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, S),
    Sum is H + S.

max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, M),
    Max is max(H, M).

product_list([], 1).
product_list([H|T], Product) :- 
    product_list(T, P), 
    Product is H * P.

cell_player(Board, RowIdx, ColIdx, Player) :-
    nth1(RowIdx, Board, Row),
    nth1(ColIdx, Row, Player).

bfs(_, _, [], Visited, 0, Visited) :- !.
bfs(Board, Player, [[Row, Col] | Queue], Visited, Size, NewVisited) :- 
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
    bfs(Board, Player, NewQueue, [[Row, Col] | Visited], S, NewVisited),
    Size is S + 1, !.
bfs(Board, Player, [_ | Queue], Visited, Size, NewVisited) :- 
    bfs(Board, Player, Queue, Visited, Size, NewVisited).

calculate_largest_group(Board, Player, MaxSize) :- 
    findall(Size, (
        member(RowIdx, [1,2,3,4,5,6,7,8]), 
        member(ColIdx, [1,2,3,4,5,6,7,8]), 
        cell_player(Board, RowIdx, ColIdx, Player), 
        bfs(Board, Player, [[RowIdx, ColIdx]], [], Size, _)
    ), Sizes), 
    max_list(Sizes, MaxSize).

calculate_group_sizes(Board, Player, Sizes) :- 
    calculate_group_sizes_aux(Board, Player, [], Sizes).

calculate_group_sizes_aux(_, _, Visited, []) :- 
    length(Visited, L), L >= 64, !.
calculate_group_sizes_aux(Board, Player, Visited, Sizes) :- 
    member(RowIdx, [1,2,3,4,5,6,7,8]), 
    member(ColIdx, [1,2,3,4,5,6,7,8]), 
    \+ member([RowIdx, ColIdx], Visited), 
    \+ cell_player(Board, RowIdx, ColIdx, Player), 
    calculate_group_sizes_aux(Board, Player, [[RowIdx, ColIdx] | Visited], Sizes), !.
calculate_group_sizes_aux(Board, Player, Visited, [Size | Sizes]) :- 
    member(RowIdx, [1,2,3,4,5,6,7,8]), 
    member(ColIdx, [1,2,3,4,5,6,7,8]), 
    \+ member([RowIdx, ColIdx], Visited), 
    cell_player(Board, RowIdx, ColIdx, Player), 
    bfs(Board, Player, [[RowIdx, ColIdx]], Visited, Size, NewVisited),
    Size > 0,
    calculate_group_sizes_aux(Board, Player, NewVisited, Sizes), !.

neighbor([Row, Col], [Row1, Col]) :- Row1 is Row - 1. % Up
neighbor([Row, Col], [Row1, Col]) :- Row1 is Row + 1. % Down
neighbor([Row, Col], [Row, Col1]) :- Col1 is Col - 1. % Left
neighbor([Row, Col], [Row, Col1]) :- Col1 is Col + 1. % Right

calculate_scores(Board, OScore, XScore, default_rules) :- 
    calculate_largest_group(Board, o, OScore), 
    calculate_largest_group(Board, x, XScore).

calculate_scores(Board, OScore, XScore, optional_rules) :- 
    calculate_group_sizes(Board, o, OSizes),
    calculate_group_sizes(Board, x, XSizes),
    product_list(OSizes, OScore),
    product_list(XSizes, XScore).
