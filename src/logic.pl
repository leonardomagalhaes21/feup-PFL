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


between(Low, High, Low) :- Low =< High.
between(Low, High, Value) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, Value).

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






not_on_edge([EndRow, EndCol]) :-
    EndRow > 1, EndRow < 8,
    EndCol > 1, EndCol < 8.

within_bounds(Row, Col) :-
    Row >= 1, Row =< 8,
    Col >= 1, Col =< 8.

cell_belongs_to_player(Board, [Row, Col], Player) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Cell),
    Cell == Player.

valid_direction([StartRow, StartCol], [StartRow, EndCol]) :-
    StartCol \= EndCol.
valid_direction([StartRow, StartCol], [EndRow, StartCol]) :-
    StartRow \= EndRow.


check_horizontal_slide(Board, Row, StartCol, EndCol) :-
    StartCol < EndCol,
    check_horizontal_slide_left_to_right(Board, Row, StartCol, EndCol).
check_horizontal_slide(Board, Row, StartCol, EndCol) :-
    StartCol >= EndCol,
    check_horizontal_slide_right_to_left(Board, Row, StartCol, EndCol).

check_horizontal_slide_left_to_right(Board, Row, StartCol, EndCol) :-
    findall(Cell, (
        between(StartCol, EndCol, Col),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell \= empty
    ), Marbles),
    length(Marbles, M1),

    findall(Cell, (
        between(EndCol, 7, Col),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell == empty
    ), EmptySpaces),
    length(EmptySpaces, EmptyCount),

    EmptyCount >= M1.

check_horizontal_slide_right_to_left(Board, Row, StartCol, EndCol) :-
    findall(Cell, (
        between(EndCol, StartCol, Col),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell \= empty
    ), Marbles),
    length(Marbles, M1),

    findall(Cell, (
        between(2, EndCol, Col),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell == empty
    ), EmptySpaces),
    length(EmptySpaces, EmptyCount),

    EmptyCount >= M1.

check_vertical_slide(Board, Col, StartRow, EndRow) :-
    StartRow < EndRow,
    check_vertical_slide_top_to_bottom(Board, Col, StartRow, EndRow).
check_vertical_slide(Board, Col, StartRow, EndRow) :-
    StartRow >= EndRow,
    check_vertical_slide_bottom_to_top(Board, Col, StartRow, EndRow).

check_vertical_slide_top_to_bottom(Board, Col, StartRow, EndRow) :-
    findall(Cell, (
        between(StartRow, EndRow, Row),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell \= empty
    ), Marbles),
    length(Marbles, M1),

    findall(Cell, (
        between(EndRow, 7, Row),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell == empty
    ), EmptySpaces),
    length(EmptySpaces, EmptyCount),

    EmptyCount >= M1.

check_vertical_slide_bottom_to_top(Board, Col, StartRow, EndRow) :-
    findall(Cell, (
        between(EndRow, StartRow, Row),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell \= empty
    ), Marbles),
    length(Marbles, M1),

    findall(Cell, (
        between(2, EndRow, Row),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell == empty
    ), EmptySpaces),
    length(EmptySpaces, EmptyCount),

    EmptyCount >= M1.

valid_slide(Board, [StartRow, StartCol], [StartRow, EndCol]) :-
    check_horizontal_slide(Board, StartRow, StartCol, EndCol).
valid_slide(Board, [StartRow, StartCol], [EndRow, StartCol]) :-
    check_vertical_slide(Board, StartCol, StartRow, EndRow).

valid_move(Board, [StartRow, StartCol], [EndRow, EndCol], Player) :-
    not_on_edge([EndRow, EndCol]),
    within_bounds(StartRow, StartCol),
    within_bounds(EndRow, EndCol),
    cell_belongs_to_player(Board, [StartRow, StartCol], Player),
    valid_direction([StartRow, StartCol], [EndRow, EndCol]),
    valid_slide(Board, [StartRow, StartCol], [EndRow, EndCol]).
