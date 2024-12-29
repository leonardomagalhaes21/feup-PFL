:- consult('logic.pl').
:- consult('menu.pl').

initial_state(GameConfig, GameState) :-
    GameConfig = [Player1Type, Player2Type, Rules, Player1Name, Player2Name],
    generate_random_board(Board),
    GameState = [Board, o ,Player1Type, Player2Type, Rules, Player1Name, Player2Name].

display_game(GameState) :-
    GameState = [Board, CurrentPlayer ,Player1Type, Player2Type, Rules, Player1Name, Player2Name],
    write('Current Player: '), write(CurrentPlayer), nl,
    display_board(Board),
    calculate_scores(Board, OScore, XScore, Rules),
    write('Current Player: '), write(CurrentPlayer), nl,
    format('~w (o) Score: ~w', [Player1Name, OScore]), nl,
    format('~w (x) Score: ~w', [Player2Name, XScore]), nl,
    valid_moves(GameState, ListOfMoves),
    write('Valid Moves: '), write(ListOfMoves), nl.

valid_moves(GameState, ListOfMoves) :-
    GameState = [Board, CurrentPlayer | _],
    generate_moves(Board, CurrentPlayer, ListOfMoves).

move(GameState, Move, NewGameState) :-
    GameState = [Board, CurrentPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name],
    Move = [StartRow, StartCol, EndRow, EndCol],
    cell_player(Board, StartRow, StartCol, CurrentPlayer),
    valid_move(Board, [StartRow, StartCol], [EndRow, EndCol], CurrentPlayer),
    execute_move(Board, [StartRow, StartCol, EndRow, EndCol], NewBoard),
    switch_player(CurrentPlayer, NextPlayer),
    NewGameState = [NewBoard, NextPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name].

play :-
    main_menu.

:- play.
