:- consult('logic.pl').
:- consult('menu.pl').

initial_state(GameConfig, GameState) :-
    GameConfig = [StartingPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty],
    generate_random_board(Board),
    GameState = [Board, StartingPlayer ,Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty].

display_game(GameState) :-
    GameState = [Board, CurrentPlayer ,_Player1Type, _Player2Type, Rules, Player1Name, Player2Name, _Difficulty],
    nl,
    display_board(Board),
    calculate_scores(Board, OScore, XScore, Rules),
    write('Current Player: '), write(CurrentPlayer), nl,
    format('~w (o) Score: ~w', [Player1Name, OScore]), nl,
    format('~w (x) Score: ~w', [Player2Name, XScore]), nl,
    valid_moves(GameState, ListOfMoves),
    convert_real_moves(ListOfMoves, UserMoves),
    write('Valid Moves: '), write(UserMoves), nl.

valid_moves(GameState, ListOfMoves) :-
    GameState = [Board, CurrentPlayer | _],
    generate_moves(Board, CurrentPlayer, ListOfMoves).

move(GameState, Move, NewGameState) :-
    GameState = [Board, CurrentPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty],
    Move = [StartRow, StartCol, EndRow, EndCol],
    cell_player(Board, StartRow, StartCol, CurrentPlayer),
    valid_move(Board, [StartRow, StartCol], [EndRow, EndCol], CurrentPlayer),
    execute_move(Board, [StartRow, StartCol, EndRow, EndCol], NewBoard),
    switch_player(CurrentPlayer, NextPlayer),
    valid_moves([NewBoard, NextPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name], NextPlayerMoves),
    valid_moves_check(NewBoard, CurrentPlayer, NextPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty, NextPlayerMoves, NewGameState).

game_over(GameState, Winner) :-
    GameState = [Board, _, _, _, Rules, _, _, _Difficulty],
    calculate_scores(Board, OScore, XScore, Rules),
    valid_moves([Board, o | _], OMoves),
    valid_moves([Board, x | _], XMoves),
    check_winner(OMoves, XMoves, OScore, XScore, Winner).


% value(+GameState, +Player, -Value)
value(GameState, o, Value) :-
    GameState = [Board, _, _, _, Rules, _, _, _],
    calculate_scores(Board, OScore, XScore, Rules),
    Value is OScore - XScore.

value(GameState, x, Value) :-
    GameState = [Board, _, _, _, Rules, _, _, _],
    calculate_scores(Board, OScore, XScore, Rules),
    Value is XScore - OScore.


% choose_move(+GameState, +Level, -Move)
choose_move(GameState, 1, Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).

choose_move(GameState, 2, Move) :-
    valid_moves(GameState, Moves),
    findall(Value-M, (member(M, Moves), simulate_move(GameState, M, Value)), MoveValues),
    max_member(BestValue-Move, MoveValues),
    convert_real_move(Move, M),
    format('Best move: ~w, Value: ~w', [M, BestValue]), nl.

    
play :-
    main_menu.

:- play.
