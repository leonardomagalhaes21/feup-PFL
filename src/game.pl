:- consult('logic.pl').
:- consult('menu.pl').

initial_state(GameConfig, GameState) :-
    GameConfig = [Player1Type, Player2Type, Rules, Player1Name, Player2Name],
    generate_random_board(Board),
    GameState = [Board, Player1Type ,Player1Type, Player2Type, Rules, Player1Name, Player2Name].

display_game(GameState) :-
    GameState = [Board, CurrentPlayer ,Player1Type, Player2Type, Rules, Player1Name, Player2Name],
    write('Current Player: '), write(CurrentPlayer), nl,
    display_board(Board),
    calculate_scores(Board, OScore, XScore, Rules),
    write('Current Player: '), write(CurrentPlayer), nl,
    format('~w (o) Score: ~w', [Player1Name, OScore]), nl,
    format('~w (x) Score: ~w', [Player2Name, XScore]), nl.


play :-
    main_menu.

:- play.
