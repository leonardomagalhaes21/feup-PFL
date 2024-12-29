% Main menu predicate
main_menu :-
    write('======================================='), nl,
    write('              Mabula Game              '), nl,
    write('======================================='), nl,
    write('1. Start New Game'), nl,
    write('2. Instructions'), nl,
    write('3. Exit'), nl,
    write('======================================='), nl,
    write('Please enter your choice: '),
    read(Choice),
    handle_choice_menu(Choice).

% Handle user choice
handle_choice_menu(1) :-
    write('Starting a new game...'), nl,
    get_game_config(GameConfig),
    initial_state(GameConfig, GameState),
    play_game(GameState).

handle_choice_menu(2) :-
    display_instructions.

handle_choice_menu(3) :-
    write('Exiting...'), nl.
handle_choice_menu(_) :-
    write('Invalid choice, please try again.'), nl,
    main_menu.
    

display_instructions :-
    nl,
    write('======================================='), nl,
    write('              Mabula Rules             '), nl,
    write('======================================='), nl,
    write('1. Each player starts with 12 marbles.'), nl,
    write('2. Players take turns moving marbles.'), nl,
    write('3. Moves push marbles across the board.'), nl,
    write('4. The goal is to form the largest group.'), nl,
    write('5. Alternatively, you can decide to score'), nl,
    write('   based on the product of group sizes.'), nl,
    write('======================================='), nl,
    main_menu.



play_game(GameState) :-
    GameState = [Board, _CurrentPlayer, _Player1Type, _Player2Type, Rules, Player1Name, Player2Name],
    game_over(GameState, Winner),
    display_game(GameState),
    handle_game_over(Winner, Board, Player1Name, Player2Name, Rules).

play_game(GameState) :-
    GameState = [_Board, CurrentPlayer, Player1Type, Player2Type, _Rules, _Player1Name, _Player2Name],
    \+ game_over(GameState, _),
    display_game(GameState),
    handle_game_continue(GameState, CurrentPlayer, Player1Type, Player2Type).

handle_game_over(Winner, Board, Player1Name, Player2Name, Rules) :-
    calculate_scores(Board, OScore, XScore, Rules),
    format('Game over! ~w (o) Score: ~w, ~w (x) Score: ~w', [Player1Name, OScore, Player2Name, XScore]), nl,
    format('Winner: ~w', [Winner]), nl,
    main_menu.

handle_game_continue(GameState, CurrentPlayer, human, human) :-
    format('~w, it\'s your turn. Enter your move (e.g., "[3,1,3,5]."):', [CurrentPlayer]), nl,
    read(Move),
    handle_move(GameState, Move).

handle_game_continue(_, _, _, _) :-
    write('This mode is not supported yet.'), nl,
    main_menu.

handle_move(GameState, Move) :-
    move(GameState, Move, NewGameState),
    play_game(NewGameState).

handle_move(GameState, _) :-
    write('Invalid move, please try again.'), nl,
    play_game(GameState).






get_game_config(GameConfig) :-
    get_player_types(Player1Type, Player2Type),
    get_rules(Rules),
    get_player_name(1, Player1Name),
    get_player_name(2, Player2Name),
    GameConfig = [Player1Type, Player2Type, Rules, Player1Name, Player2Name].

get_player_types(Player1Type, Player2Type) :-
    write('Select player types:'), nl,
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Human'), nl,
    write('4. Computer vs Computer'), nl,
    read(Choice),
    handle_player_types_choice(Choice, Player1Type, Player2Type).

handle_player_types_choice(1, human, human).
handle_player_types_choice(2, human, computer).
handle_player_types_choice(3, computer, human).
handle_player_types_choice(4, computer, computer).
handle_player_types_choice(_, Player1Type, Player2Type) :-
    write('Invalid choice, please try again.'), nl,
    get_player_types(Player1Type, Player2Type).

get_rules(Rules) :-
    write('Select rules:'), nl,
    write('1. Default'), nl,
    write('2. Optional'), nl,
    read(Choice),
    handle_rules_choice(Choice, Rules).

handle_rules_choice(1, default_rules).
handle_rules_choice(2, optional_rules).
handle_rules_choice(_, Rules) :-
    write('Invalid choice, please try again.'), nl,
    get_rules(Rules).

get_player_name(PlayerNumber, PlayerName) :-
    format('Enter Player ~w name: ', [PlayerNumber]), nl,
    read(PlayerName).


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
    write('   1 2 3 4 5 6 7 8'), nl,
    write('  +---------------+'), nl,
    display_rows(Board, 1),
    write('  +---------------+'), nl.