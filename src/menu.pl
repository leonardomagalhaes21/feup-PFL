% Main menu predicate
main_menu :-
    write('Welcome to Mabula!'), nl,
    write('1. Start New Game'), nl,
    write('2. Exit'), nl,
    write('Please enter your choice: '),
    read(Choice),
    handle_choice_menu(Choice).

% Handle user choice
handle_choice_menu(1) :-
    write('Starting a new game...'), nl,
    get_game_config(GameConfig),
    initial_state(GameConfig, GameState),
    display_game(GameState),
    %...
    main_menu.
handle_choice_menu(2) :-
    write('Exiting...'), nl.
handle_choice_menu(_) :-
    write('Invalid choice, please try again.'), nl,
    main_menu.
    


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
    write('   A B C D E F G H'), nl,
    write('  +----------------+'), nl,
    display_rows(Board, 1),
    write('  +----------------+'), nl.