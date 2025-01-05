% menu.pl

% Utiliza o módulo 'system' para funções auxiliares.
:- use_module(library(system)).

% Exibe o menu principal do jogo.
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

% Lida com a escolha do usuário no menu principal.
handle_choice_menu(1) :-
    write('Starting a new game...'), nl,
    get_game_config(GameConfig),  % Obtém as configurações do jogo.
    initial_state(GameConfig, GameState),  % Inicializa o estado do jogo.
    play_game(GameState).

handle_choice_menu(2) :-
    display_instructions.  % Mostra as instruções do jogo.

handle_choice_menu(3) :-
    write('Exiting...'), nl.

handle_choice_menu(_) :-
    write('Invalid choice, please try again.'), nl,
    main_menu.

% Converte o formato de movimento do usuário para o formato interno.
convert_user_move([StartCol, UserStartRow, EndCol, UserEndRow], [RealStartRow, StartCol, RealEndRow, EndCol]) :-
    RealStartRow is 9 - UserStartRow,
    RealEndRow is 9 - UserEndRow.

% Converte o formato de movimento interno para o formato exibido ao usuário.
convert_real_move([StartRow, StartCol, EndRow, EndCol], [StartCol, UserStartRow, EndCol, UserEndRow]) :-
    UserStartRow is 9 - StartRow,
    UserEndRow is 9 - EndRow.

convert_real_moves([], []).
convert_real_moves([Move|Moves], [UserMove|UserMoves]) :-
    convert_real_move(Move, UserMove),
    convert_real_moves(Moves, UserMoves).

% Mostra as instruções do jogo.
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

% Gerencia o fluxo de jogo principal.
play_game(GameState) :-
    GameState = [Board, _CurrentPlayer, _Player1Type, _Player2Type, Rules, Player1Name, Player2Name, _Difficulty],
    game_over(GameState, Winner),  % Verifica se o jogo terminou.
    display_game(GameState),  % Exibe o estado atual do jogo.
    handle_game_over(Winner, Board, Player1Name, Player2Name, Rules).

play_game(GameState) :-
    GameState = [_Board, CurrentPlayer, Player1Type, Player2Type, _Rules, _Player1Name, _Player2Name, Difficulty],
    \+ game_over(GameState, _),  % Continua enquanto o jogo não acabou.
    display_game(GameState),
    handle_game_continue(GameState, CurrentPlayer, Player1Type, Player2Type, Difficulty).

% Trata o término do jogo exibindo os resultados.
handle_game_over(Winner, Board, Player1Name, Player2Name, Rules) :-
    calculate_scores(Board, OScore, XScore, Rules),
    format('Game over! ~w (o) Score: ~w, ~w (x) Score: ~w', [Player1Name, OScore, Player2Name, XScore]), nl,
    format('Winner: ~w', [Winner]), nl,
    main_menu.

% Gerencia a continuação do jogo com base no tipo de jogadores (humanos ou IA).
handle_game_continue(GameState, CurrentPlayer, human, human, _) :-
    format('~w, it\'s your turn. Enter your move ("[Xi,Yi,Xf,Yf]."):', [CurrentPlayer]), nl,
    read(UserMove),
    convert_user_move(UserMove, Move),
    handle_move(GameState, Move).

handle_game_continue(GameState, o, human, computer, _) :-
    format('~w, it\'s your turn. Enter your move ("[Xi,Yi,Xf,Yf]."):', o), nl,
    read(UserMove),
    convert_user_move(UserMove, Move),
    handle_move(GameState, Move).

handle_game_continue(GameState, x , human, computer, Difficulty) :-
    choose_move(GameState, Difficulty, ComputerMove),  % IA escolhe movimento.
    sleep(1),
    handle_move(GameState, ComputerMove).

handle_game_continue(GameState, x, computer, human, _) :-
    format('~w, it\'s your turn. Enter your move ("[Xi,Yi,Xf,Yf]."):', x), nl,
    read(UserMove),
    convert_user_move(UserMove, Move),
    handle_move(GameState, Move).

handle_game_continue(GameState, o , computer, human, Difficulty) :-
    choose_move(GameState, Difficulty, ComputerMove),
    sleep(1),
    handle_move(GameState, ComputerMove).

handle_game_continue(GameState, _CurrentPlayer, computer, computer, Difficulty) :-
    choose_move(GameState, Difficulty, ComputerMove),
    sleep(1),
    handle_move(GameState, ComputerMove).

handle_game_continue(_, _, _, _) :-
    write('This mode is not supported yet.'), nl,
    main_menu.

% Processa o movimento e atualiza o estado do jogo.
handle_move(GameState, Move) :-
    move(GameState, Move, NewGameState),
    play_game(NewGameState).

handle_move(GameState, _) :-
    write('Invalid move, please try again.'), nl,
    play_game(GameState).

% Obtém as configurações iniciais do jogo.
get_game_config(GameConfig) :-
    get_player_types(Player1Type, Player2Type),
    get_rules(Rules),
    get_player_name(1, Player1Name),
    get_player_name(2, Player2Name),
    get_starting_player(StartingPlayer),
    get_difficulty(Player1Type, Player2Type, Difficulty),
    GameConfig = [StartingPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty].

% Obtém o jogador inicial com base na escolha do usuário.
get_starting_player(StartingPlayer) :-
    write('Select starting player:'), nl,
    write('1. Player 1 (O)'), nl,
    write('2. Player 2 (X)'), nl,
    read(Choice),
    handle_starting_player_choice(Choice, StartingPlayer).

handle_starting_player_choice(1, o).
handle_starting_player_choice(2, x).
handle_starting_player_choice(_, StartingPlayer) :-
    write('Invalid choice, please try again.'), nl,
    get_starting_player(StartingPlayer).

% Obtém a dificuldade caso IA esteja envolvida.
get_difficulty(human, human, 0).
get_difficulty(_, _, Difficulty) :-
    write('Select difficulty:'), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    read(Choice),
    handle_difficulty_choice(Choice, Difficulty).

handle_difficulty_choice(1, 1).
handle_difficulty_choice(2, 2).
handle_difficulty_choice(_, Difficulty) :-
    write('Invalid choice, please try again.'), nl,
    get_difficulty(_, _, Difficulty).

% Obtém os tipos de jogadores (Humanos ou IA).
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

% Obtém as regras do jogo escolhidas pelo jogador.
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

% Obtém o nome de um jogador.
get_player_name(PlayerNumber, PlayerName) :-
    format('Enter Player ~w name: ', [PlayerNumber]), nl,
    read(PlayerName).

% Exibe um item do tabuleiro com base no caractere associado.
display_item(Item):- 
    char(Item, C), 
    write(C).

% Exibe uma linha do tabuleiro.
display_row([]).
display_row([Item|Items]):-
    write('|'), 
    display_item(Item),
    display_row(Items).

% Exibe todas as linhas do tabuleiro.
display_rows([], _, _).
display_rows([Row|Rows], TotalRows, N):- 
    DisplayRowNum is TotalRows - N + 1,
    write(DisplayRowNum), 
    write(' '), 
    display_row(Row), 
    write('|'), nl, 
    N1 is N + 1, 
    display_rows(Rows, TotalRows, N1).

% Exibe o tabuleiro completo.
display_board([]).
display_board(Board):-
    write('  +---------------+'), nl,
    length(Board, TotalRows),
    display_rows(Board, TotalRows, 1),
    write('  +---------------+'), nl,
    write('   1 2 3 4 5 6 7 8'), nl,
    nl.
