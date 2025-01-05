% game.pl

% Inclui outros ficheiros com lógica adicional e o menu principal.
:- consult('logic.pl').
:- consult('menu.pl').

% Inicializa o estado do jogo.
% Recebe uma configuração de jogo e gera o estado inicial com um tabuleiro aleatório.
initial_state(GameConfig, GameState) :-
    GameConfig = [StartingPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty],
    generate_random_board(Board),  % Gera um tabuleiro inicial aleatório.
    GameState = [Board, StartingPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty].

% Exibe o estado atual do jogo.
% Mostra o tabuleiro, pontuações, jogador atual e movimentos válidos.
display_game(GameState) :-
    GameState = [Board, CurrentPlayer, _Player1Type, _Player2Type, Rules, Player1Name, Player2Name, _Difficulty],
    nl,
    display_board(Board),  % Mostra o tabuleiro.
    calculate_scores(Board, OScore, XScore, Rules),  % Calcula as pontuações com base nas regras.
    write('Current Player: '), write(CurrentPlayer), nl,
    format('~w (o) Score: ~w', [Player1Name, OScore]), nl,
    format('~w (x) Score: ~w', [Player2Name, XScore]), nl,
    valid_moves(GameState, ListOfMoves),  % Gera movimentos válidos.
    convert_real_moves(ListOfMoves, UserMoves),  % Converte movimentos para exibição amigável.
    write('Valid Moves: '), write(UserMoves), nl.

% Gera todos os movimentos válidos para o jogador atual.
valid_moves(GameState, ListOfMoves) :-
    GameState = [Board, CurrentPlayer | _],
    generate_moves(Board, CurrentPlayer, ListOfMoves).

% Executa um movimento e gera um novo estado de jogo.
% Verifica se o movimento é válido, realiza a troca de jogador e atualiza o estado.
move(GameState, Move, NewGameState) :-
    GameState = [Board, CurrentPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty],
    Move = [StartRow, StartCol, EndRow, EndCol],
    cell_player(Board, StartRow, StartCol, CurrentPlayer),  % Verifica a célula inicial pertence ao jogador atual.
    valid_move(Board, [StartRow, StartCol], [EndRow, EndCol], CurrentPlayer),  % Confirma que o movimento é válido.
    execute_move(Board, [StartRow, StartCol, EndRow, EndCol], NewBoard),  % Realiza o movimento no tabuleiro.
    switch_player(CurrentPlayer, NextPlayer),  % Troca o jogador atual.
    valid_moves([NewBoard, NextPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name], NextPlayerMoves),
    valid_moves_check(NewBoard, CurrentPlayer, NextPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty, NextPlayerMoves, NewGameState).

% Verifica se o jogo terminou e identifica o vencedor.
game_over(GameState, Winner) :-
    GameState = [Board, _, _, _, Rules, _, _, _Difficulty],
    calculate_scores(Board, OScore, XScore, Rules),  % Calcula as pontuações finais.
    valid_moves([Board, o | _], OMoves),  % Verifica movimentos restantes para o jogador 'o'.
    valid_moves([Board, x | _], XMoves),  % Verifica movimentos restantes para o jogador 'x'.
    check_winner(OMoves, XMoves, OScore, XScore, Winner).  % Determina o vencedor.

% Calcula o valor de um estado de jogo para um jogador específico.
% Usado por algoritmos de IA para avaliar o estado.
value(GameState, o, Value) :-
    GameState = [Board, _, _, _, Rules, _, _, _],
    calculate_scores(Board, OScore, XScore, Rules),
    Value is OScore - XScore.

value(GameState, x, Value) :-
    GameState = [Board, _, _, _, Rules, _, _, _],
    calculate_scores(Board, OScore, XScore, Rules),
    Value is XScore - OScore.

% Escolhe um movimento baseado no nível de dificuldade.
% Nível 1: Escolha aleatória.
% Nível 2: Escolha o movimento com melhor valor.
choose_move(GameState, 1, Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).  % Escolhe um movimento aleatório.

choose_move(GameState, 2, Move) :-
    valid_moves(GameState, Moves),
    findall(Value-M, (member(M, Moves), simulate_move(GameState, M, Value)), MoveValues),  % Avalia todos os movimentos possíveis.
    max_member(BestValue-Move, MoveValues),  % Escolhe o movimento com o maior valor.
    convert_real_move(Move, M),
    format('Best move: ~w, Value: ~w', [M, BestValue]), nl.

% Inicia o jogo a partir do menu principal.
play :-
    main_menu.

:- play.
