% logic.pl

% Este módulo contém funções utilitárias e a lógica principal para validar movimentos e calcular pontuações no jogo Mabula.

% Inclui o ficheiro com lógica relacionada ao tabuleiro.
:- consult('board.pl').

% Define valores padrão para células do tabuleiro.
default(empty).

% Associa jogadores e seus caracteres para exibição no tabuleiro.
char(o, 'O').
char(x, 'X').
char(empty, ' ').

% Mapeia o número do jogador para identificadores internos.
player(1, player1).
player(2, player2).

% Alterna entre os jogadores 'o' e 'x'.
switch_player(o, x).
switch_player(x, o).

% Representação inicial de um tabuleiro vazio 8x8.
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

% Gera um valor entre dois limites (Low e High), inclusive.
between(Low, High, Low) :- Low =< High.
between(Low, High, Value) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, Value).

% Calcula a soma dos elementos de uma lista.
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, S),
    Sum is H + S.

% Determina o maior valor em uma lista.
max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, M),
    Max is max(H, M).

% Calcula o produto dos elementos de uma lista.
product_list([], 1).
product_list([H|T], Product) :- 
    product_list(T, P), 
    Product is H * P.

% Verifica se uma célula pertence ao jogador especificado.
cell_player(Board, RowIdx, ColIdx, Player) :-
    nth1(RowIdx, Board, Row),
    nth1(ColIdx, Row, Player).

% Implementa a busca em largura (BFS) para calcular grupos conectados de células.
bfs(_, _, [], Visited, 0, Visited) :- !.
bfs(Board, Player, [[Row, Col] | Queue], Visited, Size, NewVisited) :- 
    \+ member([Row, Col], Visited), % Verifica se a célula já foi visitada.
    cell_player(Board, Row, Col, Player), % Confirma que a célula pertence ao jogador.
    findall(
        [R, C], 
        (neighbor([Row, Col], [R, C]), % Localiza vizinhos válidos.
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

% Calcula o maior grupo de células conectadas para um jogador.
calculate_largest_group(Board, Player, MaxSize) :- 
    findall(Size, (
        member(RowIdx, [1,2,3,4,5,6,7,8]), % Percorre as linhas do tabuleiro.
        member(ColIdx, [1,2,3,4,5,6,7,8]), % Percorre as colunas do tabuleiro.
        cell_player(Board, RowIdx, ColIdx, Player), 
        bfs(Board, Player, [[RowIdx, ColIdx]], [], Size, _)
    ), Sizes), 
    max_list(Sizes, MaxSize).

% Calcula os tamanhos de todos os grupos conectados de células de um jogador.
calculate_group_sizes(Board, Player, Sizes) :- 
    calculate_group_sizes_aux(Board, Player, [], Sizes).

% Função auxiliar para calcular os tamanhos de grupos conectados.
calculate_group_sizes_aux(_, _, Visited, []) :- 
    length(Visited, L), L >= 64, !. % Condição de parada quando todas as células foram visitadas.
calculate_group_sizes_aux(Board, Player, Visited, Sizes) :- 
    member(RowIdx, [1,2,3,4,5,6,7,8]), % Varre as linhas do tabuleiro.
    member(ColIdx, [1,2,3,4,5,6,7,8]), % Varre as colunas do tabuleiro.
    \+ member([RowIdx, ColIdx], Visited), % Garante que a célula ainda não foi visitada.
    \+ cell_player(Board, RowIdx, ColIdx, Player), % Ignora células que não pertencem ao jogador.
    calculate_group_sizes_aux(Board, Player, [[RowIdx, ColIdx] | Visited], Sizes), !.
calculate_group_sizes_aux(Board, Player, Visited, [Size | Sizes]) :- 
    member(RowIdx, [1,2,3,4,5,6,7,8]), 
    member(ColIdx, [1,2,3,4,5,6,7,8]), 
    \+ member([RowIdx, ColIdx], Visited), 
    cell_player(Board, RowIdx, ColIdx, Player), 
    bfs(Board, Player, [[RowIdx, ColIdx]], Visited, Size, NewVisited),
    Size > 0, % Considera apenas grupos com tamanho maior que 0.
    calculate_group_sizes_aux(Board, Player, NewVisited, Sizes), !.

% Retorna as coordenadas das células vizinhas de uma célula especificada.
neighbor([Row, Col], [Row1, Col]) :- Row1 is Row - 1. % Célula acima
neighbor([Row, Col], [Row1, Col]) :- Row1 is Row + 1. % Célula abaixo
neighbor([Row, Col], [Row, Col1]) :- Col1 is Col - 1. % Célula à esquerda
neighbor([Row, Col], [Row, Col1]) :- Col1 is Col + 1. % Célula à direita

% Calcula as pontuações com base nas regras de jogo especificadas.
calculate_scores(Board, OScore, XScore, default_rules) :- 
    calculate_largest_group(Board, o, OScore), % Pontuação baseada no maior grupo do jogador 'o'.
    calculate_largest_group(Board, x, XScore). % Pontuação baseada no maior grupo do jogador 'x'.

calculate_scores(Board, OScore, XScore, optional_rules) :- 
    calculate_group_sizes(Board, o, OSizes), % Calcula tamanhos de grupos conectados para 'o'.
    calculate_group_sizes(Board, x, XSizes), % Calcula tamanhos de grupos conectados para 'x'.
    product_list(OSizes, OScore), % Produto dos tamanhos de grupos para 'o'.
    product_list(XSizes, XScore). % Produto dos tamanhos de grupos para 'x'.


% Verifica se uma célula está na borda do tabuleiro.
not_on_edge([EndRow, EndCol]) :-
    EndRow > 1, EndRow < 8,  % Linha está dentro dos limites internos (não na borda).
    EndCol > 1, EndCol < 8.  % Coluna está dentro dos limites internos (não na borda).

% Verifica se uma célula está dentro dos limites do tabuleiro (1 a 8).
within_bounds(Row, Col) :-
    Row >= 1, Row =< 8,
    Col >= 1, Col =< 8.

% Verifica se uma célula pertence ao jogador especificado.
cell_belongs_to_player(Board, [Row, Col], Player) :-
    nth1(Row, Board, BoardRow),  % Obtém a linha correspondente no tabuleiro.
    nth1(Col, BoardRow, Cell),   % Obtém a célula correspondente na linha.
    Cell = Player.

% Determina se o movimento é horizontal e válido.
valid_direction([StartRow, StartCol], [StartRow, EndCol]) :-
    StartCol \= EndCol.  % Colunas devem ser diferentes para ser um movimento horizontal.

% Determina se o movimento é vertical e válido.
valid_direction([StartRow, StartCol], [EndRow, StartCol]) :-
    StartRow \= EndRow.  % Linhas devem ser diferentes para ser um movimento vertical.

% Verifica se o deslizamento horizontal da célula é válido.
check_horizontal_slide(Board, Row, StartCol, EndCol) :-
    StartCol < EndCol,  % Movimento para a direita.
    check_horizontal_slide_left_to_right(Board, Row, StartCol, EndCol).
check_horizontal_slide(Board, Row, StartCol, EndCol) :-
    StartCol >= EndCol,  % Movimento para a esquerda.
    check_horizontal_slide_right_to_left(Board, Row, StartCol, EndCol).

% Valida o deslizamento horizontal da esquerda para a direita.
check_horizontal_slide_left_to_right(Board, Row, StartCol, EndCol) :-
    % Conta as células ocupadas no caminho.
    findall(Cell, (
        C is EndCol - 1,
        between(StartCol, C, Col),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell \= empty  % Deve haver marbles no caminho.
    ), Marbles),
    length(Marbles, M1),  % Número de marbles no caminho.

    % Conta os espaços vazios após o destino.
    findall(Cell, (
        between(EndCol, 7, Col),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell = empty
    ), EmptySpaces),
    length(EmptySpaces, EmptyCount),

    EmptyCount >= M1.  % Espaços vazios suficientes para o deslizamento.

% Valida o deslizamento horizontal da direita para a esquerda.
check_horizontal_slide_right_to_left(Board, Row, StartCol, EndCol) :-
    % Conta as células ocupadas no caminho.
    findall(Cell, (
        C is EndCol + 1,
        between(C, StartCol, Col),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell \= empty
    ), Marbles),
    length(Marbles, M1),

    % Conta os espaços vazios após o destino.
    findall(Cell, (
        between(2, EndCol, Col),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell = empty
    ), EmptySpaces),
    length(EmptySpaces, EmptyCount),

    EmptyCount >= M1.

% Verifica se o deslizamento vertical da célula é válido.
check_vertical_slide(Board, Col, StartRow, EndRow) :-
    StartRow < EndRow,  % Movimento para baixo.
    check_vertical_slide_top_to_bottom(Board, Col, StartRow, EndRow).
check_vertical_slide(Board, Col, StartRow, EndRow) :-
    StartRow >= EndRow,  % Movimento para cima.
    check_vertical_slide_bottom_to_top(Board, Col, StartRow, EndRow).

% Valida o deslizamento vertical de cima para baixo.
check_vertical_slide_top_to_bottom(Board, Col, StartRow, EndRow) :-
    % Conta as células ocupadas no caminho.
    findall(Cell, (
        R is EndRow - 1,
        between(StartRow, R, Row),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell \= empty
    ), Marbles),
    length(Marbles, M1),

    % Conta os espaços vazios após o destino.
    findall(Cell, (
        between(EndRow, 7, Row),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell = empty
    ), EmptySpaces),
    length(EmptySpaces, EmptyCount),

    EmptyCount >= M1.

% Valida o deslizamento vertical de baixo para cima.
check_vertical_slide_bottom_to_top(Board, Col, StartRow, EndRow) :-
    % Conta as células ocupadas no caminho.
    findall(Cell, (
        R is EndRow + 1,
        between(R, StartRow, Row),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell \= empty
    ), Marbles),
    length(Marbles, M1),

    % Conta os espaços vazios após o destino.
    findall(Cell, (
        between(2, EndRow, Row),
        nth1(Row, Board, BoardRow),
        nth1(Col, BoardRow, Cell),
        Cell = empty
    ), EmptySpaces),
    length(EmptySpaces, EmptyCount),

    EmptyCount >= M1.

% Verifica se o deslizamento (horizontal ou vertical) é válido.
valid_slide(Board, [StartRow, StartCol], [StartRow, EndCol]) :-
    check_horizontal_slide(Board, StartRow, StartCol, EndCol).
valid_slide(Board, [StartRow, StartCol], [EndRow, StartCol]) :-
    check_vertical_slide(Board, StartCol, StartRow, EndRow).

% Verifica se um movimento específico é válido.
valid_move(Board, [StartRow, StartCol], [EndRow, EndCol], Player) :-
    not_on_edge([EndRow, EndCol]),  % A célula final não deve estar na borda.
    \+ not_on_edge([StartRow, StartCol]),  % A célula inicial deve estar na borda.
    within_bounds(StartRow, StartCol),
    within_bounds(EndRow, EndCol),
    cell_belongs_to_player(Board, [StartRow, StartCol], Player),
    valid_direction([StartRow, StartCol], [EndRow, EndCol]),
    valid_slide(Board, [StartRow, StartCol], [EndRow, EndCol]).


% Gera todos os movimentos válidos para o jogador atual.
% Percorre todas as combinações possíveis de posições iniciais e finais no tabuleiro (8x8), verificando se cada movimento é válido.
generate_moves(Board, Player, Moves) :-
    findall([StartRow, StartCol, EndRow, EndCol], (
        between(1, 8, StartRow),  % Todas as linhas iniciais.
        between(1, 8, StartCol),  % Todas as colunas iniciais.
        between(1, 8, EndRow),    % Todas as linhas finais.
        between(1, 8, EndCol),    % Todas as colunas finais.
        valid_move(Board, [StartRow, StartCol], [EndRow, EndCol], Player)  % Verifica a validade do movimento.
    ), Moves).

% Executa um movimento no tabuleiro, alterando o estado.
% Identifica o jogador responsável pela célula inicial e move as peças.
execute_move(Board, [StartRow, StartCol, EndRow, EndCol], NewBoard) :-
    cell_player(Board, StartRow, StartCol, Player),  % Obtém o jogador da célula inicial.
    move_marbles(Board, [StartRow, StartCol], [EndRow, EndCol], Player, NewBoard).  % Move as peças.

% Move as peças no tabuleiro na direção especificada (horizontal ou vertical).
% Calcula a direção do movimento com base nas diferenças de linhas e colunas.
move_marbles(Board, [StartRow, StartCol], [EndRow, EndCol], Player, NewBoard) :-
    DeltaRow is sign(EndRow - StartRow),  % Direção vertical (+1, 0, -1).
    DeltaCol is sign(EndCol - StartCol),  % Direção horizontal (+1, 0, -1).
    move_marbles_aux(Board, [StartRow, StartCol], DeltaRow, DeltaCol, [EndRow, EndCol], Player, NewBoard).

% Caso base: Se a posição atual for igual à posição final, o movimento termina.
move_marbles_aux(Board, [Row, Col], _, _, [Row, Col], _, Board).

% Recursivamente move as peças até a posição final.
move_marbles_aux(Board, [Row, Col], DeltaRow, DeltaCol, [EndRow, EndCol], Player, NewBoard) :-
    move_marble_to_next(Board, [Row, Col], DeltaRow, DeltaCol, TempBoard),  % Move a peça para a próxima posição.
    NextRow is Row + DeltaRow,  % Calcula a próxima linha.
    NextCol is Col + DeltaCol,  % Calcula a próxima coluna.
    move_marbles_aux(TempBoard, [NextRow, NextCol], DeltaRow, DeltaCol, [EndRow, EndCol], Player, NewBoard).

% Move a peça da posição atual para a próxima célula, considerando o estado da próxima célula.
move_marble_to_next(Board, [Row, Col], DeltaRow, DeltaCol, NewBoard) :-
    NextRow is Row + DeltaRow,
    NextCol is Col + DeltaCol,
    cell_player(Board, Row, Col, Player),  % Obtém o jogador da célula atual.
    handle_next_position(Board, [Row, Col], [NextRow, NextCol], Player, DeltaRow, DeltaCol, NewBoard).

% Caso 1: A próxima posição está vazia.
handle_next_position(Board, [Row, Col], [NextRow, NextCol], Player, _, _, NewBoard) :-
    cell_player(Board, NextRow, NextCol, empty),  % Verifica se a próxima célula está vazia.
    place_marble(Board, [NextRow, NextCol], Player, TempBoard),  % Coloca a peça na próxima célula.
    clear_cell(TempBoard, [Row, Col], NewBoard).  % Limpa a célula atual.

% Caso 2: A próxima posição está ocupada (empurra as peças para frente).
handle_next_position(Board, [Row, Col], [NextRow, NextCol], Player, DeltaRow, DeltaCol, NewBoard) :-
    cell_player(Board, NextRow, NextCol, NextPlayer),
    NextPlayer \= empty,  % A próxima célula contém uma peça.
    move_marble_to_next(Board, [NextRow, NextCol], DeltaRow, DeltaCol, TempBoard),  % Move a peça ocupante.
    place_marble(TempBoard, [NextRow, NextCol], Player, TempBoard2),  % Coloca a peça atual na próxima célula.
    clear_cell(TempBoard2, [Row, Col], NewBoard).  % Limpa a célula atual.

% Coloca uma peça em uma célula específica do tabuleiro.
place_marble(Board, [Row, Col], Player, NewBoard) :-
    nth1(Row, Board, OldRow),  % Obtém a linha correspondente.
    replace_in_list(Col, OldRow, Player, NewRow),  % Substitui o valor da célula na linha.
    replace_in_list(Row, Board, NewRow, NewBoard).  % Substitui a linha no tabuleiro.

% Remove a peça de uma célula específica do tabuleiro.
clear_cell(Board, [Row, Col], NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_in_list(Col, OldRow, empty, NewRow),  % Substitui a célula pela célula vazia.
    replace_in_list(Row, Board, NewRow, NewBoard).

% Substitui um elemento em uma lista por outro, mantendo o restante.
replace_in_list(Index, List, Element, NewList) :-
    nth1(Index, List, _, Rest),  % Divide a lista antes e depois do índice.
    nth1(Index, NewList, Element, Rest).  % Insere o novo elemento na posição.

% Verifica movimentos válidos para o próximo estado do jogo.
valid_moves_check(NewBoard, CurrentPlayer, _NextPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty, [], [NewBoard, CurrentPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty]).
valid_moves_check(NewBoard, _CurrentPlayer, NextPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty, NextPlayerMoves, [NewBoard, NextPlayer, Player1Type, Player2Type, Rules, Player1Name, Player2Name, Difficulty]) :-
    NextPlayerMoves \= [].  % Verifica se existem movimentos válidos.

% Determina o vencedor do jogo com base nas pontuações finais.
check_winner([], [], OScore, XScore, 'o') :- OScore > XScore.
check_winner([], [], OScore, XScore, 'x') :- XScore > OScore.
check_winner([], [], OScore, XScore, 'draw') :- OScore =:= XScore.  % Empate.

% Simula um movimento e calcula o valor do estado resultante.
simulate_move(GameState, Move, Value) :-
    move(GameState, Move, NewGameState),  % Realiza o movimento.
    NewGameState = [_, CurrentPlayer, _, _, _, _, _, _],
    switch_player(CurrentPlayer, Player),  % Troca o jogador.
    value(NewGameState, Player, Value).  % Calcula o valor do estado para o próximo jogador.
