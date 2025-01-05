% board.pl

% Inclui as bibliotecas necessárias para geração de números aleatórios e manipulação de listas.
:- use_module(library(random)).
:- use_module(library(lists)).

% Gera as peças iniciais do jogo (marbles) de forma aleatória e válida.
generate_marbles(Marbles) :-
    repeat,  % Tenta repetidamente gerar até satisfazer as condições.
    generate_marbles_aux(12, 12, [], Marbles),  % Gera 12 peças de cada tipo ('o' e 'x').
    check_circular(Marbles),  % Verifica se a configuração não possui início e fim iguais.
    !.  % Para após gerar uma configuração válida.

% Função auxiliar para gerar as peças.
% Termina quando não há mais peças a adicionar.
generate_marbles_aux(0, 0, Acc, Marbles) :-
    reverse(Acc, Marbles).  % Reverte a lista acumulada para obter a ordem final.

% Continua gerando as peças enquanto houver peças restantes ('o' ou 'x').
generate_marbles_aux(OCount, XCount, Acc, Marbles) :-
    valid_choices(OCount, XCount, Acc, Choices),  % Obtém as escolhas válidas para a próxima peça.
    random_member(Choice, Choices),  % Escolhe aleatoriamente uma peça válida.
    update_counts(Choice, OCount, XCount, NewOCount, NewXCount),  % Atualiza os contadores de peças restantes.
    generate_marbles_aux(NewOCount, NewXCount, [Choice | Acc], Marbles).  % Adiciona a peça escolhida e continua.

% Determina as escolhas válidas para a próxima peça ('o' ou 'x').
valid_choices(OCount, XCount, Acc, Choices) :-
    findall(o, (OCount > 0, can_place(o, Acc)), OList),  % Lista de escolhas válidas para 'o'.
    findall(x, (XCount > 0, can_place(x, Acc)), XList),  % Lista de escolhas válidas para 'x'.
    append(OList, XList, Choices).  % Combina as listas de 'o' e 'x'.

% Verifica se uma peça pode ser colocada na posição atual, com base no estado do acumulador.
can_place(_, []).  % Sempre pode colocar a primeira peça.
can_place(Marble, [H | _]) :-
    Marble \= H.  % Não permite que a peça seja igual à anterior.
can_place(Marble, [H1, H2 | _]) :-
    Marble = H1,  % Permite se a peça for igual à anterior...
    Marble \= H2.  % ...mas não igual à penúltima.
can_place(Marble, [H1 | _]) :-
    Marble \= H1.  % Caso geral: a peça não deve ser igual à anterior.

% Atualiza os contadores de peças restantes ao adicionar uma nova peça.
update_counts(o, OCount, XCount, NewOCount, XCount) :-
    NewOCount is OCount - 1.  % Decrementa o contador de 'o'.
update_counts(x, OCount, XCount, OCount, NewXCount) :-
    NewXCount is XCount - 1.  % Decrementa o contador de 'x'.

% Verifica se a configuração de peças não forma um ciclo (início e fim iguais).
check_circular([Head | Tail]) :-
    last(Tail, Last),  % Obtém a última peça.
    Head \= Last.  % Verifica se a primeira e a última peças são diferentes.

% Gera o tabuleiro inicial com as peças posicionadas de forma válida.
generate_random_board(Board) :-
    generate_marbles(Marbles),  % Gera as peças aleatórias.
    Marbles = [M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24],
    % Posiciona as peças no tabuleiro em um padrão específico.
    Board = [
        [empty, M1, M2, M3, M4, M5, M6, empty],  % Linha superior com peças.
        [M24, empty, empty, empty, empty, empty, empty, M7],  % Laterais.
        [M23, empty, empty, empty, empty, empty, empty, M8],
        [M22, empty, empty, empty, empty, empty, empty, M9],
        [M21, empty, empty, empty, empty, empty, empty, M10],
        [M20, empty, empty, empty, empty, empty, empty, M11],
        [M19, empty, empty, empty, empty, empty, empty, M12],
        [empty, M18, M17, M16, M15, M14, M13, empty]  % Linha inferior com peças.
    ].
