% Utils
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



% Main menu predicate
main_menu :-
    write('Welcome to Mabula!'), nl,
    write('1. Start New Game'), nl,
    write('2. Exit'), nl,
    write('Please enter your choice: '),
    read(Choice),
    handle_choice(Choice).

% Handle user choice
handle_choice(1) :-
    write('Starting a new game...'), nl,
    board(Board),
    display_board(Board),
    %...
    main_menu.
handle_choice(2) :-
    write('Exiting...'), nl.
handle_choice(_) :-
    write('Invalid choice, please try again.'), nl,
    main_menu.
    