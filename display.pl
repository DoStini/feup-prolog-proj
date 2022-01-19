cls :- write('\33\[2J').

playerChar(red, 'X').
playerChar(blue, 'O').
playerChar(empty, ' ').

drawCell(Player) :-
    playerChar(Player, Char),
    put_char(Char).

drawLine([]) :-
    put_char('\n').

drawLine([Elem | Line]) :-
    drawCell(Elem),
    put_char(' '),
    drawLine(Line).

drawGame(Board) :-
    format("~s", ["\n####################################################\n"]),
    drawBoard(Board),
    format("~s", ["####################################################\n"]).

drawBoard([]).

drawBoard([Line | RemBoard]) :-
    drawLine(Line),
    drawBoard(RemBoard).

display_game(Board/Player) :-
    nl,
    playerChar(Player, Char),
    drawGame(Board),
    format("~s~a~s~a~s", ["Player ", Player, " (", Char  ,"), it is your turn to play!\n"]),
    nl.
