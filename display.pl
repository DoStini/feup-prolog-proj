cls :- write('\33\[2J').

playerChar(red, 'X').
playerChar(blue, 'O').
playerChar(empty, ' ').

drawCell(Player) :-
    playerChar(Player, Char),
    put_char(Char).

drawLine(Line) :-
    put_char('|'),
    drawLineInner(Line).

drawLineInner([]).
drawLineInner([Elem | Line]) :-
    put_char(' '),
    drawCell(Elem),
    put_char(' '),
    put_char('|'),
    drawLineInner(Line).

drawNumbers(Board) :-
    put_char(' '),
    drawNumbers(Board, 0).

drawNumbers([], _) :- nl.
drawNumbers([_|Board], I) :-
    put_char(' '),
    put_char(' '),
    put_char(' '),
    write(I),
    NextI is I + 1,
    drawNumbers(Board, NextI).

drawGrid(Line) :-
    put_char(' '),
    put_char(' '),
    put_char('+'),
    drawGridInner(Line).

drawGridInner([]) :- nl.
drawGridInner([_|Line]) :-
    put_char('-'),
    put_char('-'),
    put_char('-'),
    put_char('+'),
    drawGridInner(Line).

drawGame(Board) :-
    format("~s", ["\n####################################################\n"]),
    drawNumbers(Board),
    drawBoard(Board),
    drawNumbers(Board),
    format("~s", ["####################################################\n"]).

drawBoard(Board) :-
    drawGrid(Board),
    drawBoard(Board, 0).

drawBoard([], _).
drawBoard([Line | RemBoard], LineNo) :-
    write(LineNo),
    put_char(' '),
    drawLine(Line),
    put_char(' '),
    write(LineNo),
    nl,
    drawGrid(Line),
    LineNext is LineNo + 1,
    drawBoard(RemBoard, LineNext).

display_game(Board/Player) :-
    nl,
    playerChar(Player, Char),
    drawGame(Board),
    format("~s~a~s~a~s", ["Player ", Player, " (", Char  ,"), it is your turn to play!\n"]),
    nl.
