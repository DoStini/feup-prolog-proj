cls :- write('\33\[2J').

drawCell(red) :-
    put_char('X').

drawCell(blue) :-
    put_char('O').

drawCell(empty) :-
    put_char(' ').


drawLine([]) :-
    put_char('\n').

drawLine([Elem | Line]) :-
    drawCell(Elem),
    put_char(' '),
    drawLine(Line).


drawBoard([]).

drawBoard([Line | RemBoard]) :-
    drawLine(Line),
    drawBoard(RemBoard).
