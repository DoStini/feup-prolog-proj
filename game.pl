:- use_module(library(lists)).

opposite(red,  blue).
opposite(blue, red).

color(0, none).
color(1, blue).
color(2, red).

/*
 X,Y | X,Y
 0,0 | 1,0
 0,1 | 1,1
*/

getDir(up, 0/(-1)).
getDir(up_right, 1/(-1)).
getDir(right, 1/0).
getDir(down_right, 1/1).
getDir(down, 0/1).
getDir(down_left, (-1)/1).
getDir(left, (-1)/0).
getDir(up_left, (-1)/(-1)).

generateLine(Size, Start, NewLine) :- generateLine(Size, Start, NewLine, 0, []).

generateLine(Size, _Start, NewLine, Size, NewLine) :- !.
generateLine(Size, Start, NewLine, Acc, Prev) :-
    Acc < Size,
    ColorIdx is mod(mod(Acc, 2) + Start, 2) + 1,
    % ColorIdx is mod(Acc, 2) + Start + 1,
    color(ColorIdx, Color),
    Elem = [Color],
    append(Prev, Elem, Combined),
    NextAcc is Acc + 1,
    generateLine(Size, Start, NewLine, NextAcc, Combined).

generateBoard(Size, Board) :-
    generateBoard(Size, Board, 0, []).

generateBoard(Size, Board, Size, Board) :- !.

generateBoard(Size, Board, Acc, Prev) :-
    Size > Acc,
    Start is mod(Acc, 2),
    generateLine(Size, Start, Line),
    Elem = [Line],
    append(Prev, Elem, Combined),
    NextAcc is Acc + 1,
    generateBoard(Size, Board, NextAcc, Combined).

boardCenter(Board, ResX/ResY) :-
    length(Board, C),
    X is (C - 1) / 2,
    ResX is X,
    ResY is X.

distSqr(P1x/P1y, P2x/P2y, Res) :-
    Res is (P2y - P1y) ** 2 + (P2x - P1x) ** 2.

distInc(Board, CurrX/CurrY, Tx/Ty) :- 
    boardCenter(Board, Cx/Cy),
    distSqr(CurrX/CurrY, Cx/Cy, Current),
    distSqr(Tx/Ty, Cx/Cy, Target),
    Target > Current.

checkBounds(Board, X/Y) :-
    checkBounds(Board, X/Y,X/0).

checkBounds(Board, X/Y,X/Y) :-
    nth0(Y, Board, Line),
    checkLine(Line, X, 0).

checkBounds(Board, X/Y, X/AccY) :-
    length(Board, L),
    NextY is AccY + 1,
    NextY < L,
    checkBounds(Board, X/Y, X/NextY).

checkLine(_Line, X, X).

checkLine(Line, X, AccX) :-
    length(Line, L),
    NextX is AccX + 1,
    NextX < L,
    checkLine(Line, X, NextX).

verifyPlayerCell(Board, Player, X/Y) :-
    checkBounds(Board, X/Y),
    nth0(Y, Board, Line),
    nth0(X, Line, Player).

verifyEmpty(Board, X/Y) :-
    checkBounds(Board, X/Y),
    nth0(Y, Board, Line),
    nth0(X, Line, empty).


entryMove(Board, Player, Px/Py, Dir, Conquer) :-
    verifyPlayerCell(Board, Player, Px/Py),
    move(Board, Player, Px/Py, Dir, Conquer).

/** Non capturing move */
move(Board, _Player, Px/Py, Dir, false) :-
    getDir(Dir, DirX/DirY),
    NewX is Px + DirX,
    NewY is Py + DirY,
    verifyEmpty(Board, NewX/NewY),
    distInc(Board, Px/Py, NewX/NewY).

/** Capturing move */
move(Board, Player, Px/Py, Dir, true) :- 
    move(Board, Player, Px/Py, Dir, true, Px/Py).

move(Board, Player, Px/Py, Dir, true, PrevX/PrevY) :-
    getDir(Dir, DirX/DirY),
    NewX is PrevX + DirX,
    NewY is PrevY + DirY,
    moveConquer(Board, Player, Dir, Px/Py, NewX/NewY).

moveConquer(Board, Player, _Dir, Px/Py, NewX/NewY) :-
    opposite(Player, Enemy),
    verifyPlayerCell(Board, Enemy, NewX/NewY),
    \+ distInc(Board, Px/Py, NewX/NewY).

moveConquer(Board, Player, Dir, Px/Py, NewX/NewY) :-
    verifyEmpty(Board, NewX/NewY),
    move(Board, Player, Px/Py, Dir, true, NewX/NewY).
