:- use_module(library(lists)).

opposite(red,  blue).
opposite(blue, red).

color(1, blue).
color(2, red).

generateLine(Size, Start, NewLine) :- generateLine(Size, Start, NewLine, 0, []).

generateLine(Size, Start, NewLine, Size, NewLine) :- !.
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

verifyPlayerCell(Board, Player, X/Y) :-
    nth0(Y, Board, Line),
    nth0(X, Line, Cell),
    Cell is Player.

test(Size):-
    generateBoard(Size, Board),
    verifyPlayerCell(Board, 1, 2/2).


test2(Size):-
    generateBoard(Size, Board),
    verifyPlayerCell(Board, 1, 2/2).


% move(Board, Px/Py, Dir/Capt) :-
    