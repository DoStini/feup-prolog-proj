:- use_module(library(lists)).

generateLine(Size, Start, NewLine) :- generateLine(Size, Start, NewLine, 0, []).

generateLine(Size, Start, NewLine, Size, NewLine) :- !.
generateLine(Size, Start, NewLine, Acc, Prev) :-
    Acc < Size,
    Color is mod(mod(Acc, 2) + Start, 2),
    format('~*t', [Color]),
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

boardCenter(Board, Res) :-
    length(Board, C),
    X is (C - 1) / 2,
    Res = [X, X].

distSqr(P1, P2, Res) :-
    nth0(0, P1, P1x),
    nth0(1, P1, P1y),
    nth0(0, P2, P2x),
    nth0(1, P2, P2y),
    Res is (P2y - P1y) ** 2 + (P2x - P1x) ** 2.

distInc(Board, CurrentPos, TargetPos) :- 
    boardCenter(Board, Center),
    distSqr(CurrentPos, Center, Current),
    distSqr(TargetPos, Center, Target),
    nth0(0, Center, X),
    format('~2f ~2f ~2f', [Current, Target, X]),
    Target > Current.
