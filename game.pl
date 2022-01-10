:- use_module(library(lists)).

opposite(red,  blue).
opposite(blue, red).

getDir(up, 0/(-1)).
getDir(up_right, 1/(-1)).
getDir(right, 1/0).
getDir(down_right, 1/1).
getDir(down, 0/1).
getDir(down_left, (-1)/1).
getDir(left, (-1)/0).
getDir(up_left, (-1)/(-1)).

testMultiple(T, Goal, Times) :- testMultiple(T, Goal, 0, 0, Times).

testMultiple(T, _G, Acc, Times, Times) :- T is Acc div Times, !.
testMultiple(T, Goal, Acc, AccTime, Times) :-
    AccTime < Times,
    test(T2, Goal),
    T1 is Acc + T2,
    Acc1 is AccTime +  1,
    testMultiple(T, Goal, T1, Acc1, Times).

testMultipleBounds(T, Size, Times) :-
    generateBoard(Size, B),
    S is Size - 1,
    testMultiple(T, checkBounds(B, S/S), Times).

test(T, Goal) :-
    statistics(walltime,[Start,_]),
    Goal,
    statistics(walltime,[Stop,_]),
    T is Stop - Start.

%% generateLine(+Size, +Color, -NewLine) is det.
%
%  Gets a board row with length equal to the Size given and with alternating colors, 
%  starting with the color given.
%
%  @param Size The size of the line.
%  @param Color The starting color of the line.
%  @param NewLine The created line.
%
generateLine(Size, Color, NewLine) :- generateLine(Size, Color, NewLine, 0, []).

generateLine(Size, _Color, NewLine, Size, NewLine) :- !.
generateLine(Size, Color, NewLine, Acc, Prev) :-
    Acc < Size,
    Elem = [Color],
    append(Prev, Elem, Combined),
    NextAcc is Acc + 1,
    opposite(Color, Opposite),
    generateLine(Size, Opposite, NewLine, NextAcc, Combined).

%% generateBoard(+Size, -Board) is det.
%
%  Gets a square board with length equal to the size given.
%
%  @param Size The length and height of the board.
%  @param Board The resulting board.
%
generateBoard(Size, Board) :-
    generateBoard(Size, Board, blue, 0, []).

generateBoard(Size, Board, _Color, Size, Board) :- !.
generateBoard(Size, Board, Color, Acc, Prev) :-
    Size > Acc,
    generateLine(Size, Color, Line),
    Elem = [Line],
    append(Prev, Elem, Combined),
    NextAcc is Acc + 1,
    opposite(Color, OppositeColor),
    generateBoard(Size, Board, OppositeColor, NextAcc, Combined).

%% boardCenter(+Board, ?Resx/?ResY) is det.
%
%  Gets the center position of a given Board.
%  True if the position given is at the center of the board.
%
%  @param Board The board.
%  @param ResX/ResY The center of the board.
%
boardCenter(Board, ResX/ResY) :-
    length(Board, C),
    X is (C - 1) / 2,
    ResX is X,
    ResY is X.

%% distSqr(+P1x/+P1y, +P2x/+P2y, -Res) is det.
%
%  Gets the squared distance between two given positions
%
%  @param P1x/P1y The first position.
%  @param P2x/P2y The second position.
%  @param Res The squared distance.
%
distSqr(P1x/P1y, P2x/P2y, Res) :-
    Res is (P2y - P1y) ** 2 + (P2x - P1x) ** 2.

%% distInc(+Board, +CurrX/CurrY, +Tx/+Ty) is det.
%
%  Checks if the distance to the center increases from one position to the other.
%  True if target position is farther from the center than the current position.
%
%  @param Board The game board.
%  @param CurrX/Curry The current position.
%  @param Tx/Ty The target position.
%
distInc(Board, CurrX/CurrY, Tx/Ty) :- 
    boardCenter(Board, Cx/Cy),
    distSqr(CurrX/CurrY, Cx/Cy, Current),
    distSqr(Tx/Ty, Cx/Cy, Target),
    Target > Current.

%% checkBounds(+Board, ?X/?Y) is nondet. %1
%% checkBounds(+Board, +X/+Y) is det.    %2
%
%  %1. Gets all positions of the Board given X or Y or neither. Assumes
%      Board has at least size 1.
%  %2. Checks if the given position is in the board, if so returns true.
%
%  @param Board The game board.
%  @param X/Y The position in the game board.
%
checkBounds(Board, X/Y) :-
    checkBounds(Board, X/Y,X/0).

checkBounds([Line|_], X/Y,X/Y) :-
    checkLine(Line, X, 0).
checkBounds([_|Board], X/Y, X/AccY) :-
    NextY is AccY + 1,
    checkBounds(Board, X/Y, X/NextY).

checkLine([_|_], X, X).
checkLine([_|Line], X, AccX) :-
    NextX is AccX + 1,
    checkLine(Line, X, NextX).

%% verifyPlayerCell(+Board, ?Player, ?X/?Y) is nondet. % 1
%% verifyPlayerCell(+Board, ?Player, +X/+Y) is det.    % 2
%% verifyPlayerCell(+Board, +Player, +X/+Y) is det.    % 3
%
%  %1. Gets all positions and the respective player colors.
%  %2. Gets the player color at the given position.
%  %3. True if Player color is at the given position.
%
%  @param Board The game board.
%  @param Player The player color.
%  @param X/Y The position in the board.
%
verifyPlayerCell(Board, Player, X/Y) :-
    checkBounds(Board, X/Y),
    nth0(Y, Board, Line),
    nth0(X, Line, Player).

%% verifyEmpty(+Board, ?X/?Y) is nondet. % 1
%% verifyEmpty(+Board, +X/+Y) is det.    % 2
%
%  %1. Gets all empty positions.
%  %2. True if the given position is empty.
%
%  @param Board The game board.
%  @param X/Y The position in the board.
%
verifyEmpty(Board, X/Y) :-
    checkBounds(Board, X/Y),
    nth0(Y, Board, Line),
    nth0(X, Line, empty).

entryMove(Board, Player, Px/Py, Dir, Conquer, TargetX/TargetY) :-
    verifyPlayerCell(Board, Player, Px/Py),
    move(Board, Player, Px/Py, Dir, Conquer, TargetX/TargetY).

/** Non capturing move */
move(Board, _Player, Px/Py, Dir, false, TargetX/TargetY) :-
    getDir(Dir, DirX/DirY),
    NewX is Px + DirX,
    NewY is Py + DirY,
    verifyEmpty(Board, NewX/NewY),
    distInc(Board, Px/Py, NewX/NewY),
    TargetX is NewX,
    TargetY is NewY.

/** Capturing move */
move(Board, Player, Px/Py, Dir, true, TargetX/TargetY) :- 
    move(Board, Player, Px/Py, Dir, true, Px/Py, TargetX/TargetY).

move(Board, Player, Px/Py, Dir, true, PrevX/PrevY, TargetX/TargetY) :-
    getDir(Dir, DirX/DirY),
    NewX is PrevX + DirX,
    NewY is PrevY + DirY,
    moveConquer(Board, Player, Dir, Px/Py, NewX/NewY, TargetX/TargetY).

moveConquer(Board, Player, _Dir, Px/Py, NewX/NewY, NewX/NewY) :-
    opposite(Player, Enemy),
    verifyPlayerCell(Board, Enemy, NewX/NewY),
    \+ distInc(Board, Px/Py, NewX/NewY).

moveConquer(Board, Player, Dir, Px/Py, NewX/NewY, TargetX/TargetY) :-
    verifyEmpty(Board, NewX/NewY),
    move(Board, Player, Px/Py, Dir, true, NewX/NewY, TargetX/TargetY).

% applyMove(Board/P1/P2, Px/Py, Dir, Conquer, NewBoard/NewP1/NewP2) :-
%     entryMove(Board, Player, Px/Py, Dir, Conquer, TargetX/TargetY),
