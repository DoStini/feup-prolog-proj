:- use_module(library(lists)).
:- [util].

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
%  %1. Gets all positions of the Board given X or Y or neither.
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
verifyPlayerCell(Board/Player, X/Y) :-
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

%% end_game(+Board, -Player) is det.
%
%  True if game has ended.
%
%  @param Board The game board.
%  @param Player The color of the player who won
%
end_game(Board, Player) :-
    colors_board(Board, Blue, Red),
    end_game(Player, Blue, Red).

end_game(Player,_Blue,0) :-
    Player = blue, !.
end_game(Player,0,_Red) :-
    Player = red, !.

%% colors_board(+Board, -Blue, -Red) is det.
%
%  Gets the number of pieces in the board of each color.
%
%  @param Board The game board.
%  @param Blue The number of blue pieces.
%  @param Red The number of red pieces.
%
colors_board(Board, Blue, Red) :-
    colors_board(Board, Blue, Red, 0, 0), !.

colors_board([], Blue, Red, Blue, Red) :- !.
colors_board([Line|Board], Blue, Red, BSum, RSum) :-
    colors_line(Line, B, R, 0, 0),
    BSum1 is BSum + B,
    RSum1 is RSum + R,
    colors_board(Board, Blue, Red, BSum1, RSum1).

safe_div(_, 0, 0) :- !.
safe_div(Num, Div, Res) :-
    Res is Num / Div, !.

%% value(+Board, +Player, -Value) is det.
%
%  Calculates the negated advantage a player has.
%
%  @param Board The game board.
%  @param Player The one playing
%  @param Value The negated advantage
%
value(Board, Player, Value) :-
    color_diff(Board, Player, Acc),
    opposite(Player, Opposite),
    valid_moves(Board/Player, PlayerMoves),
    valid_moves(Board/Opposite, OppositeMoves),
    conquerLength(PlayerMoves, PlayerConquer, PlayerNonConquer),
    conquerLength(OppositeMoves, OppositeConquer, OppositeNonConquer),
    TotalPlayer is PlayerNonConquer + PlayerConquer,
    TotalOpposite is OppositeNonConquer + OppositeConquer,
    safe_div(TotalPlayer, PlayerConquer, ConquerPoints),
    safe_div(TotalPlayer, PlayerNonConquer, NonConquerPoints),
    safe_div(TotalOpposite, OppositeConquer, OppositeConquerPoints),
    safe_div(TotalOpposite, OppositeNonConquer, OppositeNonConquerPoints),
    Value is -(Acc + ConquerPoints + NonConquerPoints * 0.5 - OppositeConquerPoints * 1.5 - OppositeNonConquerPoints * 0.9).

conquerLength(Moves, Conquer, NonConquer) :-
    conquerLength(Moves, Conquer, NonConquer, 0, 0).

conquerLength([], Conquer, NonConquer, Conquer, NonConquer) :- !.
conquerLength([_/_/_/true|Moves], Conquer, NonConquer, ConquerAcc, NonConquerAcc) :-
    NewConquer is ConquerAcc + 1,
    conquerLength(Moves, Conquer, NonConquer, NewConquer, NonConquerAcc).
conquerLength([_/_/_/false|Moves], Conquer, NonConquer, ConquerAcc, NonConquerAcc) :-
    NewNonConquer is NonConquerAcc + 1,
    conquerLength(Moves, Conquer, NonConquer, ConquerAcc, NewNonConquer).

color_diff(Board, red, Value) :-
    colors_board(Board, Blue, Red),
    Value is Red - Blue.

color_diff(Board, blue, Value) :-
    colors_board(Board, Blue, Red),
    Value is Blue - Red.

%% colors_line(+Line, -Blue, -Red) is det.
%
%  Gets the number of pieces in a board row of each color.
%
%  @param Line The game board row.
%  @param Blue The number of blue pieces.
%  @param Red The number of red pieces.
%  @param BSum The accumulative number of blue pieces.
%  @param BSum The accumulative number of red pieces.
%
colors_line([], Blue, Red, Blue, Red) :- !.
colors_line([blue|Line], Blue, Red, BSum, RSum) :-
    BSum1 is BSum + 1,
    colors_line(Line, Blue, Red, BSum1, RSum).
colors_line([red|Line], Blue, Red, BSum, RSum) :-
    RSum1 is RSum + 1,
    colors_line(Line, Blue, Red, BSum, RSum1).
colors_line([_|Line], Blue, Red, BSum, RSum) :-
    colors_line(Line, Blue, Red, BSum, RSum).

%% valid_moves(+Board/+Player, -ListOfMoves) is det.
%  
%  Gets a list of valid moves for a given game state.
%
valid_moves(Board/Player, ListOfMoves) :-
    findall(Px/Py/Dir/Conquer, can_move(Board/Player, Px/Py/Dir/Conquer, _), ListOfMoves).

%% can_move(+Board, ?Player, ?Px/?Py, )
can_move(Board/Player, Px/Py/Dir/Conquer, TargetX/TargetY) :-
    verifyPlayerCell(Board/Player, Px/Py),
    move_inner(Board/Player, Px/Py/Dir/Conquer, TargetX/TargetY).

/** Non capturing move */
move_inner(Board/_Player, Px/Py/Dir/false, TargetX/TargetY) :-
    getDir(Dir, DirX/DirY),
    NewX is Px + DirX,
    NewY is Py + DirY,
    verifyEmpty(Board, NewX/NewY),
    distInc(Board, Px/Py, NewX/NewY),
    TargetX is NewX,
    TargetY is NewY.

/** Capturing move */
move_inner(Board/Player, Px/Py/Dir/true, TargetX/TargetY) :- 
    move_inner(Board/Player, Px/Py/Dir/true, Px/Py, TargetX/TargetY).

move_inner(Board/Player, Px/Py/Dir/true, PrevX/PrevY, TargetX/TargetY) :-
    getDir(Dir, DirX/DirY),
    NewX is PrevX + DirX,
    NewY is PrevY + DirY,
    move_conquer(Board/Player, Px/Py/Dir, NewX/NewY, TargetX/TargetY).

move_conquer(Board/Player, Px/Py/_Dir, NewX/NewY, NewX/NewY) :-
    opposite(Player, Enemy),
    verifyPlayerCell(Board/Enemy, NewX/NewY),
    \+ distInc(Board, Px/Py, NewX/NewY).

move_conquer(Board/Player, Px/Py/Dir, NewX/NewY, TargetX/TargetY) :-
    verifyEmpty(Board, NewX/NewY),
    move_inner(Board/Player, Px/Py/Dir/true, NewX/NewY, TargetX/TargetY).

move(Board/Player, Px/Py/Dir/Conquer, NewBoard/Next) :-
    can_move(Board/Player, Px/Py/Dir/Conquer, TargetX/TargetY),
    replaceCurrent(Board, Player, Px/Py, TargetX/TargetY, NewBoard),
    opposite(Player, Next).

replaceCurrent(Board, Player, Px/Py, TargetX/TargetY, NewBoard) :-
    nth0(Py, Board, CurrentLine),
    replace(CurrentLine, Px, empty, NewCurrentLine),
    replace(Board, Py, NewCurrentLine, TempBoard),

    nth0(TargetY, TempBoard, TargetLine),
    replace(TargetLine, TargetX, Player, NewTargetLine),
    replace(TempBoard, TargetY, NewTargetLine, NewBoard).
