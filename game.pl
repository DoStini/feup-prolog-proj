:- use_module(library(lists)).
:- use_module(library(between)).
:- [util].

opposite(red,  blue).
opposite(blue, red).

dir_vector(up, 0/(-1)).
dir_vector(up_right, 1/(-1)).
dir_vector(right, 1/0).
dir_vector(down_right, 1/1).
dir_vector(down, 0/1).
dir_vector(down_left, (-1)/1).
dir_vector(left, (-1)/0).
dir_vector(up_left, (-1)/(-1)).

%% generate_line(+Size, +Color, -NewLine) is det.
%
%  Gets a board row with length equal to the Size given and with alternating colors, 
%  starting with the color given.
%
%  @param Size The size of the line.
%  @param Color The starting color of the line.
%  @param NewLine The created line.
%
generate_line(Size, Color, NewLine) :- generate_line(Size, Color, NewLine, 0, []).

generate_line(Size, _Color, NewLine, Size, NewLine) :- !.
generate_line(Size, Color, NewLine, Acc, Prev) :-
    Acc < Size,
    Elem = [Color],
    append(Prev, Elem, Combined),
    NextAcc is Acc + 1,
    opposite(Color, Opposite),
    generate_line(Size, Opposite, NewLine, NextAcc, Combined).

%% generate_board(+Size, -Board) is det.
%
%  Gets a square board with length equal to the size given.
%
%  @param Size The length and height of the board.
%  @param Board The resulting board.
%
generate_board(Size, Board) :-
    generate_board(Size, Board, blue, 0, []).

generate_board(Size, Board, _Color, Size, Board) :- !.
generate_board(Size, Board, Color, Acc, Prev) :-
    Size > Acc,
    generate_line(Size, Color, Line),
    Elem = [Line],
    append(Prev, Elem, Combined),
    NextAcc is Acc + 1,
    opposite(Color, OppositeColor),
    generate_board(Size, Board, OppositeColor, NextAcc, Combined).

%% board_center(+Board, ?Resx/?ResY) is det.
%
%  Gets the center position of a given Board.
%  True if the position given is at the center of the board.
%
%  @param Board The board.
%  @param ResX/ResY The center of the board.
%
board_center(Board, ResX/ResY) :-
    length(Board, C),
    X is (C - 1) / 2,
    ResX is X,
    ResY is X.

%% dist_sqr(+P1x/+P1y, +P2x/+P2y, -Res) is det.
%
%  Gets the squared distance between two given positions
%
%  @param P1x/P1y The first position.
%  @param P2x/P2y The second position.
%  @param Res The squared distance.
%
dist_sqr(P1x/P1y, P2x/P2y, Res) :-
    Res is (P2y - P1y) ** 2 + (P2x - P1x) ** 2.

%% dist_inc(+Board, +CurrX/CurrY, +Tx/+Ty) is det.
%
%  Checks if the distance to the center increases from one position to the other.
%  True if target position is farther from the center than the current position.
%
%  @param Board The game board.
%  @param CurrX/Curry The current position.
%  @param Tx/Ty The target position.
%
dist_inc(Board, CurrX/CurrY, Tx/Ty) :- 
    board_center(Board, Cx/Cy),
    dist_sqr(CurrX/CurrY, Cx/Cy, Current),
    dist_sqr(Tx/Ty, Cx/Cy, Target),
    Target > Current.

%% check_bounds(+Size, -X/-Y) is nondet. %1
%% check_bounds(+Size, +X/+Y) is det.    %2
%
%  %1. Gets all positions of the Board given X or Y or neither.
%  %2. Checks if the given position is in the board, if so returns true.
%
%  @param Size The game board size.
%  @param X/Y The position in the game board.
%
check_bounds(Size, X/Y) :-
    Max is Size - 1,
    between(0, Max, Y),
    between(0, Max, X).

%% cell_player(+Board/+Size/?Player, ?X/?Y) is nondet. % 1
%% cell_player(+Board/+Size/?Player, +X/+Y) is det.    % 2
%% cell_player(+Board/+Size/+Player, +X/+Y) is det.    % 3
%
%  %1. Gets all positions and the respective player colors.
%  %2. Gets the player color at the given position.
%  %3. True if Player color is at the given position.
%
%  @param Board The game board.
%  @param Size The game board size.
%  @param Player The player color.
%  @param X/Y The position in the board.
%
cell_player(Board/Size/Player, X/Y) :-
    check_bounds(Size, X/Y),
    nth0(Y, Board, Line),
    nth0(X, Line, Player).

%% cell_empty(+GameState, +X/+Y) is det.
%
%  True if the given position is empty.
%
%  @param Board The game board.
%  @param X/Y The position in the board.
%
cell_empty(Board/Size/_, X/Y) :-
    check_bounds(Size, X/Y),
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

%% value(+GameState, -Value) is det.
%
%  Calculates the negated advantage a player has.
%
%  @param GameState The game state Board/Size/Player
%  @param Value The negated advantage
%
value(Board/Size/Player, Value) :-
    color_diff(Board, Player, Acc),
    opposite(Player, Opposite),
    valid_moves(Board/Size/Player, PlayerMoves),
    valid_moves(Board/Size/Opposite, OppositeMoves),
    num_conquer_moves(PlayerMoves, PlayerConquer, PlayerNonConquer),
    num_conquer_moves(OppositeMoves, OppositeConquer, OppositeNonConquer),
    TotalPlayer is (Size * Size) / 2 * 6,
    ConquerPoints is PlayerConquer / TotalPlayer,
    NonConquerPoints is PlayerNonConquer / TotalPlayer,
    OppositeConquerPoints is OppositeConquer / TotalPlayer,
    OppositeNonConquerPoints is OppositeNonConquer / TotalPlayer,
    Value is -(Acc + ConquerPoints + NonConquerPoints * 0.1 - OppositeConquerPoints * 2 - OppositeNonConquerPoints * 0.3).

%% num_conquer_moves(+Moves, -Conquer, -NonConquer) is det.
%
%  Gets the number of conquering and non-conquering moves from a list of valid moves
%
%  @param Moves is a list of Moves (Px/Py/Dir/Conquer)
%  @param Conquer is the number of conquering moves
%  @param NonConquer is the number of non-conquering moves
%
num_conquer_moves(Moves, Conquer, NonConquer) :-
    num_conquer_moves(Moves, Conquer, NonConquer, 0, 0).

num_conquer_moves([], Conquer, NonConquer, Conquer, NonConquer) :- !.
num_conquer_moves([_/_/_/true|Moves], Conquer, NonConquer, ConquerAcc, NonConquerAcc) :-
    NewConquer is ConquerAcc + 1,
    num_conquer_moves(Moves, Conquer, NonConquer, NewConquer, NonConquerAcc).
num_conquer_moves([_/_/_/false|Moves], Conquer, NonConquer, ConquerAcc, NonConquerAcc) :-
    NewNonConquer is NonConquerAcc + 1,
    num_conquer_moves(Moves, Conquer, NonConquer, ConquerAcc, NewNonConquer).

%% color_diff(+Board, +Player, -Value) is det.
%
%  Gets the difference of player pieces in relation to the given player.
%
%  @param Board The game board.
%  @param Player The color of the player being evaluated.
%  @param Value The number of Player - OtherPlayer pieces.
%
color_diff(Board, red, Value) :-
    colors_board(Board, Blue, Red),
    Value is Red - Blue.

color_diff(Board, blue, Value) :-
    colors_board(Board, Blue, Red),
    Value is Blue - Red.

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

%% valid_moves(+GameState, -ListOfMoves) is det.
%  
%  Gets a list of valid moves for a given game state.
%
%  @param GameState The current state of the game Board/Size/Player.
%  @param ListOfMoves The list of valid moves.
%
valid_moves(Board/Size/Player, ListOfMoves) :-
    findall(Px/Py/Dir/Conquer, ( can_move(Board/Size/Player, Px/Py/Dir/Conquer, _)), ListOfMoves).

%% can_move(+GameState, +Move, ?TargetX/?TargetY) is det. %1
%% can_move(+GameState, -Move, -TargetX/-TargetY) is nondet. %2
%  
%  %1. Checks if the given move is valid, and returns the ending position.
%  %2. Returns a possible move, given the game state.
%
%  @param GameState The current state of the game Board/Size/Player.
%  @param Move The move Px/Py/Dir/Conquer.
%  @param TargetX/TargetY The ending position.
%
can_move(Board/Size/Player, Px/Py/Dir/Conquer, TargetX/TargetY) :-
    cell_player(Board/Size/Player, Px/Py),
    dir_vector(Dir, DirX/DirY),
    NewX is Px + DirX,
    NewY is Py + DirY,
    can_move(Board/Size/Player, Px/Py/DirX/DirY/Conquer, TargetX/TargetY, NewX/NewY).

can_move(Board/Size/Player, Px/Py/DirX/DirY/false, TargetX/TargetY, AccX/AccY) :-
    can_move_non_conquer(Board/Size/Player, Px/Py/DirX/DirY, TargetX/TargetY, AccX/AccY), !.
can_move(Board/Size/Player, Px/Py/DirX/DirY/true, TargetX/TargetY, AccX/AccY) :-
    can_move_conquer(Board/Size/Player, Px/Py/DirX/DirY, TargetX/TargetY, AccX/AccY), !.

%% can_move_non_conquer(+GameState, +Move, -TargetX/-TargetY, +AccX/+AccY) is det.
%  
%  Checks if the given move is non conquering and returns the ending position.
%
%  @param GameState The current state of the game Board/Size/Player.
%  @param Move The move Px/Py/Dir/Conquer.
%  @param TargetX/TargetY The ending position.
%  @param AccX/AccY The current position.
%
can_move_non_conquer(Board/Size/Player, Px/Py/_/_, AccX/AccY, AccX/AccY) :-
    cell_empty(Board/Size/Player, AccX/AccY),
    dist_inc(Board, Px/Py, AccX/AccY).

%% can_move_conquer(+GameState, +Move, -TargetX/-TargetY, +PrevX/+PrevY) is det.
%  
%  Checks if the given move is conquering and returns the ending position.
%  Iterates through the board in the given direction until a cell isn't empty or
%  an enemy cell has been encountered.
%
%  @param GameState The current state of the game Board/Size/Player.
%  @param Move The move Px/Py/Dir/Conquer.
%  @param TargetX/TargetY The ending position.
%  @param PrevX/PrevY The current position.
%
can_move_conquer(Board/Size/Player, Px/Py/_/_, NewX/NewY, NewX/NewY) :-
    opposite(Player, Enemy),
    cell_player(Board/Size/Enemy, NewX/NewY),
    \+ dist_inc(Board, Px/Py, NewX/NewY), !.
can_move_conquer(Board/Size/Player, Px/Py/DirX/DirY, TargetX/TargetY, PrevX/PrevY) :-
    cell_empty(Board/Size/Player, PrevX/PrevY),
    NewX is PrevX + DirX,
    NewY is PrevY + DirY,
    can_move_conquer(Board/Size/Player, Px/Py/DirX/DirY, TargetX/TargetY, NewX/NewY).

%% move(+GameState, +Move, -NewGameState) is det.
%  
%  Executes a valid move and returns a new game state.
%
%  @param GameState The current state of the game Board/Size/Player.
%  @param Move The move Px/Py/Dir/Conquer.
%  @param NewGameState The resulting game state.
%
move(Board/Size/Player, Px/Py/Dir/Conquer, NewBoard/Size/Next) :-
    can_move(Board/Size/Player, Px/Py/Dir/Conquer, TargetX/TargetY),
    replace_current(Board, Player, Px/Py, TargetX/TargetY, NewBoard),
    opposite(Player, Next).

%% replace_current(+Board, +Player, +Px/+Py, +TargetX/+TargetY, -NewBoard) is det.
%  
%  Replaces in the board Px/Py with empty color and TargetX/TargetY with Player color.
%  Effectively moves a Player piece from Px/Py to TargetX/TargetY.
%
%  @param Board The game board.
%  @param Player The player color to replace in TargetX/TargetY
%  @param Px/Py The position to empty.
%  @param TargetX/TargetY The position to place Player.
%  @param NewBoard The resulting game board.
%
replace_current(Board, Player, Px/Py, TargetX/TargetY, NewBoard) :-
    nth0(Py, Board, CurrentLine),
    replace(CurrentLine, Px, empty, NewCurrentLine),
    replace(Board, Py, NewCurrentLine, TempBoard),

    nth0(TargetY, TempBoard, TargetLine),
    replace(TargetLine, TargetX, Player, NewTargetLine),
    replace(TempBoard, TargetY, NewTargetLine, NewBoard).
