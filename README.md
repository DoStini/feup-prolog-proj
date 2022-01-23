# PFL Project 2 - Class 05 Group 01

#### The game **Zola** was developed by **group 01** from **class 05**:

| Student                       | Number    | Contribution |
| ----------------------------- | --------- | ------------ |
| André Júlio Moreira           | 201904721 | 33.3(3) %    |
| Mário Manuel Seixas Travassos | 201905871 | 33.3(3) %    |
| Nuno Miguel da Silva Alves    | 201908250 | 33.3(3) %    |

## Installation and Execution

### Linux Specific Software (Optional)

After installing **SICSTUS** Linux version, we can notice it is very buggy and unreliable. You can use [rlwrap](https://github.com/hanslub42/rlwrap), which wraps **SICSTUS** and creates a more clean interaction between the user and **SICSTUS**. After installing it, run `rlwrap sicstus`.

### Running the game

After initializing **SICSTUS**, we need to load the game using `[main].` and then start the game using `play.`.

### Windows alternative

After opening **SICSTUS**, select File > consult > main.pl. You can then run `play.`

## Game Description

Zola is a two-player board game that is played on a checkerboard, filled with a checkered pattern of Red and Blue checkers, each of which represents a player.

Both players move one of their checkers in each of their turns. If a player has a possible move they can make, they must do so. If they have no more moves available, they must skip the current round and wait until they have an available move to make. Due to the nature of the game, at least one of the two players will have a move available at any given time.

The objective of this game is to capture all the enemy checkers. Draws never occur in this game.

### Moves

There are two types of moves in Zola: **Capturing** and **Non-Capturing**.

**Non-Capturing** moves are king-like moves to an adjacent unoccupied square (horizontally, vertically, or diagonally), which must increase the distance to the center of the board.

**Capturing** moves are queen-like moves along a straight line (horizontal, vertical or diagonal), which end at an enemy-occupied square. The enemy piece is removed from the game and replaced with the player checker. This type of move must maintain or decrease the distance to the center of the board.

### Ownership

Zola was made by Mark Steere, a board game designer specialized in designing abstract games.

Official rule sheet: https://www.marksteeregames.com/Zola.pdf

## Game Logic

### Game Representation

The game state is represented as a `GameState`, which is a composition of `Board`, `Size` and `Player`. `Board` is a list of lists of pieces, which is an atom defined as `red`, `blue` or `empty`. `Size` is the length/height of the Board. `Player`, which represents the current player, is identified by their piece's color, `red` or `blue`. The functions that use these can either use `example(GameState)` or `example(Board/Player)` if those values need to be destructured.

The initial `Board` is full and has alternating pieces:

`Board = [[blue,red,blue,red,blue],[red,blue,red,blue,red],[blue,red,blue,red,blue],[red,blue,red,blue,red],[blue,red,blue,red,blue]]`

The initial `Player` is defined by the user.

| State       | GameState                                                           |
| ----------- | ------------------------------------------------------------------- |
| Initial     | `[empty,red,blue],[red,blue,red],[blue,red,blue]]/3/blue`           |
| Second Move | `[[empty,red,blue],[blue,blue,red],[blue,red,blue]]/3/red`          |
| Finished    | `[[empty,empty,empty],[empty,empty,empty],[blue,blue,empty]]/3/red` |

A move is represented as a `Move`, which is a composition of `Px`, `Py`, `Dir`, and `Conquer`. `Px/Py` is the starting position, `Dir` is the direction of the move validated using `dir_vector` or `dir` and `Conquer` which is `true` if it is a conquering move and `false` if it is not.

### Game Interaction

The game visualization predicates are in **display.pl**, and the input predicates are in **input.pl**. When the `play` predicate is called, a menu displays the different options, which allows us to choose to play against a friend, against a computer, or even watch two computers play.

After selecting the type of game, we are prompted with some configuration options. If the player chooses to play with computers, he will be able to choose the difficulty, either an AI that plays randomly or an AI that is smart and evaluates its options. The last step in setting up the game is to choose a game size and the initial player.

All of the input is being validated, and in case any errors happen, the program will ask the user again for a correct input. This is done with the help of `repeat` and a `cut` in the input validation predicate:

```prolog
ask_difficulty(Type):-
    repeat,
    format("~s", ["Choose the AI difficulty:\n 1. Random\n 2. Smart\n"]),
    read(Option),
    askDifficulty(Type, Option), !.
```

For example, when `ask_difficulty(Type, Option)` fails, the program will go back to the repeat statement and will only backtrack from the `ask_difficulty(Type)`, when `ask_difficulty(Type, Option)` succeeds.

This is done in various places from **input validation** to **moves validation**:

```prolog
repeat,
choose_move(GameState, CurPlayerType, Move),
(move(GameState, Move, NextState) ; (format("~s", ["!!INVALID MOVE, TRY AGAIN!!\n"]), fail))
```

The `display_game` predicate takes the `GameState` as a parameter. Its objective is to display the game board and to inform who the current player is. To make the board consistent when its size is bigger than 10, we use `` format("~` t~d~2|", [LineNo]) `` to write the Y numbers so that they are aligned and padded to the left, and `` format("~` t~1|~` t~2+~` t~d~2+", [I]) `` to write the X numbers so that they are padded and only occupy 2 spaces.

To print out the pieces correctly, we use a predicate to convert the atom to a char:

```prolog
player_char(red, 'X').
player_char(blue, 'O').
player_char(empty, ' ').
```

To print out the actual board, the predicate `draw_board` recursively transverses the list of lists and writes the character given by `player_char`.

### Move Execution

To execute a move, according to the rules of the game, it needs to be validated.

First, the piece we are trying to move needs to be verified if it is one of ours, which is validated by `cell_player`.

The player can only move horizontally, vertically, or diagonally. This validation is already being made in the user input since the player chooses a position and a direction.

As already stated in the game description, there are two types of moves, capturing and non-capturing.

In order for a piece to capture an enemy checker, the distance between the center of the board and the target position must decrease or be maintained relative to the previous position. This step can be validated using `\+ dist_inc`. The board needs to be transversed in the chosen direction. In every step, the game verifies what the current cell is. There are 4 possible outcomes:

- Empty cell: The search must go through
- Enemy checker: The search ends and the `can_move` predicate returns the new position, and then we can finally update the board with `replace_current`, returning the new game state in `move`
- Own checker: The search ends, invalidating the move
- Out of bounds: The search ends, invalidating the move

The first `can_move_conquer` predicate verifies if we reached an enemy checker, and if that condition is verified, the predicate backtracks and success, returning the new position.

The second `can_move_conquer` predicate verifies if we are currently in an empty cell, which means we need to continue searching.

If none of these conditions are matched, the predicate fails, indicating an invalid move.

```prolog
can_move_conquer(Board/Size/Player, Px/Py/_/_, NewX/NewY, NewX/NewY) :-
    opposite(Player, Enemy),
    cell_player(Board/Size/Enemy, NewX/NewY),
    \+ dist_inc(Board, Px/Py, NewX/NewY), !.

can_move_conquer(Board/Size/Player, Px/Py/DirX/DirY, TargetX/TargetY, PrevX/PrevY) :-
    cell_empty(Board/Size/Player, PrevX/PrevY),
    NewX is PrevX + DirX,
    NewY is PrevY + DirY,
    can_move_conquer(Board/Size/Player, Px/Py/DirX/DirY, TargetX/TargetY, NewX/NewY).
```

On the other hand, to make a non capturing move, the distance must be increased and the player can only move one unit. In this case, the cell must be empty, so `cell_empty` validates this step, and we only need to check the closest cell in the direction chosen.

```prolog
can_move_non_conquer(Board/Size/Player, Px/Py/_/_, AccX/AccY, AccX/AccY) :-
    cell_empty(Board/Size/Player, AccX/AccY),
    dist_inc(Board, Px/Py, AccX/AccY).
```

If all of the steps are valid, the `GameState` is updated returning a `NewGameState`. When updating the board, the old cell must be emptied and the new one must be replaced with the current player.

```prolog
move(Board/Size/Player, Px/Py/Dir/Conquer, NewBoard/Size/Next) :-
    can_move(Board/Size/Player, Px/Py/Dir/Conquer, TargetX/TargetY),
    replace_current(Board, Player, Px/Py, TargetX/TargetY, NewBoard),
    opposite(Player, Next).
```

The `replace_current` predicate uses an auxiliary predicate to replace a value in a list of lists:

```prolog
replace(List, X, Value, Result) :-
    replace(List, X, Value, Result, [], 0).

replace([], _X, _Value, _Result, _AccRes, _X).

replace([_Elem | Rem], X, Value, Result, AccRes, X) :-
    ElemList = [Value],
    append(AccRes, ElemList, NewRes),
    append(NewRes, Rem, Result).

replace([Elem | Rem], X, Value, Result, AccRes, AccX) :-
    NewX is AccX + 1,
    ElemList = [Elem],
    append(AccRes, ElemList, Combined),
    replace(Rem, X, Value, Result, Combined, NewX).
```

### Game Ending

According to the rules of the game, the game ends when a player loses all of their pieces. This is easily verified by iterating through the board and counting the pieces of each player. Ties do not occur in this game, so we only need to check which of the players has 0, and return the opposite player as the winner. This check is done with `colors_board` to count the pieces and with `end_game`.

### AI

To create an AI, we need to find all the valid moves and then choose the best one. First, we need a `valid_moves` predicate. This predicate uses `findall` and our predicate and `can_move`. For this to work, the predicate passed into `findall` must be able to work without instantiating the `Move` parameter. What `can_move` does is both validate and generate valid positions. This is done in the predicate `cell_player` called within `can_move` which calls `check_bounds` that uses `between`:

```
check_bounds(Size, X/Y) :-
    Max is Size - 1,
    between(0, Max, Y),
    between(0, Max, X).
```

Since the predicate is generating positions, we can use `can_move` to generate the remaining `Move` parameters, `Dir` and `Conquer`. Since the `Dir` values are validated using `dir_vector`, and there is a limited number of clauses, prolog can automatically generate these `Dir` if we use the `dir_vector` predicate, allowing `findall` to generate all possible moves.

After finding all of the valid moves, the AI needs to choose which one it will make. There are two levels of difficulty, one that plays randomly and one that calculates the outcome of every possible move and selects the best one.

The first method uses the predicate `random_member` to select one of the moves from the `ValidMoves` list.

The second one follows a heuristic. It takes into account the difference between pieces and some ratios. These ratios are values divided by `TotalPlayer` which is almost the theoretical max number of moves in a board of size `Size`: `(Size * Size) / 2 * 8` is the max and `(Size * Size) / 2 * 6` is the calculation used. `(Size * Size) / 2` is the number of pieces of one color at the start of the game and `8` is the number of directions. `6` was used instead of `8` because the theoretical max is most likely not reachable in a normal game so this makes the ratios more important. The values used are:

```prolog
ConquerPoints is PlayerConquer / TotalPlayer,
NonConquerPoints is PlayerNonConquer / TotalPlayer * 0.1,
OppositeConquerPoints is OppositeConquer / TotalPlayer * (-2),
OppositeNonConquerPoints is OppositeNonConquer / TotalPlayer * (-0.3),
```

Where `PlayerConquer` is the number of possible conquering moves, `PlayerNonConquer` is the number of possible non-conquering moves, `OppositeConquer` is the number of possible conquering moves of the opponent, and `OppositeNonConquer` is the number of possible non-conquering moves of the opponent. With these values, the AI prioritizes choosing moves that block the opponent from moving and evade being captured, while also setting up possible attacks. The number of moves is retrieved using the predicate `num_conquer_moves`.

The `value` predicate calculates the negated value of the heuristic. This is done to use `setof` to order the moves according to the best points. Since `setof` sorts the result in ascending order, for efficiency reasons, we want to sort them in descending order so that the best move is at the head of the list.

## Conclusions

The development of this work allowed us to understand the strengths and limitations of the **Prolog** programming language and this paradigm.

One of the limitations we found in **Sicstus Prolog** was the user interaction. The built-in functionalities to read input from the user are very hard to handle and very easy to break the program, especially the necessity of adding an `.` to any input, which is largely unavoidable without the use of very well-implemented external libraries.

However, regarding the implementation of the rules of the game, they were very straightforward to implement using this paradigm. We were able to build simple _rules_ on top of each other to build the whole ruleset. One of the most interesting and powerful characteristics of logical programming was the fact that **Prolog** could also use the **validation predicates** to generate moves, implementing an **AI player** very easily. This point is a great advantage over other programming paradigms as it has proven to be very efficient with very readable code.
