player(red).
player(blue).

type('human').
type('random').
type('smart').

dir(up).
dir(up_right).
dir(right).
dir(down_right).
dir(down).
dir(down_left).
dir(left).
dir(up_left).

askNumber(Text, Min, Max, Number) :-
    repeat,
    write(Text),
    read(X),
    number(X),
    X >= Min,
    X < Max,
    Number = X, !.

askDifficulty(Type):-
    repeat,
    format("~s", ["Choose the AI difficulty:\n 1. Random\n 2. Smart\n"]),
    read(Option),
    askDifficulty(Type, Option), !.

askDifficulty(random, 1).
askDifficulty(smart, 2).
askDifficulty(_, _) :- fail.

askFirst(First, Second, Type) :-
    repeat,
    format("~s", ["Who goes first\n 1. Player\n 2. AI\n"]),
    read(Option),
    askFirst(First, Second, Type, Option), !.

askFirst(human, Type, Type, 1).
askFirst(Type, human, Type, 2).


askPlayer(Player) :-
    repeat,
    write('Please input initial player: (red/blue) '),
    read(Player),
    player(Player), !.

askConfig(Size, FirstPlayer) :-
    askNumber('Please input Size: ', 1, 1000000, Size),
    askPlayer(FirstPlayer).

askDir(Dir) :-
    repeat,
    write('Please input direction: '),
    read(Dir),
    dir(Dir), !.

askMove(Size, X/Y/Dir/_) :-
    askNumber('Please input X: ', 0, Size, X),
    askNumber('Please input Y: ', 0, Size, Y),
    askDir(Dir).
