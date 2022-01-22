player('red', red).
player('blue', blue).

type('human').
type('random').
type('smart').

dir('up', up).
dir('up_right', up_right).
dir('right', right).
dir('down_right', down_right).
dir('down', down).
dir('down_left',down_left).
dir('left', left).
dir('up_left', up_left).

conquer('y', true).
conquer('n', false).

askNumber(Text, Min, Max, Number) :-
    repeat,
    write(Text),
    read(X),
    number(X),
    X >= Min,
    X < Max,
    Number = X, !.

% askTypePlayers(Type1, Type2) :-
%     repeat,
%     write('Please choose player 1: (human/random/smart) '),
%     read(Type1),
%     type(Type1), !,
%     repeat,
%     write('Please choose player 2: (human/random/smart) '),
%     read(Type2),
%     type(Type2), !.

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
    read(X),
    player(X, Player), !.

askConfig(Size, FirstPlayer) :-
    askNumber('Please input Size: ', 1, 1000000, Size),
    askPlayer(FirstPlayer).

askDir(Dir) :-
    repeat,
    write('Please input direction: '),
    read(X),
    dir(X, Dir), !.

askConquer(Conquer) :-
    repeat,
    write('Conquer? (Y/N) '),
    read(X),
    conquer(X, Conquer), !.

askMove(Size, X/Y/Dir/Conquer) :-
    askNumber('Please input X: ', 0, Size, X),
    askNumber('Please input Y: ', 0, Size, Y),
    askDir(Dir),
    askConquer(Conquer).