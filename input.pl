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

ask_number(Text, Min, Max, Number) :-
    repeat,
    write(Text),
    read(X),
    number(X),
    X >= Min,
    X < Max,
    Number = X, !.

ask_difficulty(Type):-
    repeat,
    format("~s", ["Choose the AI difficulty:\n 1. Random\n 2. Smart\n"]),
    read(Option),
    ask_difficulty(Type, Option), !.

ask_difficulty(random, 1).
ask_difficulty(smart, 2).
ask_difficulty(_, _) :- fail.

ask_first(First, Second, Type) :-
    repeat,
    format("~s", ["Who goes first\n 1. Player\n 2. AI\n"]),
    read(Option),
    ask_first(First, Second, Type, Option), !.

ask_first(human, Type, Type, 1).
ask_first(Type, human, Type, 2).


ask_player(Player) :-
    repeat,
    write('Please input initial player: (red/blue) '),
    read(Player),
    player(Player), !.

ask_config(Size, FirstPlayer) :-
    ask_number('Please input Size: ', 1, 1000000, Size),
    ask_player(FirstPlayer).

ask_dir(Dir) :-
    repeat,
    write('Please input direction: '),
    read(Dir),
    dir(Dir), !.

askMove(Size, X/Y/Dir/_) :-
    ask_number('Please input X: ', 0, Size, X),
    ask_number('Please input Y: ', 0, Size, Y),
    ask_dir(Dir).
