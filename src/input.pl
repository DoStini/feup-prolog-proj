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

%% ask_number(+Text, +Min, +Max, -Number) is det.
%
%  Asks the player for a number between Min and Max.
%
ask_number(Text, Min, Max, Number) :-
    repeat,
    write(Text),
    read(X),
    number(X),
    X >= Min,
    X < Max,
    Number = X, !.

%% ask_difficulty(-Type) is det.
%
%  Asks the player for AI difficulty (smart/random).
%
ask_difficulty(Type):-
    repeat,
    format("~s", ["Choose the AI difficulty:\n 1. Random\n 2. Smart\n"]),
    read(Option),
    ask_difficulty(Type, Option), !.

ask_difficulty(random, 1).
ask_difficulty(smart, 2).

%% ask_first(-First, -Second, +Type) is det.
%
%  Asks the player for who goes first, player or AI.
%
%  @param First The PlayerType that goes first.
%  @param Second The PlayerType that goest second.
%  @param Type the PlayerType of the AI.
%
ask_first(First, Second, Type) :-
    repeat,
    format("~s", ["Who goes first\n 1. Player\n 2. AI\n"]),
    read(Option),
    ask_first(First, Second, Type, Option), !.

ask_first(human, Type, Type, 1).
ask_first(Type, human, Type, 2).

%% ask_player(-Player) is det.
%
%  Asks the player which color goes first.
%
ask_player(Player) :-
    repeat,
    write('Please input initial player: (red/blue) '),
    read(Player),
    player(Player), !.

%% ask_config(-Size, -FirstPlayer) is det.
%
%  Asks the player for board size and first player color.
%
ask_config(Size, FirstPlayer) :-
    ask_number('Please input Size: ', 1, 1000000, Size),
    ask_player(FirstPlayer).

%% ask_dir(-Dir) is det.
%
%  Asks the player which Dir they would like to go.
%  validated with dir(+Dir).
%
ask_dir(Dir) :-
    repeat,
    write('Please input direction: '),
    read(Dir),
    dir(Dir), !.

%% ask_move(+Size, -Move) is det.
%
%  Asks the player for a move inside the board bounds.
%
askMove(Size, X/Y/Dir/_) :-
    ask_number('Please input X: ', 0, Size, X),
    ask_number('Please input Y: ', 0, Size, Y),
    ask_dir(Dir).
