:- [game].

player('red', red).
player('blue', blue).

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

match(X, X).

askNumber(Text, Min, Max, Number) :-
    repeat,
    write(Text),
    read(X),
    number(X),
    X >= Min,
    X < Max,
    Number = X, !.

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

askMove(Size, X, Y, Dir, Conquer) :-
    askNumber('Please input X: ', 0, Size, X),
    askNumber('Please input Y: ', 0, Size, Y),
    askDir(Dir),
    askConquer(Conquer).

gameCycle(Board/_, _Size) :-
    end_game(Board, PlayerYes), !,
    drawGame(Board),
    format("~s~a~s", ["\nWinner is ", PlayerYes, "!\n\n"]).
gameCycle(Board/Player, Size) :-
    display_game(Board/Player),
    (
        repeat,
        askMove(Size, X, Y, Dir, Conquer),
        (move(Board/Player, X/Y/Dir/Conquer, NewBoard/Next) ; (format("~s", ["!!INVALID MOVE, TRY AGAIN!!\n"]), fail))
    ),
    gameCycle(NewBoard/Next, Size).

handleOption(1) :-
    drawConfig,
    askConfig(Size, FirstPlayer),
    drawEndSection,
    generateBoard(Size, Board),
    gameCycle(Board/FirstPlayer, Size).
handleOption(2).

handleOption(3).
handleOption(_) :- fail.

play :-
    drawMenu,
    repeat,
    format("~s", ["Please choose an option: "]),
    read(Option),
    handleOption(Option), !.
    % askConfig(Size, FirstPlayer),
    % generateBoard(Size, Board),
    % gameCycle(Board/FirstPlayer, Size).
