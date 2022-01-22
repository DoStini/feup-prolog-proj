:- [game].
:- [display].
:- [ai].
:- [input].

gameCycle(Board/_/_/_, _Size) :-
    end_game(Board, Player), !,
    drawGame(Board),
    format("~s~a~s", ["\nWinner is ", Player, "!\n\n"]).

gameCycle(GameState/Player1/Player2, Size) :-
    display_game(GameState),
    (
        repeat,
        choose_move(GameState, Player1, Move),
        (move(GameState, Move, NextState) ; (format("~s", ["!!INVALID MOVE, TRY AGAIN!!\n"]), fail))
    ),
    gameCycle(NextState/Player2/Player1, Size).

handleOption(1) :-
    drawConfig,
    askConfig(Size, FirstPlayer),
    drawEndSection,
    generateBoard(Size, Board),
    gameCycle(Board/FirstPlayer/human/human, Size).
handleOption(2) :-
    drawConfig,
    askConfig(Size, FirstPlayer),
    askDifficulty(Type),
    askFirst(First, Second, Type),
    drawEndSection,
    generateBoard(Size, Board),
    gameCycle(Board/FirstPlayer/First/Second, Size).
handleOption(3) :-
    drawConfig,
    askConfig(Size, FirstPlayer),
    format("~s~a~s", ["Configure ", FirstPlayer, " AI\n"]),
    askDifficulty(FirstType),
    opposite(FirstPlayer, SecondPlayer),
    format("~s~a~s", ["Configure ", SecondPlayer, " AI\n"]),
    askDifficulty(SecondType),
    drawEndSection,
    generateBoard(Size, Board),
    gameCycle(Board/FirstPlayer/FirstType/SecondType, Size).

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
