:- [game].
:- [display].
:- [ai].
:- [input].

gameCycle(Board/_/_/_, _Size) :-
    end_game(Board, Player), !,
    drawGame(Board),
    format("~s~a~s", ["\nWinner is ", Player, "!\n\n"]).

gameCycle(GameState/Player1/Player2, Size) :-
    (
        repeat,
        display_game(GameState),
        choose_move(GameState, Player1, Move),
        (move(GameState, Move, NextState) ; (format("~s", ["!!INVALID MOVE, TRY AGAIN!!\n"]), fail))
    ),
    gameCycle(NextState/Player2/Player1, Size).

play :-
    askConfig(Size, FirstPlayer, Player1/Player2),
    generateBoard(Size, Board),
    gameCycle(Board/FirstPlayer/Player1/Player2, Size).
