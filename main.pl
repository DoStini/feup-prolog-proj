:- [game].
:- [display].
:- [ai].
:- [input].

gameCycle(Board/_/_/_/_) :-
    end_game(Board, Player), !,
    drawGame(Board),
    format("~s~a~s", ["\nWinner is ", Player, "!\n\n"]).
gameCycle(Board/Size/Player/CurPlayerType/NextPlayerType) :-
    valid_moves(Board/Size/Player, List),
    length(List, MoveNo),
    MoveNo =:= 0, !,
    noMoves(Board/Size/Player),
    opposite(Player, NextPlayer),
    gameCycle(Board/Size/NextPlayer/NextPlayerType/CurPlayerType).
gameCycle(GameState/CurPlayerType/NextPlayerType) :-
    display_game(GameState),
    (
        repeat,
        choose_move(GameState, CurPlayerType, Move),
        (move(GameState, Move, NextState) ; (format("~s", ["!!INVALID MOVE, TRY AGAIN!!\n"]), fail))
    ),
    gameCycle(NextState/NextPlayerType/CurPlayerType).

handleOption(1) :-
    drawConfig,
    askConfig(Size, FirstPlayer),
    drawEndSection,
    generate_board(Size, Board),
    gameCycle(Board/Size/FirstPlayer/human/human).
handleOption(2) :-
    drawConfig,
    askConfig(Size, FirstPlayer),
    askDifficulty(Type),
    askFirst(First, Second, Type),
    drawEndSection,
    generate_board(Size, Board),
    gameCycle(Board/Size/FirstPlayer/First/Second).
handleOption(3) :-
    drawConfig,
    askConfig(Size, FirstPlayer),
    format("~s~a~s", ["Configure ", FirstPlayer, " AI\n"]),
    askDifficulty(FirstType),
    opposite(FirstPlayer, SecondPlayer),
    format("~s~a~s", ["Configure ", SecondPlayer, " AI\n"]),
    askDifficulty(SecondType),
    drawEndSection,
    generate_board(Size, Board),
    gameCycle(Board/Size/FirstPlayer/FirstType/SecondType).

play :-
    drawMenu,
    repeat,
    format("~s", ["Please choose an option: "]),
    read(Option),
    handleOption(Option), !.
