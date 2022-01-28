:- [game].
:- [display].
:- [ai].
:- [input].

%% initial_state(+Size, +Player, -GameState) is det.
%
%  Returns the initial GameState given a size and the first Player color
%
%  @param Size The size of the board
%  @param Player The color of the first player.
%  @param GameState The initial game state.
%
initial_state(Size, Player, GameState) :-
    generate_board(Size, Board),
    GameState = Board/Size/Player.

%% game_cycle(+GameState/Current)
game_cycle(Board/_/_/_/_) :-
    end_game(Board, Player), !,
    draw_game(Board),
    format("~s~a~s", ["\nWinner is ", Player, "!\n\n"]),
    play.
game_cycle(Board/Size/Player/CurPlayerType/NextPlayerType) :-
    valid_moves(Board/Size/Player, List),
    length(List, MoveNo),
    MoveNo =:= 0, !,
    no_moves(Board/Size/Player),
    opposite(Player, NextPlayer),
    game_cycle(Board/Size/NextPlayer/NextPlayerType/CurPlayerType).
game_cycle(GameState/CurPlayerType/NextPlayerType) :-
    display_game(GameState),
    (
        repeat,
        choose_move(GameState, CurPlayerType, Move),
        (move(GameState, Move, NextState) ; (format("~s", ["!!INVALID MOVE, TRY AGAIN!!\n"]), fail))
    ),
    game_cycle(NextState/NextPlayerType/CurPlayerType).

handle_option(1) :-
    draw_config,
    ask_config(Size, FirstPlayer),
    draw_end_section,
    initial_state(Size, FirstPlayer, GameState),
    game_cycle(GameState/human/human).
handle_option(2) :-
    draw_config,
    ask_config(Size, FirstPlayer),
    ask_difficulty(Type),
    ask_first(First, Second, Type),
    draw_end_section,
    initial_state(Size, FirstPlayer, GameState),
    game_cycle(GameState/First/Second).
handle_option(3) :-
    draw_config,
    ask_config(Size, FirstPlayer),
    format("~s~a~s", ["Configure ", FirstPlayer, " AI\n"]),
    ask_difficulty(FirstType),
    opposite(FirstPlayer, SecondPlayer),
    format("~s~a~s", ["Configure ", SecondPlayer, " AI\n"]),
    ask_difficulty(SecondType),
    draw_end_section,
    initial_state(Size, FirstPlayer, GameState),
    game_cycle(GameState/FirstType/SecondType).

play :-
    draw_menu,
    repeat,
    format("~s", ["Please choose an option: "]),
    read(Option),
    handle_option(Option), !.
