cls :- write('\33\[2J').

player_char(red, 'X').
player_char(blue, 'O').
player_char(empty, ' ').

no_moves(Board/_/Player) :-
    nl,
    player_char(Player, Char),
    drawGame(Board),
    format("~s~a~s~a~s", ["\nPlayer ", Player, " (", Char  ,"), has no moves and can't play, turn is skipped!\n"]),
    nl.

draw_cell(Player) :-
    player_char(Player, Char),
    put_char(Char).

draw_line(Line) :-
    put_char('|'),
    draw_line_inner(Line).

draw_line_inner([]).
draw_line_inner([Elem | Line]) :-
    put_char(' '),
    draw_cell(Elem),
    put_char(' '),
    put_char('|'),
    draw_line_inner(Line).

draw_numbers(Board) :-
    put_char(' '),
    draw_numbers(Board, 0).

draw_numbers([], _) :- nl.
draw_numbers([_|Board], I) :-
    put_char(' '),
    put_char(' '),
    put_char(' '),
    write(I),
    NextI is I + 1,
    draw_numbers(Board, NextI).

draw_grid(Line) :-
    put_char(' '),
    put_char(' '),
    put_char('+'),
    draw_grid_inner(Line).

draw_grid_inner([]) :- nl.
draw_grid_inner([_|Line]) :-
    put_char('-'),
    put_char('-'),
    put_char('-'),
    put_char('+'),
    draw_grid_inner(Line).

drawGame(Board) :-
    format("~s", ["\n#####################################################\n"]),
    draw_numbers(Board),
    draw_board(Board),
    draw_numbers(Board),
    format("~s", ["#####################################################\n"]).

draw_board(Board) :-
    draw_grid(Board),
    draw_board(Board, 0).

draw_board([], _).
draw_board([Line | RemBoard], LineNo) :-
    write(LineNo),
    put_char(' '),
    draw_line(Line),
    put_char(' '),
    write(LineNo),
    nl,
    draw_grid(Line),
    LineNext is LineNo + 1,
    draw_board(RemBoard, LineNext).

display_game(Board/_/Player) :-
    nl,
    player_char(Player, Char),
    drawGame(Board),
    format("~s~a~s~a~s", ["\nPlayer ", Player, " (", Char  ,"), it is your turn to play!\n"]),
    nl.

draw_menu :-
    format("~s", ["\n#####################################################\n"]),
    format("~s", [  "                   WELCOME TO ZOLA                   \n"]),
    format("~s", ["\n 1. Play Against a Friend\n"]),
    format("~s", [" 2. Play Against the Computer\n"]),
    format("~s", [" 3. Watch Computers Play the Game\n"]),
    format("~s", ["\n#####################################################\n"]).

draw_config :-
    format("~s", ["\n#####################################################\n"]),
    format("~s", [  "                    CONFIGURATION                    \n\n"]).

draw_end_section :-
    format("~s", ["\n#####################################################\n"]).
