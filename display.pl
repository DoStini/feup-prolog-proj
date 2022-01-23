cls :- write('\33\[2J').

%% player_char(+Player, -Char) is det.
player_char(red, 'X').
player_char(blue, 'O').
player_char(empty, ' ').

%% dir_string(+Dir, -DirString) is det.
dir_string(up, "UP").
dir_string(up_right, "UP-and-RIGHT").
dir_string(right, "RIGHT").
dir_string(down_right, "DOWN-and-RIGHT").
dir_string(down, "DOWN").
dir_string(down_left, "DOWN-and-LEFT").
dir_string(left, "LEFT").
dir_string(up_left, "UP-and-LEFT").

%% conquer_string(+Conquer, -ConquerString) is det.
conquer_string(true, "CONQUERED").
conquer_string(false, "DIDN'T CONQUER").

%% display_ai_move(+Move, +Player) is det.
%
%  Displays an AI move in a textual form.
%
display_ai_move(Px/Py/Dir/Conquer, Player) :-
    sleep(1),
    dir_string(Dir, DirString),
    conquer_string(Conquer, ConquerString),
    format("~s~a~s~d~s~d~s~s~s~s~s",
        ["AI ", Player, " moved from (", Px, ", ", Py, ") in direction ", DirString, " and ", ConquerString, "."]
    ),
    nl,
    sleep(1).

%% no_moves(+GameState) is det.
%
%  Displays that a player has no moves.
%
no_moves(Board/_/Player) :-
    nl,
    player_char(Player, Char),
    draw_game(Board),
    format("~s~a~s~a~s", ["\nPlayer ", Player, " (", Char  ,"), has no moves and can't play, turn is skipped!\n"]),
    nl.

%% draw_cell(+Player) is det.
%
%  Draws a cell, converting player color to player char
%
draw_cell(Player) :-
    player_char(Player, Char),
    put_char(Char).

%% draw_line(+Line) is det.
%
%  Draws the cells in a line
%
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

%% draw_numbers(+Board) is det.
%
%  Draws the X numbers of the grid.
%
draw_numbers(Board) :-
    write('  '),
    draw_numbers(Board, 0).

draw_numbers([], _) :- nl.
draw_numbers([_|Board], I) :-
    format("~` t~1|~` t~2+~` t~d~2+", [I]),
    NextI is I + 1,
    draw_numbers(Board, NextI).

%% draw_grid(+Line) is det.
%
%  Draws the horizontal line between rows.
%
draw_grid(Line) :-
    put_char(' '),
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

%% draw_game(+Board) is det.
%
%  Draws the board and the x numbers.
%
draw_game(Board) :-
    format("~s", ["\n#####################################################\n"]),
    draw_numbers(Board),
    draw_board(Board),
    draw_numbers(Board),
    format("~s", ["#####################################################\n"]).

%% draw_board(+Board) is det.
%
%  Draws the grid lines and cells.
%
draw_board(Board) :-
    draw_grid(Board),
    draw_board(Board, 0).

draw_board([], _).
draw_board([Line | RemBoard], LineNo) :-
    format("~` t~d~2|", [LineNo]),
    put_char(' '),
    draw_line(Line),
    put_char(' '),
    write(LineNo),
    nl,
    draw_grid(Line),
    LineNext is LineNo + 1,
    draw_board(RemBoard, LineNext).

%% display_game(+GameState) is det.
%
%  Draws the board and shows a message.
%
display_game(Board/_/Player) :-
    nl,
    player_char(Player, Char),
    draw_game(Board),
    format("~s~a~s~a~s", ["\nPlayer ", Player, " (", Char  ,"), it is your turn to play!\n"]),
    nl.

%% draw_menu/0 is det.
draw_menu :-
    format("~s", ["\n#####################################################\n"]),
    format("~s", [  "                   WELCOME TO ZOLA                   \n"]),
    format("~s", ["\n 1. Play Against a Friend\n"]),
    format("~s", [" 2. Play Against the Computer\n"]),
    format("~s", [" 3. Watch Computers Play the Game\n"]),
    format("~s", ["\n#####################################################\n"]).

%% draw_config/0 is det.
draw_config :-
    format("~s", ["\n#####################################################\n"]),
    format("~s", [  "                    CONFIGURATION                    \n\n"]).

%% draw_end_section/0 is det.
draw_end_section :-
    format("~s", ["\n#####################################################\n"]).
