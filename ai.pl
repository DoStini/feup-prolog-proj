:- use_module(library(random)).

% choose_move(GameState, human, Move):-

% move(Board/Player, Px/Py/Dir/Conquer, NewBoard/Next) :-
choose_move(Board, computer-Level, Move):-
    valid_moves(Board, Moves),
    choose_move(Level, Board, Moves, Move).

choose_move(1, _Board, Moves, Move):-
    random_member(Move, Moves).

choose_move(2, Board, Moves, Move):-
    setof(Value-Mv, NewBoard^( 
        member(Mv, Moves),
        move(Board/Player, Move, NewBoard/_),
        value(NewBoard, Player, Value) 
    ), [_V-Move|_]).

test_choose(Move) :-
    generateBoard(5, Board),
    valid_moves(Board/red, Moves),
    choose_move(1, Board, Moves, Move).
