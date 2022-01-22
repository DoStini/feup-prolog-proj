:- use_module(library(random)).
:- [input].

choose_move(Board/_, human, Move) :-
    length(Board, Size),
    askMove(Size, Move).

choose_move(GameState, Type, Move):-
    valid_moves(GameState, Moves),
    choose_move(Type, GameState, Moves, Move).

choose_move(random, _GameState, Moves, Move):-
    random_member(Move, Moves).

choose_move(smart, Board/Player, Moves, Move):-
    setof(Value-Mv, NewBoard^( 
        member(Mv, Moves),
        move(Board/Player, Move, NewBoard/_),
        value(NewBoard, Player, Value) 
    ), [_V-Move|_]),
    nl.

test_choose(Move) :-
    generateBoard(5, Board),
    valid_moves(Board/red, Moves),
    choose_move(human, Board, Moves, Move).
