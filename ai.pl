:- use_module(library(random)).
:- [input].

choose_move(_/Size/_, human, Move) :-
    askMove(Size, Move).

choose_move(GameState, Type, Move):-
    valid_moves(GameState, Moves),
    choose_move(Type, GameState, Moves, Move).

choose_move(random, _GameState, Moves, Move):-
    random_member(Move, Moves).

choose_move(smart, Board/Size/Player, Moves, Move):-
    setof(Value-Mv, NewBoard^( 
        member(Mv, Moves),
        move(Board/Size/Player, Mv, NewBoard/_/_),
        value(NewBoard/Size/Player, Value) 
    ), [_V-Move|_]),
    format("~s~a~s", ["AI ", Player, " moved "]),
    write(Move),
    nl.

test_choose(Move) :-
    generate_board(5, Board),
    valid_moves(Board/red, Moves),
    choose_move(human, Board, Moves, Move).
