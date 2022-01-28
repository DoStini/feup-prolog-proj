:- use_module(library(random)).
:- use_module(library(system)).
:- [input].

%% choose_move(+GameState, +PlayerType except random, -Move) is det.
%% choose_move(+GameState, random, -Move) is nondet.
%
%  Selects a move given a PlayerType, which can be human/random/smart.
%  human is asked for a move, random chooses at random, and smart uses an heuristic.
%
%  @param GameState The current GameState.
%  @param PlayerType The type of player
%  @param Move The selected Move
%
choose_move(_/Size/_, human, Move) :-
    askMove(Size, Move).

choose_move(Board/Size/Player, Type, Move):-
    valid_moves(Board/Size/Player, Moves),
    choose_move(Type, Board/Size/Player, Moves, Move),
    display_ai_move(Move, Player).

choose_move(random, _GameState, Moves, Move):-
    random_member(Move, Moves).

choose_move(smart, Board/Size/Player, Moves, Move):-
    setof(Value-Mv, NewBoard^( 
        member(Mv, Moves),
        move(Board/Size/Player, Mv, NewBoard/_/_),
        value(NewBoard/Size/Player, Value) 
    ), [_V-Move|_]).
