%% replace(+List, +X, +Value, -Result) is det.
%
%  Replaces a value in a list at the X index.
%
%  @param List The list.
%  @param X The index where Value will be placed.
%  @param Value The Value to be placed at the X index.
%  @param Result The resulting list.
%
replace(List, X, Value, Result) :-
    replace(List, X, Value, Result, [], 0).

replace([], _X, _Value, _Result, _AccRes, _X).

replace([_Elem | Rem], X, Value, Result, AccRes, X) :- 
    ElemList = [Value],
    append(AccRes, ElemList, NewRes),
    append(NewRes, Rem, Result).

replace([Elem | Rem], X, Value, Result, AccRes, AccX) :-
    NewX is AccX + 1,
    ElemList = [Elem],
    append(AccRes, ElemList, Combined),
    replace(Rem, X, Value, Result, Combined, NewX).
