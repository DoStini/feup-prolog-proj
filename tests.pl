distSqr([2,4], [1,7], X).
distInc([[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]], [0,0], [1,1]).
move([[blue,red,blue],[red,blue,red],[blue,red,blue]], 0/0, right, false).
test(Size):-
    generateBoard(Size, Board),
    verifyPlayerCell(Board, 1, 2/2).


test2(Size):-
    generateBoard(Size, Board),
    verifyPlayerCell(Board, 1, 2/2).