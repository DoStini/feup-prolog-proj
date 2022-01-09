distSqr([2,4], [1,7], X).
distInc([[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]], [0,0], [1,1]).


entryMove([[red,blue,red],[blue,empty,red],[empty,red,empty]], blue, 0/1, Dir, Conquer).
% should return down non conquer and down right and right conquer


test(Size):-
    generateBoard(Size, Board),
    verifyPlayerCell(Board, 1, 2/2).


test2(Size):-
    generateBoard(Size, Board),
    verifyPlayerCell(Board, 1, 2/2).