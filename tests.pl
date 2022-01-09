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



/*

     entryMove([[red,blue,red],[blue,empty,red],[empty,red,empty]], Player, X/Y, Dir, Conquer).
Player = red,
X = 0,
Y = 0,
Dir = right,
n
Player = red,
X = 0,
Y = 0,
Dir = down,
n
Player = blue,
X = 1,
Y = 0,
Dir = down_right,
n
Player = blue,
X = 1,
Y = 0,
Dir = down,
n
Player = red,
X = 2,
Y = 0,
Dir = left,
n
Player = blue,
X = 0,
Y = 1,
Dir = down,
n
Player = blue,
X = 0,
Y = 1,
Dir = right,
n
Player = blue,
X = 0,
Y = 1,
Dir = down_right,
n
Player = empty,
X = 1,
Y = 1,
Dir = down_right,
n
Player = empty,
X = 1,
Y = 1,
Dir = down_left,
n
Player = red,
X = 2,
Y = 1,
Dir = down,
n
Player = red,
X = 2,
Y = 1,
Dir = left,
n
Player = red,
X = 2,
Y = 1,
Dir = up_left,
n
Player = red,
X = 1,
Y = 2,
Dir = right,
n
Player = red,
X = 1,
Y = 2,
Dir = left,
n
Player = red,
X = 1,
Y = 2,
Dir = up,
n
Player = red,
X = 1,
Y = 2,
Dir = up_left,


*/



/**

    entryMove([[red,blue,red],[blue,empty,red],[empty,red,empty]], Player, X/Y, Dir, Conquer, TargetX/TargetY).
Player = red,
X = 0,
Y = 0,
Dir = right,
Conquer = true,
TargetX = 1,
n
Player = red,
X = 0,
Y = 0,
Dir = down,
Conquer = true,
TargetX = 0,
n
Player = blue,
X = 1,
Y = 0,
Dir = down_right,
Conquer = true,
TargetX = 2,
n
Player = blue,
X = 1,
Y = 0,
Dir = down,
Conquer = true,
TargetX = 1,
n
Player = red,
X = 2,
Y = 0,
Dir = left,
Conquer = true,
TargetX = 1,
n
Player = blue,
X = 0,
Y = 1,
Dir = down,
Conquer = false,
TargetX = 0,
n
Player = blue,
X = 0,
Y = 1,
Dir = right,
Conquer = true,
TargetX = 2,
n
Player = blue,
X = 0,
Y = 1,
Dir = down_right,
Conquer = true,
TargetX = 1,
n
Player = empty,
X = 1,
Y = 1,
Dir = down_right,
Conquer = false,
TargetX = 2,
n
Player = empty,
X = 1,
Y = 1,
Dir = down_left,
Conquer = false,
TargetX = 0,
n
Player = red,
X = 2,
Y = 1,
Dir = down,
Conquer = false,
TargetX = 2,
n
Player = red,
X = 2,
Y = 1,
Dir = left,
Conquer = true,
TargetX = 0,
n
Player = red,
X = 2,
Y = 1,
Dir = up_left,
Conquer = true,
TargetX = 1,
n
Player = red,
X = 1,
Y = 2,
Dir = right,
Conquer = false,
TargetX = 2,
n
Player = red,
X = 1,
Y = 2,
Dir = left,
Conquer = false,
TargetX = 0,
n
Player = red,
X = 1,
Y = 2,
Dir = up,
Conquer = true,
TargetX = 1,
n
Player = red,
X = 1,
Y = 2,
Dir = up_left,
Conquer = true,
TargetX = 0,

/*
