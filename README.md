Hands of Time solver
====================

A simple, brute-force solver for Hands of Time puzzles.

Background
----------
Hands of Time is a class of particularly annoying puzzles in
Final Fantasy XIII-2.  A player is presented with a circle of _N_ numbers,
ranging from 1 to _N_/2, where _N_ can be anywhere from 5 to 13.  The player
must visit each number precisely once, and after visiting a position with
number _k_, the next position (if any) must be precisely _k_ spaces clockwise
or counterclockwise from the current position.

(In more CS-friendly terms, the player is given a directed graph with _N_
vertices in a circle where the targets of the two edges starting at a given
vertex _v_ are at equal distances but opposite directions from _v_,
and must find a Hamiltonian path through this graph.)

Operation
---------
The code compiles under GHC.  When prompted for the puzzle, input a list of the
numbers given in the puzzle (say, starting at the top and going clockwise).  The
output will be a list of positions to visit in the puzzle (0 being the top
position, 1 a single space clockwise from there, etc.)

Comments
--------
One one level, there's not much to the solver: try an initial space, try going
clockwise, backtrack and try counterclockwise if that doesn't work, try the
next initial space if one fails.  I find it interesting mostly because of how
easy it is to implement with Haskell's persistent set data structure; there's
no need to mark a position as available again when backtracking past it,
because the original set of available positions is still there.
