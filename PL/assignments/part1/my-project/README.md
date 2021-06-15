# my-project
I have commented all the code for movesNumbers out, as it gives the wrong output.
The idea was to find all possible states that the starting state could lead to,
up until there was one move left. The last move is performed in the sequencesFromState function.
This function finds all the possible move from a state, and counts how many there are, and how many are winning turns.
This is done for all possible states, resulting in a list of all sequences from all states.
These are added up, giving us the result.
I think the malfunction has to do with the findAllStates function.

for "movesNumbers 1 simple.in" it gave ("OK",8,0,0) as expected,
    "movesNumbers 2 simple.in" = ("OK",80,0,0),
    "movesNumbers 3 simple.in" = ("OK",908,6,0),
	"movesNumbers 4 simple.in" = ("OK",9744,6,128).
It seems odd, because it gets too many moves in "2 simple.in", but too few moves in the subsequent tests.
I really tried hard to get it working, but to no prevail.

The reason this project has '99% expressions used' is becuase the tests for
generateRandom somehow messed up two earlier tests, (duplicatePieces & errorInCards2)
the test 'duplicatePieces' tests the 'hasDuplicatePieces' function
'errorInCards2' tests the 'length cards /= 5' part of the 'errorInCards' function.
I tried rewritting the tests, but nothing seemed to help..