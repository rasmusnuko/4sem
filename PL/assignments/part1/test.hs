module Test where

import Types
import Cards

testState = State ["Tiger", "Rooster", "Elephant", "Dragon", "Ox"] [(0,2), (0,0), (0,1), (0,3), (0,4)] [(4,2), (4,0), (4,1), (4,3), (4,4)] 0
move1 = Move (0,2) (2,2) "Tiger"
move2 = Move (4,1) (3,0) "Elephant"
move3 = Move (0,1) (1,1) "Ox"
move4 = Move (4,2) (2,2) "Tiger"
moves = [move1, move2, move3, move4]