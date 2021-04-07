module IsValidAux where

import Types
import Cards
import Errors

-- Passes the State and list of Move's on to isValidAux',
-- along side a counter used to keep track of the amount of moves played
isValidAux :: State -> [Move] -> [Char]
isValidAux state moves = isValidAux' state moves 1

isValidAux' :: State -> [Move] -> Int -> [Char]
isValidAux' (State cards piecesA piecesB turn) [] count
    | errorInState (State cards piecesA piecesB turn) /= 0 = "NonValid Statess " ++ show (count-1)
    | null piecesA || null piecesB = show (State cards piecesA piecesB turn)

isValidAux' (State cards piecesA piecesB turn) (move:moves) count
    | errorInState (State cards piecesA piecesB turn) /= 0 = "NonValid State " ++ show (count-1)
    | null piecesA || null piecesB = show (State cards piecesA piecesB turn)
    | errorInMove cards move piecesA piecesB turn = "NonValid Move " ++ (show count)
    | otherwise = isValidAux' (applyMove (State cards piecesA piecesB turn) move) moves (count+1)

-- Takes a State and a Move, and returns a new State where the Move has been performed
applyMove :: State -> Move -> State
applyMove (State cards piecesA piecesB turn) (Move start end card)
    = (State cards' piecesA' piecesB' turn')
    where
        cards' = swapCards cards card
        piecesA' = getFirst (applyPieces start end turn piecesA piecesB)
        piecesB' = getSecond (applyPieces start end turn piecesA piecesB)
        turn' = (if turn == 0 then 1 else 0)

-- Takes the list of cards and the card played,
-- and swaps the last in the list and played card
swapCards :: [[Char]] -> [Char] -> [[Char]]
swapCards (x:xs) card
    | x == card = (last (x:xs)):(swapCards xs card)
    | null xs = card:[]
    | otherwise = x:(swapCards xs card)

applyPieces :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
applyPieces start end turn piecesA piecesB
    | turn == 0 && (head newPiecesA) == (4,2)  = (newPiecesA, [])
    | turn == 1 && (head newPiecesB) == (0,2) = ([], newPiecesB)
    | turn == 0 && end == (head newPiecesB) = (newPiecesA, [])
    | turn == 1 && end == (head newPiecesA) = ([], newPiecesB)
    | turn == 0 && elem end (tail newPiecesB) = (newPiecesA, (hitDetected piecesB end))
    | turn == 1 && elem end (tail newPiecesA) = ((hitDetected piecesA end), newPiecesB)
    | otherwise = (newPiecesA, newPiecesB)
    where
        newPiecesA = applyPiecesA start end turn piecesA
        newPiecesB = applyPiecesB start end turn piecesB

applyPiecesA :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)]
applyPiecesA start end turn piecesA
    | turn == 1 = piecesA
    | otherwise = ((takeWhile (/= start) piecesA) ++ end:[] ++ (tail (dropWhile (/= start) piecesA)))

applyPiecesB :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)]
applyPiecesB start end turn piecesB
    | turn ==  0 = piecesB
    | otherwise = (((takeWhile (/= start) piecesB) ++ end:[] ++ (tail (dropWhile (/= start) piecesB))))

hitDetected :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
hitDetected pieces end
    | length pieces == 1 = []
    | otherwise = ((takeWhile (/= end) pieces) ++ end:[] ++ (tail (dropWhile (/= end) pieces)))

getFirst :: ([(Int, Int)], [(Int, Int)]) -> [(Int, Int)]
getFirst (a,_) = a

getSecond :: ([(Int, Int)], [(Int, Int)]) -> [(Int, Int)]
getSecond (_,a) = a

testState = State ["Tiger", "Rooster", "Elephant", "Dragon", "Ox"] [(0,2), (0,0), (0,1), (0,3), (0,4)] [(4,2), (4,0), (4,1), (4,3), (4,4)] 0
move1 = Move (0,2) (2,2) "Tiger"
move2 = Move (4,1) (3,0) "Elephant"
move3 = Move (0,1) (1,1) "Ox"
move4 = Move (4,2) (2,2) "Tiger"
moves = [move1, move2, move3, move4]