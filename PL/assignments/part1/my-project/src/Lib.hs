module Lib (
 generateRandom,
 isValid,
 movesNumbers,
 isValidAux
 ) where

import System.IO
import Control.Monad
import System.Random

data Move = Move {
    start :: (Int, Int),
    end :: (Int, Int),
    card :: [Char]
} deriving Show

data State = State {
    cards :: [[Char]],
    piecesA :: [(Int, Int)],
    piecesB :: [(Int, Int)],
    turn :: Int
} deriving Show

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

-- Checks if any player has won
applyPieces :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
applyPieces start end turn piecesA piecesB
    | turn == 0 && (head newPiecesA) == (4,2)  = (newPiecesA, [])
    | turn == 1 && (head newPiecesB) == (0,2) = ([], newPiecesB)
    | turn == 0 && end == (head newPiecesB) = (newPiecesA, [])
    | turn == 1 && end == (head newPiecesA) = ([], newPiecesB)
    | turn == 0 && elem end (tail newPiecesB) = (newPiecesA, (hitDetected piecesB end))
    | turn == 1 && elem end (tail newPiecesA) = ((hitDetected piecesA end), newPiecesB)
    | otherwise = (newPiecesA, newPiecesB)
    where  -- Getting the new positions
        newPiecesA = applyPiecesA start end turn piecesA
        newPiecesB = applyPiecesB start end turn piecesB

-- Updating player A's pieces
applyPiecesA :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)]
applyPiecesA start end turn piecesA
    | turn == 1 = piecesA
    | otherwise = swapPieces piecesA start end

-- Updating player B's pieces
applyPiecesB :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)]
applyPiecesB start end turn piecesB
    | turn ==  0 = piecesB
    | otherwise = swapPieces piecesB end

-- Swaps start with end and returns the new list
swapPieces :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
swapPieces [] _ _ = []
swapPieces (x:xs) start end
    | x == start = end:(swapPieces xs start end)
    | otherwise = x:(swapPieces xs start end)

-- Removes a piece from a list
hitDetected :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
hitDetected pieces end
    | length pieces == 1 = []
    | otherwise = ((takeWhile (/= end) pieces) ++ end:[] ++ (tail (dropWhile (/= end) pieces)))

-- Gets first element i (Int, Int) tuple
getFirst :: ([(Int, Int)], [(Int, Int)]) -> [(Int, Int)]
getFirst (a,_) = a

-- Gets second element i (Int, Int) tuple
getSecond :: ([(Int, Int)], [(Int, Int)]) -> [(Int, Int)]
getSecond (_,a) = a

-- Passes on the state to be checked in a lot of ways
errorInState :: State -> Int
errorInState (State cards piecesA piecesB turn)
    | errorInCards cards = 1
    | (errorInPieces piecesA) || (errorInPieces piecesB) = 2
    | hasDuplicates (piecesA++piecesB) = 3
    | (turn /= 0) && (turn /= 1) = 4
    | otherwise = 0

-- Checks for 5 cards in total
errorInCards :: [[Char]] -> Bool
errorInCards [] = True
errorInCards cards
    | length cards /= 5 = True
    | otherwise = errorInCards' [getLegalMoves card | card <- cards]

-- Checks if all Cards in state are valid cards
-- (Nonvalid names gives an empty list)
errorInCards' :: [[(Int, Int)]] -> Bool
errorInCards' [] = False
errorInCards' (card:cards)
    | null card = True
    | otherwise = errorInCards' cards

-- Checks if all pieces have valid coordinates
errorInPieces :: [(Int, Int)] -> Bool
errorInPieces [] = False
errorInPieces ((x1, x2):pieces)
    | length ((x1, x2):pieces) > 5 = True
    | (x1 < 0) || (x2 < 0) || (x1 > 4) || (x2 > 4) = True
    | otherwise = errorInPieces pieces

-- Checks if any two pieces are on the same tile on the board
hasDuplicates :: [(Int, Int)] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs)
    | elem x xs = True
    | otherwise = hasDuplicates xs

-- Checks if anything is fundamentally invalid in the move
errorInMove :: [[Char]] -> Move -> [(Int, Int)] -> [(Int, Int)] -> Int -> Bool
errorInMove cards (Move start end card) piecesA piecesB turn
    | (turn == 0) && not (elem card (take 2 cards)) = True
    | (turn == 1) && not (elem card (cards !! 2 :[cards !! 3])) = True
    | (turn == 0) && not (elem start piecesA) = True
    | (turn == 1) && not (elem start piecesB) = True
    | otherwise = errorInMove' start end (getLegalMoves card) turn

-- Checks if the move is possible, given the start- and end position,
-- and the legal moves of the card played
errorInMove' :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> Bool
errorInMove' _ _ [] _ = True
errorInMove' (start1, start2) (end1, end2) ((x1, x2):xs) turn
    | (end1 < 0) || (end2 < 0) || (end1 > 4) || (end2 > 4) = True
    | (turn == 0) && (start1 + x1 == end1) && (start2 + x2 == end2) = False
    | (turn == 1) && (start1 - x1 == end1) && (start2 - x2 == end2) = False
    | otherwise = errorInMove' (start1, start2) (end1, end2) xs turn

-- Gets legal moves associated with the cards in the game
-- empty list, if the String is not a card in the game
getLegalMoves :: [Char] -> [(Int, Int)]
getLegalMoves card
    | card == "Rabbit" = [(-1,-1), (1,1), (0,2)]
    | card == "Cobra" = [(0,-1), (1,-1), (1,1)]
    | card == "Rooster" = [(-1,-1), (0,-1), (0,1), (1,1)]
    | card == "Tiger" = [(-1,0), (2, 0)]
    | card == "Monkey" = [(-1,-1), (-1,1), (1,-1), (1,1)]
    | card == "Crab" = [(0,-2), (1,0), (0,2)]
    | card == "Crane" = [(-1,-1), (1,0), (-1,1)]
    | card == "Frog" = [(0,-2), (1,-1), (-1,1)]
    | card == "Boar" = [(0,-1), (0,1), (1,0)]
    | card == "Horse" = [(-1,0), (0,-1), (1,0)]
    | card == "Elephant" = [(1,-1), (0,-1), (0,1), (1,1)]
    | card == "Ox" = [(0,1), (-1,0), (1,0)]
    | card == "Goose" = [(-1,1), (0,-1), (0,1), (1,-1)]
    | card == "Dragon" = [(1,-2), (-1,-1), (-1,1), (1,2)]
    | card == "Mantis" = [(1,-1), (-1,0), (1,1)]
    | card == "Eel" = [(1,-1), (-1,-1), (0,1)]
    | otherwise = []

isValid :: FilePath -> IO (String)
isValid _ = return "ParsingError"

generateRandom :: Int -> Int -> IO (String)
generateRandom _ _ = return "Not yet implemented"

movesNumbers :: Int -> String -> IO (String)
movesNumbers _ _ = return "Not yet implemented"