-- YEHAW
module Lib (
 generateRandom,
 isValid,
 movesNumbers,
 isValidAux
 ) where

import System.IO
import Control.Monad
import System.Random
import Data.Maybe
import Data.List
import Text.Read

data State = State ([String], [(Int, Int)], [(Int, Int)], Int) deriving (Show, Read)
data Move = Move ((Int, Int), (Int,Int), String) deriving (Show, Read)

-- Parses State and [Moves] from a filePath
-- and passes the data to isValidAux
isValid :: String -> IO (String)
isValid filePath = do
    contents <- readFile filePath
    let linesAsList = lines contents
    let state = ("State " ++ (head linesAsList))
    let moves = ["Move " ++ x | x <- (tail linesAsList)]
    let maybeState = readMaybe state :: Maybe State
    let maybeMoves = [readMaybe x :: Maybe Move | x <- moves]
    -- Getting maybe state
    if (isNothing maybeState)
    then return "ParsingError"
    -- Getting maybe moves
    else if any (&&True) [isNothing x | x <- maybeMoves]
    then return "ParsingError"
    -- Checking for empty moves list or invalid state
    else if (null maybeMoves)
    then return "ParsingError"
    else if (errorInState (read state :: State) /= 0)
    then return "ParsingError"
    else return (isValidAux (read state :: State) [ read x :: Move | x <- moves]) 

-- Passes the State and list of Move's on to isValidAux',
-- along side a counter used to keep track of the amount of moves played
isValidAux :: State -> [Move] -> [Char]
isValidAux state [] = removeFirstWord (show state)
isValidAux (State (cards, piecesA, piecesB, turn)) (move:moves) 
    | (null piecesA || null piecesB) && not (null (move:moves)) = "NonValid " ++ removeFirstWord (show move)
    | errorInMove cards move piecesA piecesB turn = "NonValid " ++ removeFirstWord (show move)
    | errorInState newState /= 0 = "NonValid " ++ removeFirstWord (show move)
    | otherwise = isValidAux newState moves
    where
        state = (State (cards, piecesA, piecesB, turn))
        newState = applyMove (State (cards, piecesA, piecesB, turn)) move 

-- Removes everything up to and including the first space
removeFirstWord :: [Char] -> String 
removeFirstWord [] = []
removeFirstWord (x:xs)
    | (x == ' ') = xs
    | otherwise = removeFirstWord xs

-- Takes a State and a Move, and returns a new State where the Move has been performed
applyMove :: State -> Move -> State
applyMove (State (cards, piecesA, piecesB, turn)) (Move (start, end, card))
    = (State (cards', piecesA', piecesB', turn'))
    where
        cards' = sortCards (swapCards cards card)
        piecesA' = sortPieces (getFirst (applyPieces start end turn piecesA piecesB))
        piecesB' = sortPieces (getSecond (applyPieces start end turn piecesA piecesB))
        turn' = (if turn == 0 then 1 else 0)

-- Swaps played card with the last card
swapCards (x:xs) card
    | x == card = (last (x:xs)):(swapCards xs card)
    | null xs = card:[]
    | otherwise = x:(swapCards xs card)

-- Sorts first and second, and third and fourth cards, lexicographically repectively
sortCards :: [[Char]] -> [[Char]]
sortCards cards = (sort(take 2 cards)) ++ (sort((cards !! 2):(cards !! 3):[])) ++ (last cards):[]

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

-- Sorts a players pieces lexicographically
sortPieces :: [(Int, Int)] -> [(Int, Int)]
sortPieces [] = []
sortPieces (x:xs) = x:(sort xs)

-- Updating player A's pieces
applyPiecesA :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)]
applyPiecesA start end turn piecesA
    | turn == 1 = piecesA
    | otherwise = swapPieces piecesA start end

-- Updating player B's pieces
applyPiecesB :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)]
applyPiecesB start end turn piecesB
    | turn ==  0 = piecesB
    | otherwise = swapPieces piecesB start end

-- Swaps start with end and returns the new list
swapPieces :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
swapPieces [] _ _ = []
swapPieces (x:xs) start end
    | x == start = end:(swapPieces xs start end)
    | otherwise = x:(swapPieces xs start end)

-- Removes a piece from a list
hitDetected :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
hitDetected (x:xs) end
    | null xs = []
    | x == end = (hitDetected xs end)
    | otherwise = x:(hitDetected xs end)

-- Gets first element i (Int, Int) tuple
getFirst :: ([(Int, Int)], [(Int, Int)]) -> [(Int, Int)]
getFirst (a,_) = a

-- Gets second element i (Int, Int) tuple
getSecond :: ([(Int, Int)], [(Int, Int)]) -> [(Int, Int)]
getSecond (_,a) = a

-- Passes on the state to be checked in a lot of ways
errorInState :: State -> Int
errorInState (State (cards, piecesA, piecesB, turn))
    | errorInCards cards = 1
    | piecesA /= (sortPieces piecesA) && piecesB /= (sortPieces piecesB) = 2
    | (errorInPieces piecesA) || (errorInPieces piecesB) = 3
    | hasDuplicates (piecesA++piecesB) = 4
    | (turn /= 0) && (turn /= 1) = 5
    | otherwise = 0

-- Checks for 5 cards in total
errorInCards :: [[Char]] -> Bool
errorInCards [] = True
errorInCards cards
    | cards /= (sortCards cards) = True
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
errorInMove cards (Move (start, end, card)) piecesA piecesB turn
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

isValid' :: FilePath -> IO (String)
isValid' _ = return "ParsingError"

generateRandom :: Int -> Int -> IO (String)
generateRandom _ _ = return "Not yet implemented"

movesNumbers :: Int -> String -> IO (String)
movesNumbers _ _ = return "Not yet implemented"
