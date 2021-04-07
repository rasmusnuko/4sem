module Errors where

import Types
import Cards

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
