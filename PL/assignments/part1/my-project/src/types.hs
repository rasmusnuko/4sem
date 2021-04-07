module Types where

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