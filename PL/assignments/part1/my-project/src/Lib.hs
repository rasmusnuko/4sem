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
isValidAux :: State -> [Move] -> String
isValidAux state [] = removeFirstWord (show state)
isValidAux (State (cards, piecesA, piecesB, turn)) (move:moves) 
    | (null piecesA || null piecesB) && not (null (move:moves)) = "NonValid " ++ removeFirstWord (show move)
    | errorInMove cards move piecesA piecesB turn = "NonValid " ++ removeFirstWord (show move)
    | otherwise = isValidAux newState moves
    where
        newState = applyMove (State (cards, piecesA, piecesB, turn)) move 

-- Removes everything up to and including the first space
removeFirstWord :: String -> String 
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
        turn' = if turn == 0 then 1 else 0

-- Swaps played card with the last card
swapCards (x:xs) card
    | x == card = (last xs):(swapCards xs card)
    | null xs = card:[]
    | otherwise = x:(swapCards xs card)

-- Sorts first and second, and third and fourth cards, lexicographically repectively
sortCards :: [String] -> [String]
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
sortPieces (x:xs)
    | null xs = [x]
    | otherwise = x:(sort xs)

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
    | hasDuplicatePieces (piecesA++piecesB) = 4
    | (turn /= 0) && (turn /= 1) = 5
    | otherwise = 0

-- Checks for 5 cards in total
errorInCards :: [String] -> Bool
errorInCards [] = True
errorInCards cards
    | duplicateCards cards = True
    | cards /= (sortCards cards) = True
    | length cards /= 5 = True
    | otherwise = errorInCards' [getLegalMoves card | card <- cards]

duplicateCards :: [String] -> Bool
duplicateCards [] = False
duplicateCards (x:xs)
    | elem x xs = True
    | otherwise = duplicateCards xs

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
    | (length pieces) > 4 = True
    | (x1 < 0) || (x2 < 0) || (x1 > 4) || (x2 > 4) = True
    | otherwise = errorInPieces pieces

-- Checks if any two pieces are on the same tile on the board
hasDuplicatePieces :: [(Int, Int)] -> Bool
hasDuplicatePieces [] = False
hasDuplicatePieces (x:xs)
    | elem x xs = True
    | otherwise = hasDuplicatePieces xs

-- Checks if anything is fundamentally invalid in the move
errorInMove :: [String] -> Move -> [(Int, Int)] -> [(Int, Int)] -> Int -> Bool
errorInMove cards (Move (start, end, card)) piecesA piecesB turn
    | (turn == 0) && not (elem card (take 2 cards)) = True
    | (turn == 1) && not (elem card ([cards !! 2] ++ [cards !! 3])) = True
    | (turn == 0) && not (elem start piecesA) = True
    | (turn == 1) && not (elem start piecesB) = True
    | (turn == 0) && (elem end piecesA) = True
    | (turn == 1) && (elem end piecesB) = True
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
getLegalMoves :: String -> [(Int, Int)]
getLegalMoves card
    | card == "Rabbit" = [(-1,-1), (1,1), (0,2)]
    | card == "Cobra" = [(0,-1), (-1,1), (1,1)]
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

-- Gets cards names from a index
-- empty string, if the index is not described  
getCardNames :: Int -> String
getCardNames index
    | index == 0  = "Rabbit" 
    | index == 1  = "Cobra" 
    | index == 2  = "Rooster"
    | index == 3  = "Tiger" 
    | index == 4  = "Monkey" 
    | index == 5  = "Crab" 
    | index == 6  = "Crane" 
    | index == 7  = "Frog" 
    | index == 8  = "Boar" 
    | index == 9  = "Horse" 
    | index == 10 = "Elephant" 
    | index == 11 = "Ox" 
    | index == 12 = "Goose" 
    | index == 13 = "Dragon" 
    | index == 14 = "Mantis" 
    | index == 15 = "Eel" 

generateRandom :: Int -> Int -> IO (String)
generateRandom seed n = do
    let cards = sortCards [getCardNames card | card <- getRandomCards seed]
    let piecesA = getRandomPieces seed 3
    let piecesB = getRandomPieces (seed*2) 4
    let pieces = removeDuplicates piecesA piecesB [] []
    let finalPiecesA = sortPieces (getFirst pieces)
    let finalPiecesB = sortPieces (getSecond pieces)
    let turn = seed `mod` 2
    let state = (State (cards, finalPiecesA, finalPiecesB, turn))
    let moveList = getMovesList state seed n
    return (makeOutputString state moveList)
        
-- Returns 5 Int values [0 - 15] with no duplicates
getRandomCards :: Int -> [Int]
getRandomCards seed = getRandomCards' seed []
getRandomCards' :: Int -> [Int] -> [Int]
getRandomCards' seed foundCards
    | (length foundCards) == 5 = foundCards
    | otherwise = getRandomCards' (seed+1) (newCard:foundCards)
    where
        newCard = seed*(231) `mod` 16

-- Returns 5 random (Int, Int)
getRandomPieces :: Int -> Int -> [(Int, Int)]
getRandomPieces seed offset = getRandomPieces' seed offset []
getRandomPieces' :: Int -> Int -> [(Int,Int)] -> [(Int, Int)]
getRandomPieces' seed offset foundPieces
    | (length foundPieces) == 5 = foundPieces
    | newPiece `elem` foundPieces = getRandomPieces' (seed+offset) offset foundPieces
    | otherwise = getRandomPieces' (seed+3) offset (newPiece:foundPieces)
    where
        seed' = if seed `mod` 5 == 0 then (((seed-2) `mod` 99)*offset) else seed
        newPiece0 = seed'*(seed'+(length foundPieces)) `mod` 5
        newPiece1 = seed'*(seed'+(length foundPieces)*3) `mod` 5
        newPiece = if even seed' then (newPiece0, newPiece1) else (newPiece1, newPiece0)

-- Removes pieces that are standing on the same tile
removeDuplicates :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
removeDuplicates [] _ acceptedPiecesA acceptedPiecesB = (acceptedPiecesA, acceptedPiecesB)
removeDuplicates piecesA piecesB acceptedPiecesA acceptedPiecesB
    | x `elem` (y:ys) = removeDuplicates xs ys acceptedPiecesA (y:acceptedPiecesB)
    | y `elem` (x:xs) = removeDuplicates xs ys (x:acceptedPiecesA) acceptedPiecesB
    | otherwise = removeDuplicates xs ys (x:acceptedPiecesA) (y:acceptedPiecesB)
    where
        (x:xs) = piecesA
        (y:ys) = piecesB

-- Turns State and [Move] into a formatted string, seperated with \n
makeOutputString :: State -> [Move] -> String
makeOutputString state moves = makeOutputString' state moves ""
makeOutputString' state [] output = (removeFirstWord (show state)) ++ "\n" ++ output
makeOutputString' state (move:moves) output
    = makeOutputString' state moves (output ++ (removeFirstWord (show move)) ++ "\n")

-- Returns a list of all valid moves from a given state
-- Also keeps track of whether a move is a winning move or not
allValidMoves :: State -> [(Bool, Move)]
allValidMoves (State (cards, piecesA, piecesB, turn)) = validMoves
    where
        pieces' = if turn == 0 then piecesA else piecesB
        cards' = [(card, getLegalMoves card) | card <- cards]
        cards'' = if turn == 0 then (take 2 cards') else ([cards' !! 2] ++ [cards' !! 3])
        validMoves = 
            [
             ((winningMove piece card piecesA piecesB turn index),  -- Check if winning move
              Move (piece, (calcMove piece card turn index), (getCardName card))) -- Move containing valid move
              | piece <- pieces', -- Get a piece
                card <- cards'',  -- Get a (cardString, cardMove) tuple
                index <- [1..((length (cardMoves card))-1)], -- List of 0 to length of number of moves-1 from card
                (not (errorInMove cards (Move (piece, (calcMove piece card turn index), (getCardName card))) piecesA piecesB turn)) -- Only add move to list if it is valid)
            ] 

-- Returns true if provided information gives a winning move
winningMove :: (Int, Int) -> (String, [(Int, Int)]) -> [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> Bool
winningMove piece card piecesA piecesB turn index
    | turn == 0 && (piece == head piecesA) && (calcMove piece card turn index) == (4,2) = True
    | turn == 1 && (piece == head piecesB) && (calcMove piece card turn index) == (0,2) = True
    | turn == 0 && (calcMove piece card turn index) == (head piecesB) = True
    | turn == 1 && (calcMove piece card turn index) == (head piecesA) = True
    | otherwise = False

-- A description has been written over the parameters:
--             piece        entry of cards''     turn
calcMove :: (Int, Int) -> (String, [(Int, Int)]) -> Int -> Int -> (Int, Int)
calcMove (start0, start1) card turn index
    | turn == 0 = ((start0+cardMove0), (start1+cardMove1))
    | turn == 1 = ((start0-cardMove0), (start1-cardMove1))
    where
        moves = cardMoves card
        (cardMove0, cardMove1) = (moves !! index)
        

-- Extract name of card from (cardName, cardMove) tuple
getCardName :: (String, [(Int, Int)]) -> String
getCardName (string, cardMoves) = string

-- Gets all moves from a card
cardMoves :: (String, [(Int, Int)]) -> [(Int, Int)]
cardMoves (string, cardMovesList) = cardMovesList

-- Makes a pseudorandom list of moves
getMovesList :: State -> Int -> Int -> [Move]
getMovesList state seed n
    = getMovesList' state seed n []
getMovesList' :: State -> Int -> Int -> [Move] -> [Move]
getMovesList' state seed n foundMoves
    | (length foundMoves) == n = foundMoves
    | null validMoves = foundMoves
    | winningMove = (foundMoves++[randomMove])
    | otherwise = getMovesList' newState (seed+1) n (foundMoves++[randomMove]) 
    where
        validMoves = allValidMoves state
        nMoves = length validMoves
        randomMoveTuple = (validMoves !! ((seed*seed*11*17) `mod` nMoves))
        randomMove = extractMove randomMoveTuple
        winningMove = extractWinning randomMoveTuple
        newState = (applyMove state randomMove)

-- Extracts move from (Bool, Move) tuple
extractMove :: (Bool, Move) -> Move
extractMove (bool, move) = move

-- Extracts winning bool from (Bool, Move) tuple
extractWinning :: (Bool, Move) -> Bool
extractWinning (bool, move) = bool

movesNumbers :: Int -> String -> IO (String)
movesNumbers n string = do
  return "Not functioning properly"
--     contents <- readFile filePath
--     let linesAsList = lines contents
--     let state = ("State " ++ (head linesAsList))
--     let maybeState = readMaybe state :: Maybe State
--     -- Getting maybe state
--     if (isNothing maybeState)
--     then return (show ("ParsingError", -1, -1, -1) )
--     -- Checking for empty moves list or invalid state
--     else if (errorInState (read state :: State) /= 0)
--     then return (show ("ParsingError", -1, -1, -1))
--     else return (show (movesNumberResult n (read state :: State)))
-- 
-- movesNumberResult :: Int -> State -> (String, Int, Int, Int)
-- movesNumberResult n (State (cards, piecesA, piecesB, turn))
--     | n == 0 = ("OK", 0, 0, 0)
--     | otherwise = result
--     where
--         state = (State (cards, piecesA, piecesB, turn))
--         startingPlayer = turn
--         allSequences = [(sequencesFromState state startingPlayer) | state <- findAllStates n state]
--         resultTuple = addUpSequences allSequences
--         (resTotal, resWinning, resLosing) = resultTuple
--         result = ("OK", resTotal, resWinning, resLosing)
-- 
-- findAllStates :: Int -> State -> [State]
-- findAllStates n state = findAllStates' n [state] [] 0
-- findAllStates' :: Int -> [State] -> [State] -> Int -> [State]
-- findAllStates' n foundStates currentDepth index
--     | n == 1 = foundStates
--     | index == (length foundStates) = findAllStates' (n-1) (foundStates++currentDepth) [] index
--     | otherwise = findAllStates' n foundStates (currentDepth++newStates) (index+1)
--     where
--         currentState = (foundStates !! index)
--         allCurrentMoves = allValidMoves currentState
--         newStates = [applyMove currentState (extractMove move) | move <- allCurrentMoves, not (extractWinning move)]
-- 
-- sequencesFromState :: State -> Int -> (Int, Int, Int)
-- sequencesFromState (State (cards, piecesA, piecesB, turn)) startingPlayer = (total, winning, losing)
--     where
--         state = (State (cards, piecesA, piecesB, turn))
--         allMovesFromState = allValidMoves state
--         total = length allMovesFromState
--         winning = if (turn == startingPlayer) then sum [1 | move <- allMovesFromState, extractWinning move] else 0
--         losing  = if (turn /= startingPlayer) then sum [1 | move <- allMovesFromState, extractWinning move] else 0
-- 
-- -- Function names says it all 
-- addUpSequences :: [(Int, Int, Int)] -> (Int, Int, Int)
-- addUpSequences allSequences = addUpSequences' allSequences initialResult
--     where
--         initialResult = (0, 0, 0)
-- addUpSequences' :: [(Int, Int, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
-- addUpSequences' allSequences resultCount
--     | null allSequences = resultCount
--     | otherwise = addUpSequences' seqs intermediateResult
--     where
--         (seq:seqs) = allSequences
--         (seqTotal, seqWinning, seqLosing) = seq
--         (resTotal, resWinning, resLosing) = resultCount
--         intermediateResult = ((resTotal+seqTotal), (resWinning+seqWinning), (resLosing+seqLosing)) 
-- 