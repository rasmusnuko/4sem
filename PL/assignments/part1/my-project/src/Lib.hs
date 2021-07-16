module Lib (
 generateRandom,
 isValid,
 movesNumbers,
 isValidAux
) where

import System.IO
import Control.Monad
import System.Random
import Debug.Trace
import Data.Maybe
import Data.List
import Text.Read

-- State datatype represents a game state
data State = State ([String], [(Int, Int)], [(Int, Int)], Int) deriving (Show, Read)
-- Move datatype represents a move made by a player
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
isValidAux (State (cards, piecesA, piecesB, turn)) xs@(move:moves) 
    | (null piecesA || null piecesB) && not (null xs) = "NonValid " ++ removeFirstWord (show move)
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
    | null xs = [card]
    | otherwise = x:(swapCards xs card)

-- Sorts first and second, and third and fourth cards, lexicographically repectively
sortCards :: [String] -> [String]
sortCards cards = (sort(take 2 cards)) ++ (sort((cards !! 2):[cards !! 3])) ++ [last cards]

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
hitDetected [] _ = []
hitDetected (x:xs) end
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
    | piecesA /= (sortPieces piecesA) || piecesB /= (sortPieces piecesB) = 2
    | (errorInPieces piecesA) || (errorInPieces piecesB) = 3
    | hasDuplicatePieces (piecesA++piecesB) = 4
    | (turn /= 0) && (turn /= 1) = 5
    | otherwise = 0

-- Checks for 5 cards in total
errorInCards :: [String] -> Bool
errorInCards [] = True
errorInCards cards
    | length cards /= 5 = True
    | duplicateCards cards = True
    | cards /= (sortCards cards) = True
    | otherwise = errorInCards' [getLegalMoves card | card <- cards]

-- Returns true, if list contains duplicate cards
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
errorInPieces pieces@((x1, x2):xs)
    | (length pieces) > 5 = True
    | (x1 < 0) || (x2 < 0) || (x1 > 4) || (x2 > 4) = True
    | otherwise = errorInPieces xs

-- Checks if any two pieces are on the same tile on the board
hasDuplicatePieces :: [(Int, Int)] -> Bool
hasDuplicatePieces [] = False
hasDuplicatePieces (x:xs)
    | elem x xs = True
    | otherwise = hasDuplicatePieces xs

-- Checks if anything is fundamentally invalid in the move
errorInMove :: [String] -> Move -> [(Int, Int)] -> [(Int, Int)] -> Int -> Bool
errorInMove cards (Move (start, end, card)) piecesA piecesB turn
    | (turn == 0) && not (card `elem` (take 2 cards)) = True
    | (turn == 1) && not (card `elem` ([cards !! 2] ++ [cards !! 3])) = True
    | (turn == 0) && not (start `elem` piecesA) = True
    | (turn == 1) && not (start `elem` piecesB) = True
    | (turn == 0) && (end `elem` piecesA) = True
    | (turn == 1) && (end `elem` piecesB) = True
    | otherwise = errorInMove' start end (getLegalMoves card) turn

-- Checks if the move is possible, given the start- and end position,
-- and the legal moves of the card played
errorInMove' :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> Bool
errorInMove' (start1, start2) (end1, end2) ((x1, x2):xs) turn
    | (end1 < 0) || (end2 < 0) || (end1 > 4) || (end2 > 4) = True
    | (turn == 0) && (start1 + x1 == end1) && (start2 + x2 == end2) = False
    | (turn == 1) && (start1 - x1 == end1) && (start2 - x2 == end2) = False
    | otherwise = errorInMove' (start1, start2) (end1, end2) xs turn

-- Generates a pseudo-random State & list of n moves, based on a seed
generateRandom :: Int -> Int -> IO (String)
generateRandom seed n = do
    let gen = mkStdGen seed
    let (randomCards, gen1) = getRandomCards gen
    let cards = sortCards [getCardNames card | card <- randomCards]
    let (piecesA, gen2) = getRandomPieces gen1
    let (piecesB, gen3) = getRandomPieces gen2
    let pieces = removeDuplicates piecesA piecesB [] []
    let finalPiecesA = sortPieces (getFirst pieces)
    let finalPiecesB = sortPieces (getSecond pieces)
    let (turn, gen4) = randomR (0,1) gen3
    let state = (State (cards, finalPiecesA, finalPiecesB, turn))
    let moveList = getMovesList state gen4 n
    return (makeOutputString state moveList)
        
-- Returns 5 Int values [0 - 15] with no duplicates
getRandomCards :: StdGen -> ([Int], StdGen)
getRandomCards gen = getRandomCards' gen []
getRandomCards' :: StdGen -> [Int] -> ([Int], StdGen)
getRandomCards' gen foundCards
    | (length foundCards) == 5 = (foundCards, newGen)
    | newCard `elem` foundCards = getRandomCards' newGen foundCards
    | otherwise = getRandomCards' newGen (newCard:foundCards)
    where
        (newCard, newGen) = randomR (0,15) gen

-- Returns 5 random (Int, Int)
getRandomPieces :: StdGen -> ([(Int, Int)], StdGen)
getRandomPieces gen = getRandomPieces' gen []
getRandomPieces' :: StdGen -> [(Int,Int)] -> ([(Int, Int)], StdGen)
getRandomPieces' gen foundPieces
    | (length foundPieces) == 5 = (foundPieces, newGen2)
    | newPiece `elem` foundPieces = getRandomPieces' newGen2 foundPieces
    | otherwise = getRandomPieces' newGen2 (newPiece:foundPieces)
    where
        (newPiece0, newGen1) = randomR (0,4) gen
        (newPiece1, newGen2) = randomR (0,4) newGen1
        newPiece = (newPiece0, newPiece1)

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
        cards' = if turn == 0 then (take 2 cards) else ([cards !! 2] ++ [cards !! 3])
        cards'' = [(card, getLegalMoves card) | card <- cards']
        validMoves = 
            [
             ((winningMove piece card piecesA piecesB turn index),  -- Check if winning move
              Move (piece, (calcMove piece card turn index), (getCardName card))) -- Move containing valid move
              | piece <- pieces', -- Get a piece
                card <- cards'',  -- Get a (cardString, cardMove) tuple
                index <- [0..((length (cardMoves card))-1)], -- List of 0 to length of number of moves-1 from card
                (not (errorInMove cards (Move (piece, (calcMove piece card turn index), (getCardName card))) piecesA piecesB turn)) -- Only add move to list if it is valid)
            ] 

-- Returns true if provided information gives a winning move
winningMove :: (Int, Int) -> (String, [(Int, Int)]) -> [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> Bool
winningMove piece card piecesA piecesB turn index
    | turn == 0 && (piece == head piecesA) && ((calcMove piece card turn index) == (4,2)) = True
    | turn == 1 && (piece == head piecesB) && ((calcMove piece card turn index) == (0,2)) = True
    | turn == 0 && ((calcMove piece card turn index) == (head piecesB)) = True
    | turn == 1 && ((calcMove piece card turn index) == (head piecesA)) = True
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
getMovesList :: State -> StdGen -> Int -> [Move]
getMovesList state gen n
    = getMovesList' state gen n []
getMovesList' :: State -> StdGen -> Int -> [Move] -> [Move]
getMovesList' state gen n foundMoves
    | (length foundMoves) == n = foundMoves
    | null validMoves = foundMoves
    | winningMove = (foundMoves++[randomMove])
    | otherwise = getMovesList' newState newGen n (foundMoves++[randomMove]) 
    where
        validMoves = allValidMoves state
        nMoves = length validMoves
        (randomIndex, newGen) = randomR (0, (nMoves-1)) gen
        randomMoveTuple = (validMoves !! randomIndex)
        randomMove = extractMove randomMoveTuple
        winningMove = extractWinning randomMoveTuple
        newState = (applyMove state randomMove)

-- Extracts move from (Bool, Move) tuple
extractMove :: (Bool, Move) -> Move
extractMove (bool, move) = move

-- Extracts winning bool from (Bool, Move) tuple
extractWinning :: (Bool, Move) -> Bool
extractWinning (bool, move) = bool

-- Calculates number of moves, aswell as winning / losing scenarios for the starting player
movesNumbers :: Int -> String -> IO (String)
movesNumbers n filePath = do
    contents <- readFile filePath
    let linesAsList = lines contents
    let state = ("State " ++ (head linesAsList))
    let maybeState = readMaybe state :: Maybe State
    -- Getting maybe state
    if (isNothing maybeState)
    then return (show badState)
    -- Checking for empty moves list or invalid state
    else if (errorInState (read state :: State) /= 0)
    then return (show badState)
    else return (show (movesNumberResult n (read state :: State)))
    where
        badState = ("ParsingError",-1,-1,-1)

-- Finds all winning/losing moves from a state,
-- aswell as all last moves after n moves.
-- Converts these numbers into a ("OK", total, winning, losing) tuple
movesNumberResult :: Int -> State -> (String, Int, Int, Int)
movesNumberResult n state@(State (cards, piecesA, piecesB, turn))
    | n == 0 = ("OK", 0, 0, 0)
    | otherwise = result
    where
        startingPlayer = turn
        (lastStates, (winning, losing)) = findAllStates n state
        lastSequences = [(sequencesFromState aState startingPlayer) | aState <- lastStates]
        (lastTotal, lastWinning, lastLosing) = addUpSequences lastSequences
        (total', winning', losing') = 
            if n `mod` 2 == 1
            then ((lastTotal+(winning+losing)), (lastWinning+winning), (lastLosing+losing))
            else ((lastTotal+(winning+losing)), (lastWinning+losing), (lastLosing+winning))
        result = ("OK", total', winning', losing')

-- Finds all states after n moves
-- Also returns the total amount of wins before the final set of move, for each player 
findAllStates :: Int -> State -> ([State], (Int, Int))
findAllStates n state = findAllStates' n ([state], (0,0))
findAllStates' :: Int -> ([State], (Int, Int)) -> ([State], (Int, Int))
findAllStates' n (foundStates, winningCount)
    | n == 1 = (foundStates, winningCount)
    | otherwise = 
        findAllStates' (n-1) ([
                              applyMove state (extractMove move)
                              | state <- foundStates,
                                move <- allValidMoves state
                              ], newWinningCount)
    where
        (player0, player1) = winningCount
        winningResult = player0 + (sum [1 | state <- foundStates, move <- allValidMoves state, (extractWinning $ move)])
        newWinningCount = (player1, winningResult)

-- Generates a tuple containing (total, winning, losing)-moves
-- within one move from the given state
sequencesFromState :: State -> Int -> (Int, Int, Int)
sequencesFromState state@(State (cards, piecesA, piecesB, turn)) startingPlayer = (total, winning, losing)
    where
        allMovesFromState = allValidMoves state
        total = length allMovesFromState
        winning = if (turn == startingPlayer) then sum [1 | move <- allMovesFromState, extractWinning move] else 0
        losing  = if (turn /= startingPlayer) then sum [1 | move <- allMovesFromState, extractWinning move] else 0

-- Function names says it all 
addUpSequences :: [(Int, Int, Int)] -> (Int, Int, Int)
addUpSequences allSequences = addUpSequences' allSequences (0, 0, 0)
addUpSequences' :: [(Int, Int, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
addUpSequences' allSequences resultCount
    | null allSequences = resultCount
    | otherwise = addUpSequences' seqs intermediateResult
    where
        (seq:seqs) = allSequences
        (seqTotal, seqWinning, seqLosing) = seq
        (resTotal, resWinning, resLosing) = resultCount
        intermediateResult = ((resTotal+seqTotal), (resWinning+seqWinning), (resLosing+seqLosing))

-- Gets legal moves associated with the cards in the game
-- empty list, if the String is not a card in the game
getLegalMoves :: String -> [(Int, Int)]
getLegalMoves card
    | card == "Rabbit"   = [(-1,-1), (1,1), (0,2)]
    | card == "Cobra"    = [(0,-1), (-1,1), (1,1)]
    | card == "Rooster"  = [(-1,-1), (0,-1), (0,1), (1,1)]
    | card == "Tiger"    = [(-1,0), (2, 0)]
    | card == "Monkey"   = [(-1,-1), (-1,1), (1,-1), (1,1)]
    | card == "Crab"     = [(0,-2), (1,0), (0,2)]
    | card == "Crane"    = [(-1,-1), (1,0), (-1,1)]
    | card == "Frog"     = [(0,-2), (1,-1), (-1,1)]
    | card == "Boar"     = [(0,-1), (0,1), (1,0)]
    | card == "Horse"    = [(-1,0), (0,-1), (1,0)]
    | card == "Elephant" = [(1,-1), (0,-1), (0,1), (1,1)]
    | card == "Ox"       = [(0,1), (-1,0), (1,0)]
    | card == "Goose"    = [(-1,1), (0,-1), (0,1), (1,-1)]
    | card == "Dragon"   = [(1,-2), (-1,-1), (-1,1), (1,2)]
    | card == "Mantis"   = [(1,-1), (-1,0), (1,1)]
    | card == "Eel"      = [(1,-1), (-1,-1), (0,1)]
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

