import Types
import Cards

isValidAux :: State -> [Move] -> String
isValidAux (State cards piecesA piecesB turn) moves
    | errorInState (State cards piecesA piecesB turn) = "Error in state"
    | ((errorInMoves cards moves turn 1) /= 0) = "NonValid" ++ show (errorInMoves cards moves turn 1)
    | otherwise = "Is Valid (:"

errorInState :: State -> Bool
errorInState (State cards piecesA piecesB turn)
    | errorInCards cards = True
    | (errorInPieces piecesA) || (errorInPieces piecesB) = True
    | (turn /= 0) || (turn /= 1) = True
    | otherwise = False

errorInCards :: [[Char]] -> Bool
errorInCards [] = True
errorInCards cards
    | length cards /= 5 = True
    | otherwise = errorInCards' [getLegalMoves card | card <- cards]

errorInCards' :: [[(Int, Int)]] -> Bool
errorInCards' [] = False
errorInCards' (card:cards)
    | null card = True
    | otherwise = errorInCards' cards

errorInPieces :: [(Int, Int)] -> Bool
errorInPieces [] = False
errorInPieces ((x1, x2):pieces)
    | length ((x1, x2):pieces) > 5 = True
    | (x1 < 0) || (x2 < 0) = True
    | (x1 > 4) || (x2 > 4) = True
    | otherwise = errorInPieces pieces

errorInMoves :: [[Char]] -> [Move] -> Int -> Int -> Int
errorInMoves cards [] turn count = 0
errorInMoves cards ((Move start end card):moves) count
    | turn == 0 && not (elem card (take 2 cards)) = count
    | turn == 1 && not (elem card (cards !! 2):[cards !! 3]) = count
    | errorInMove start end (getLegalMoves card) turn piecesA piecesB = count
    | otherwise = errorInMoves cards moves (count+1)

errorInMove :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)] -> [(Int, Int)] -> Bool
errorInMove _ _ [] = True
errorInMove (start1, start2) (end1, end2) ((x1, x2):xs) turn piecesA piecesB
    | (turn == 0) && not (elem (start1, start2) piecesA) = True
    | (turn == 1) && not (elem (start1, start2) piecesB) = True
    | (turn == 0) && (start1 + x1 == end1) && (start2 + x2 == end2) = False
    | (turn == 1) && (start1 - x1 == end1) && (start2 - x2 == end2) = False
    | otherwise = errorInMove (start1, start2) (end1, end2) xs turn piecesA piecesB
    