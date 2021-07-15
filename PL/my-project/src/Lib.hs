module Lib 
    ( generateRandom
    , isValid
    , movesNumbers
    ) where

import System.IO
import Control.Monad
import System.Random
import Text.Read
import Data.List


data Turn = First | Second
    deriving Eq

-- A sum type with a list of cards. Pretty handy.
data Card = Rabbit
          | Cobra
          | Rooster
          | Tiger
          | Monkey
          | Crab
          | Crane
          | Frog
          | Boar
          | Horse
          | Elephant
          | Ox
          | Goose
          | Dragon
          | Mantis
          | Eel
          deriving (Show, Eq)

instance Ord Card where
    (<=) c1 c2 = show c1 <= show c2

-- Some type synonims first.
type Pos = (Int, Int)

-- useful type synonims, used for parsing
type Gamestate = ([Card], [Card], Card, [Pos], [Pos], Turn)

type Move = (Pos, Pos, Card)




-----------------------

-- test :: Int -> Bool
-- test m = isover && (noway final)
--   where
    
--     (_, final, ms) = run m

--     isover = length ms < 1000
--     noway (_, _, _, m1:_, m2:_, _) = (m1 /= (4,2)) && (m2 /= (0,2))
--     noway (_, _, _, m1:_, [], _) = (m1 /= (4,2))
--     noway (_, _, _, [], m2:_, _) = (m2 /= (0,2))
--     noway _ = False
    
-- -- run :: Int -> (Gamestate, )
-- run m = (initialGameState, final, ms)
--   where 
--     gen = mkStdGen m
--     initialGameState = generateInitialGameState gen
--     ms = reverse $ makeRandomMoves gen 1000 initialGameState []

--     final = case isValidmove initialGameState ms of
--       Left x0 -> error "lol"
--       Right x0 -> x0


-- testall k = filter test [1..k]





----------------------


---------------------
-- Main functions. --
---------------------
 

isValid :: FilePath -> IO (String)
isValid fp = do
    in_handle <- openFile fp ReadMode
    s         <- hGetContents in_handle
    case parse s of
        Nothing       -> pure "ParsingError"
        -- Just (gs, ms) -> pure (show (gs, ms))
        Just (gs, ms) -> case (isValidmove gs ms) of
            Right k      -> pure $ printGs k
            Left (s,e,c) -> pure $ "NonValid " ++ show (s,e,show c)


-- Given a seed and a number "n", generates a random game (starting from
-- the initial state, as defined by the rules) with
generateRandom :: Int -> Int -> IO (String)
generateRandom seed n = do
    -- Return the initial state, and the subsequent "n" moves (or less)
    return $ printGs initialGameState ++ "\n" ++ unlines (map (show . showMv) ms)
    where gen = mkStdGen seed
          initialGameState = generateInitialGameState gen
          ms = reverse $ makeRandomMoves gen n initialGameState []

-- Given an integer n and a filepath, returns all the possible games after n moves,
-- all the games where the first player wins, and all the games where the second player wins
movesNumbers :: Int -> FilePath -> IO (String)
movesNumbers n filepath = do
    in_handle <- openFile filepath ReadMode
    str       <- hGetContents in_handle
    case parse str of
      Nothing         -> return "(\"ParsingError\",-1,-1,-1)"
      Just (gamest,_) ->
          return $ "(\"OK\"," ++ x ++ "," ++ y ++ "," ++ z ++ ")"
          where firstPlayer = getFirstPlayer gamest
                (x',y',z') = calculatePossibleSeq n (1,0,0,firstPlayer) gamest
                x = show x'
                y = show y'
                z = show z'


---------------------------------------------
-- Auxiliary functions for generateRandom. --
---------------------------------------------

-- Auxiliary function used to print a gameplay to the console.
-- Given a list of pairs of moves and game states (each game state should
-- be the result of applying its accompanying move), prints first the move
-- on one line and then the resulting game state on another line.
-- gameplayToString :: [(Move, Gamestate)] -> String -> String
-- gameplayToString []                      acc = acc
-- gameplayToString ((move,gamestate) : xs) acc = gameplayToString xs newAcc
    -- where newAcc = acc ++ (show move) ++ "\n" ++ (show gamestate) ++ "\n"


-- Takes a random number generator, an integer "n" representing the amount
-- of moves, a game state, a move accumulator.
-- Returns a list of randomly generated moves.
makeRandomMoves :: StdGen
                -> Int
                -> Gamestate
                -> [Move]
                -> [Move]
makeRandomMoves gen n gamestate moveAcc
  -- "n" random moves were performed.
  | n <= 0    = moveAcc
  -- The game ended before "n" random moves were performed.
  | gameOver  = newAcc
  -- Continue applying moves.
  | otherwise = makeRandomMoves newGen (n-1) newGameState newAcc
  where (_, newGen)  = next gen
        newMove      = generateRandomMove gen gamestate
        gameOver     = isGameOver gamestate newMove
        newGameState = applyMoveAndChangeTurn gamestate newMove
        newAcc       = newMove : moveAcc


-- Given a random number generator and a game state, generates a random
-- and valid move.
generateRandomMove :: StdGen -> Gamestate -> Move
generateRandomMove gen gameState =
    case isValidmove gameState [move] of
      Left  _ -> generateRandomMove newGen gameState
      Right _ -> move
    where (cardsA, cardsB, fifthCard, posA, posB, turn) = gameState
          (randomCard, gen1)
            | turn == First = pickRandom gen cardsA
            | otherwise     = pickRandom gen cardsB
          (randomPiece@(p1,p2), gen2)
            | turn == First = pickRandom gen1 posA
            | otherwise     = pickRandom gen1 posB
          (randomMove@(m1,m2), gen3)
            | turn == First = pickRandom gen2 $ mov randomCard
            | otherwise     = pickRandom gen2 $ changeview $ mov randomCard
          move              = (randomPiece, (p1+m1, p2+m2), randomCard)
          (_, newGen)       = next gen3



-- Given a game state and a move, checks if the given move finishes
-- the game. The game is over when the opponent's master piece is captured
-- or when the player's master piece reaches the opponent's shrine.
isGameOver :: Gamestate -> Move -> Bool
isGameOver (_, _, _, (master1:_), (master2:_), First ) (m1, m2, _) =
    m2 == master2 || m1 == master1 && m2 == (4,2)
isGameOver (_, _, _, (master1:_), (master2:_), Second) (m1, m2, _) =
    m2 == master1 || m1 == master2 && m2 == (0,2)


-- Given a game state and a move, apply the move and return a new game state.
applyMoveAndChangeTurn :: Gamestate -> Move -> Gamestate
-- First player's turn.
applyMoveAndChangeTurn (cardsA,cardsB,fifth,posA,posB,First ) (m1,m2,card) =
    (sort newCardsA, cardsB, card, newPosA, newPosB, Second)
    where (masterPieceA:_) = posA
          newCardsA        = (:) fifth $ delete card cardsA
          newPosB = delete m2 posB
          -- If the master piece is captured, the game has already ended. We don't need to check that.
          newPosA
          -- If the master piece was moved, add it at the front of the list.
            | m1 == masterPieceA = (:) m2 $ delete m1 posA
          -- If a pupil was moved, add it at the end of the list.
            | otherwise          = delete m1 posA ++ [m2]
-- Second player's turn.
applyMoveAndChangeTurn (cardsA,cardsB,fifth,posA,posB,Second) (m1,m2,card) =
    (cardsA, sort newCardsB, card, newPosA, newPosB, First)
    where (masterPieceB:_) = posB
          newCardsB        = (:) fifth $ delete card cardsB
          newPosA = delete m2 posA
          -- If the master piece is captured, the game has already ended. We don't need to check that.       
          newPosB
          -- If the master piece was moved, add it at the front of the list.
            | m1 == masterPieceB = (:) m2 $ delete m1 posB
          -- If a pupil was moved, add it at the end of the list.
            | otherwise          = delete m1 posB ++ [m2]


-- -- Given a random number generator and a list of cards, returns a random
-- -- card from the list.
-- pickRandomCard :: StdGen -> [Card] -> Card
-- pickRandomCard gen cards = cards !! randomIndex
    -- where (randomNum, _) = next gen
          -- randomIndex    = randomNum `mod` (length cards)
-- 
-- 
-- -- Given a random number generator and a list of positions, returns a random
-- -- position from the list.
-- pickRandomPos :: StdGen -> [Pos] -> Pos
-- pickRandomPos gen ps = ps !! randomIndex
    -- where (randomNum, _) = next gen
          -- randomIndex    = randomNum `mod` (length ps)

-- Let's turn the above two into a single function

-- Given a random number generator and a list, returns a random element from the list,
-- togheter with a new generator
pickRandom :: StdGen -> [a] -> (a, StdGen)
pickRandom gen xs = (xs !! randomIndex, newGen)
    where (randomNum, newGen) = next gen
          randomIndex = randomNum `mod` (length xs)


-- Generates a initial game state, with a random selection of five cards
-- and a random turn with the traditional starting positions for all
-- of the pieces.
generateInitialGameState :: StdGen -> Gamestate
generateInitialGameState gen = (sort cardsA, sort cardsB, fifthCard, posA, posB, turn)
    where (c1:c2:c3:c4:c5:[]) = generateInitialCards gen
          cardsA              = c1:c2:[]
          cardsB              = c3:c4:[]
          fifthCard           = c5
          posA                = [(0,2),(0,0),(0,1),(0,3),(0,4)]
          posB                = [(4,2),(4,0),(4,1),(4,3),(4,4)]
          turn                = generateRandomTurn gen


-- Generates five random cards given a random number generator.
generateInitialCards :: StdGen -> [Card]
generateInitialCards gen = selectInitialAux gen []
    where selectInitialAux :: StdGen -> [Card] -> [Card]
          selectInitialAux gen cards
            | (==) 5 $ length cards = cards
            | otherwise             = selectInitialAux newGen newDeck
            where (_, newGen) = next gen
                  (randomCard, _)  = pickRandom gen allCards
                  newDeck     = nub $ randomCard : cards
                  allCards    =
                      [Rabbit, Cobra, Rooster, Tiger, Monkey, Crab,
                       Crane, Frog, Boar, Horse, Elephant, Ox, Goose,
                       Dragon, Mantis, Eel]


-- Generate a random turn given a random number generator.
generateRandomTurn :: StdGen -> Turn
generateRandomTurn gen
  | even randomNum = First
  | otherwise      = Second
  where (randomNum, _) = next gen

showMv (x, y, z) = (x, y, show z)

-------------------------------------------
-- Auxiliary functions for movesNumbers. --
-------------------------------------------

-- Given a game state, returns the player that will perform the first move.
getFirstPlayer :: Gamestate -> Turn
getFirstPlayer (_, _, _, _, _, turn) = turn


-- Given a number "n" and a game state, calculates the amount of all
-- possible sequences of "n" possible moves.
calculatePossibleSeq :: Int
                     -> (Int, Int, Int, Turn)
                     -> Gamestate
                     -> (Int, Int, Int)
calculatePossibleSeq n
                     acc   @(depth, win, losses, first)
                     gamest@(cardsA,cardsB,_,_,_,turn)
  | n == 0 = (depth, win, losses)
  | otherwise  =
      iterateOverCards n acc gamest (if turn == First then cardsA
                                                      else cardsB) (0,0,0)


-- Iterates over a list of cards for a given player.
-- Every card can be used with multiple pieces, so it iterates over the
-- game pieces on every function call.
iterateOverCards :: Int
                 -> (Int, Int, Int, Turn)
                 -> Gamestate
                 -> [Card]
                 -> (Int, Int, Int)
                 -> (Int, Int, Int)
iterateOverCards n acc1 _                                []     acc2 = acc2
iterateOverCards n acc1 gamestate@(_,_,_,posA,posB,turn) (x:xs) acc2 =
    iterateOverCards n acc1 gamestate xs newAcc
    where listOfPos          = if turn == First then posA else posB
          (count,win,losses) =
              iterateOverPos n acc1 gamestate x listOfPos (0,0,0)
          (accCount, accWin, accLosses) = acc2
          newAcc = (count + accCount, win + accWin, losses + accLosses)


-- Iterates over a list of game pieces that a player has a available.
-- On every game piece multiple moves can be applied, so it iterates over
-- every possible move and applies to move to the current gamestate.
-- It calls calculatePossibleSeq with every new game state to get the
-- results of every move.
iterateOverPos :: Int
               -> (Int, Int, Int, Turn)
               -> Gamestate
               -> Card
               -> [Pos]
               -> (Int, Int, Int)
               -> (Int, Int, Int)
iterateOverPos _ _                             _         _    []     acc2 =
    acc2
iterateOverPos n acc1@(depth,wins,losses,turn) gamestate card (x:xs) acc2 =
    iterateOverPos n acc1 gamestate card xs newAcc2
    where validMoves        = generateAllValidMoves gamestate card x
          -- Filter the moves that end the game.
          noGameOverMoves   = filter (not . (isGameOver gamestate)) validMoves
          -- Get the winners and lossers of every move, if they exist.
          winnersAndLossers :: [Maybe Turn]
          winnersAndLossers = map (getWinner gamestate) validMoves
          -- Count the times that the player that made the first move won.
          newWins           =
              foldr (\x acc ->
                  case x of
                    Nothing -> acc
                    Just p  -> if p == turn then acc+1
                                            else acc) 0 winnersAndLossers
          -- Count the times that the player that made the first move lost.
          newLosses         =
              foldr (\x acc ->
                  case x of
                    Nothing -> acc
                    Just p  -> if p /= turn then acc+1
                                            else acc) 0 winnersAndLossers
          -- Create a list of all possible (not game-over) new game states.
          newGameStates     =
              map (applyMoveAndChangeTurn gamestate) noGameOverMoves
          newRecursiveAcc   = (depth, wins, losses, turn)
          -- Count the new game states recursively.
          countNewGameStates =
              map (calculatePossibleSeq (n-1) newRecursiveAcc) newGameStates
          -- Recursively count the subsequent game states.
          (depthSum,rWins,rLosses) =
              foldr (\(d,w,l) (x,y,z) -> (d+x, w+y, l+z))
                    (0,0,0)
                    countNewGameStates

          (acc2Depth, acc2Wins, acc2Losses) = acc2
          newAcc2 = (acc2Depth+depthSum + newWins+newLosses, acc2Wins+rWins+newWins, acc2Losses+rLosses+newLosses)


-- Given a game state and a move, returns the winner of the game, if there
-- is any.
getWinner :: Gamestate -> Move -> Maybe Turn
getWinner gamestate@(cardsA,cardsB,fifth,posA,posB,turn) move@(m1,m2,_)
  | isGameOver gamestate move = Just turn
  | otherwise                 = Nothing


-- Takes a game state, a card and the position of a piece.
-- Returns all valid moves that can be done with the given card and piece.
generateAllValidMoves :: Gamestate -> Card -> Pos -> [Move]
generateAllValidMoves gamestate card piece@(x,y) =
    filter isValidMove possibleMoves
    where (_,_,_,posA,posB,turn) = gamestate
          isValidMove :: Move -> Bool
          isValidMove move = case isValidmove gamestate [move] of
                               Left  _ -> False
                               Right _ -> True
          relativePos
            | turn == First = mov card
            | otherwise     = changeview $ mov card
          possiblePos   = map (\(r1,r2) -> (x + r1, y + r2)) relativePos
          possibleMoves = map (\newPos -> (piece, newPos, card)) possiblePos


--------------------------------------
-- Auxiliary functions for isValid. --
--------------------------------------

diff :: Pos -> Pos -> (Int, Int)
diff (x, y) (k, p) = (x - k, y - p)


printGs :: Gamestate -> String
printGs (cds1, cds2, rest, p1, p2, t) = show ((map show $ sort cds1 ++ sort cds2 ++ [rest]), sorttail p1, sorttail p2, t)
    where -- necessary, even if the cards should supposedly always be ordered
        sorttail [] = []
        sorttail (x:xs) = x: sort xs
        


instance Show Turn where
    show First = show 0
    show Second = show 1

nextt :: Turn -> Turn
nextt First = Second
nextt Second = First


-- We associate every card with the relative positions the player can go to.
mov :: Card -> [(Int, Int)]
mov Rabbit = [(-1, -1), (1, 1), (0, 2)]
mov Cobra = [(0,-1), (1,1), (-1,1)]
mov Rooster = [(-1, -1), (0, -1), (0, 1), (1, 1)]
mov Tiger = [(-1,0), (2,0)]
mov Monkey = [(-1,-1), (-1, 1), (1,1), (1,-1)]
mov Crab = [(0, -2), (1,0), (0, 2)]
mov Crane = [(-1, -1), (1,0), (-1,1)]
mov Frog = [(0, -2), (1, -1), (-1,1)]
mov Boar = [(0,-1), (0, 1), (1, 0)]
mov Horse = [(0,-1), (-1, 0), (1, 0)]
mov Elephant = [(1, -1), (0, -1), (0, 1), (1, 1)]
mov Ox = [(-1, 0), (0, 1), (1, 0)]
mov Goose = [(1, -1), (0, -1), (0, 1), (-1, 1)]
mov Dragon = [(1, -2), (-1, -1), (-1, 1), (1, 2)]
mov Mantis = [(1, -1), (-1, 0), (1, 1)]
mov Eel = [(1, -1), (-1, -1), (0, 1)]


-- function to adapt the moves to the other player perspective
changeview xs = map (\(y, x) -> (-y, -x)) xs


-- Function to parse a string to a card
parseCard "Rabbit" = Just Rabbit
parseCard "Cobra" = Just Cobra 
parseCard "Rooster" = Just Rooster 
parseCard "Tiger" = Just Tiger 
parseCard "Monkey" = Just Monkey 
parseCard "Crab" = Just Crab 
parseCard "Crane" = Just Crane
parseCard "Frog" = Just Frog 
parseCard "Boar" = Just Boar 
parseCard "Horse" = Just Horse 
parseCard "Elephant" = Just Elephant 
parseCard "Ox" = Just Ox
parseCard "Goose" = Just Goose 
parseCard "Dragon" = Just Dragon
parseCard "Mantis" = Just Mantis
parseCard "Eel" = Just Eel
parseCard _ = Nothing

-- Helper function to aid parsing
parseMove (p1, p2, s) = case parseCard s of
    Nothing -> Nothing
    Just x -> Just (p1, p2, x)

toTurn 0 = Just First
toTurn 1 = Just Second
toTurn _ = Nothing


-- All the parsing is mostly done automagically thanks to haskell's "parse" function. Error handling is done by Maybe monad. What isn't catched by parsing alone is checked using "validategs"
parse :: String -> Maybe (Gamestate, [Move])
parse s = do
    (cds, p1, p2, state) <- readMaybe (head ls)
    cardlist <- sequence $ map parseCard cds
    turn <- toTurn state
    
    mv <- sequence $ map readMaybe (tail ls)
    moves <- sequence $ map (parseMove) mv
    
    gs <- validategs (cardlist, p1, p2, turn)
    
    return (gs, moves)

  where ls = lines s

-- Does additional checks to a gamestate
validategs :: ([Card], [Pos], [Pos], Turn) -> Maybe Gamestate
validategs (cardlist, p1, p2, ts) = 
    if cardlncheck && piecescheck && sortcard && sortpieces && dupcheck && coordcheck
    then Just (hand1, hand2, rest, p1, p2, ts)
    else Nothing

    where 
        -- are there 5 cards?
        cardlncheck = length cardlist <= 5

        -- are there 5 things in each hand?
        piecescheck = length p1 <= 5 && length p2 <= 5

        -- Is everything sorted in order?
        sortcard = issorted hand1 && issorted hand2
        sortpieces = issorted (tail p1) && issorted (tail p2)

        -- are there duplicates?
        dupcheck = length (nub (p1 ++ p2)) == length (p1 ++ p2)

        -- are all coordinates between 0 and 4 included?
        coordcheck = all (\(y, x) -> 0 <= x && x <= 4 && 0 <= y && y <= 4) (p1 ++ p2)

            
        hand1 = [cardlist !! 0, cardlist !! 1]
        hand2 = [cardlist !! 2, cardlist !! 3]
        rest = cardlist !! 4


issorted (x:y:xs) = x < y && issorted (y:xs) 
issorted x = True


isValidmove :: Gamestate -> [Move] -> Either Move Gamestate
isValidmove gs [] = Right gs
isValidmove (cds1, cds2, pool, p1, p2, t) ((start, end, c):rest) = 
    if mcheck && pcheck && checkpcard && checkpcard && checkcardmove && boundcheck
    then isValidmove (newcds1, newcds2, c, newp1, newp2, nextt t) rest
    else Left (start, end, c)

    where
        -- Do you have this piece? 
        curpl = if t == First then p1 else p2
        pcheck = start `elem` curpl

        -- are the coordinates inside the board
        boundcheck = all (\(y, x) -> 0 <= x && x <= 4 && 0 <= y && y <= 4) [start, end]
        
        -- Can you move that piece there?
        mcheck = not (end `elem` curpl)
        
        -- Do you have the correct card to make this move?
        viable = if t == First then mov c else changeview (mov c)
        checkcardmove = (diff end start) `elem` viable

        -- Do you have the correct card?
        curcds = if t == First then cds1 else cds2
        checkpcard = c `elem` curcds

        -- Update cards and players.
        newcds1 = if t == First then pool:delete c cds1 else cds1
        newcds2 = if t == Second then pool:delete c cds2 else cds2

                
        ismainpiece = head p1 == start || head p2 == start
        
        newp1 = if t == Second 
                then if (t == Second && start == head p2 && end == (0,2)) || end == head p1 then [] else delete end p1 
                else if ismainpiece then end : delete start p1 else (head p1) : sort (end : delete start (tail p1))
                
        newp2 = if t == First 
                then if (t == First && start == head p1 && end == (4,2)) || end == head p2 then [] else delete end p2 
                else if ismainpiece then end : delete start p2 else (head p2) : sort (end : delete start (tail p2))
