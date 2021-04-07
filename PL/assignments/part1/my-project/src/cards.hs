module Cards where

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