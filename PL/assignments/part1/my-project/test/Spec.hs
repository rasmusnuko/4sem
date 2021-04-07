import qualified Lib
import System.Directory
import System.FilePath.Posix
import qualified Data.List
import qualified Data.Set
import qualified Data.Text
import Data.Maybe
import Control.Monad

-- strip newlines and white spaces from a string
strip :: String -> String
strip = Data.Text.unpack . Data.Text.strip . Data.Text.pack

-- checks if the first string contains at least one substring passed in the second argument
hasSubstring :: String -> [String] -> Bool
hasSubstring x xs = or (map (Data.Text.isInfixOf (Data.Text.pack x)) (map Data.Text.pack xs))

-- retrieve the stems (the path without the extension) of all of the files in the directory
getAllStems :: String -> IO ([String])
getAllStems dir = liftM (map (dir </>) . Data.Set.toList . Data.Set.fromList . (filter (not.null) ) . map takeBaseName) (listDirectory dir)

-- format the test's information
formatTest :: String -> Maybe String -> Maybe String -> String -> String -> String
formatTest f input params obtained expected =
  Data.List.intercalate "\n" $ catMaybes [
    (("Input: " ++) . show. strip) <$> input,
    (("Params: " ++) . show . strip) <$> params,
    Just $ "Obtained: " ++ (show . strip) obtained,
    Just $ "Expected: " ++ (show . strip) expected
    ]

-- check a test for correctness and report the result
check :: String -> Maybe String -> Maybe String -> String -> String -> IO ()
check f input params obtained expected = do
  if strip obtained /= strip expected then do
    putStrLn $ "Failed test " ++ (show . takeBaseName) f
    putStrLn $ formatTest f input params obtained expected
    else do
      putStrLn $ "Passed test " ++ (show . takeBaseName) f
  putStrLn ""

applyIsValid :: String -> IO ()
applyIsValid f = do
  s1 <- readFile (f ++ ".in")
  s2 <- Lib.isValid (f ++ ".in")
  s3 <- readFile (f ++ ".out")
  check f (Just s1) Nothing s2 s3

applyGenerateRandom :: String -> IO ()
applyGenerateRandom f = do
  s1 <- readFile (f ++ ".param")
  let [seed, maxDepth] = ((map read) . lines . strip) s1
  s2 <- Lib.generateRandom seed maxDepth
  s3 <- readFile (f ++ ".out")
  s4 <- Lib.isValid (f ++ ".out")
  if strip s2 /= strip s3 || hasSubstring s4 ["NotValid", "ParsingError"] then do
    putStrLn $ "Failed test " ++ (show . takeBaseName) f
    when (strip s2 /= strip s3) $ do
      putStrLn $ formatTest f Nothing (Just s1) s2 s3
    when (hasSubstring s4 ["NotValid", "ParsingError"]) $ do
      putStrLn "The randomly generated moves are not valid."
  else do
    putStrLn $ "Passed test " ++ (show . takeBaseName) f
  putStrLn ""

applyMovesNumbers :: String -> IO ()
applyMovesNumbers f = do
  s1 <- readFile (f ++ ".in")
  s2 <- readFile (f ++ ".param")
  let maxDepth = (read . strip) s2
  s3 <- Lib.movesNumbers maxDepth (f ++ ".in")
  s4 <- readFile (f ++ ".out")
  check f (Just s1) (Just s2) s3 s4

main :: IO ()
main = do
    projDir <- getCurrentDirectory

    tests <- getAllStems (projDir </> "unit_tests" </> "isValid")
    putStrLn $ "The tests for isValid are " ++ (show . map takeBaseName) tests
    putStrLn ""
    mapM_ applyIsValid tests

    tests <- getAllStems (projDir </> "unit_tests" </> "generateRandom")
    putStrLn $ "The tests for generateRandom are " ++ (show . map takeBaseName) tests
    putStrLn ""
    mapM_ applyGenerateRandom tests

    tests <- getAllStems (projDir </> "unit_tests" </> "movesNumbers")
    putStrLn $ "The tests for movesNumbers are " ++ (show . map takeBaseName) tests
    putStrLn ""
    mapM_ applyMovesNumbers tests
