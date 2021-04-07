module Main where

import qualified Lib
import Options.Applicative
import Data.Semigroup ((<>))

data Opts = Opts {
  file :: Maybe String,
  optCmd :: Command
  }

data Command =
  GenerateRandom Int Int |
  IsValid String  |
  MovesNumbers Int String

optsParser :: Parser Opts
optsParser = Opts <$>
  (optional (option str (long "file" <> short 'f' <> help "File path for saving the output" <> metavar "FILEPATH")))
  <*> cmdOption

cmdOption = subparser (
 command "generateRandom" (info generateRandom ( progDesc "Call the function generateRandom" ))
 <> command "isValid" (info isValid ( progDesc "Call the function isValid" ))
 <> command "movesNumbers" (info movesNumbers ( progDesc "Call the function movesNumbers" ))
 ) where
  generateRandom = GenerateRandom <$>
    argument auto (metavar "INT" <> help "Seed") <*>
    argument auto (metavar "INT" <> help "Max depth")
  isValid = IsValid <$>
    argument str (metavar "FILEPATH" <> help "Input File")
  movesNumbers = MovesNumbers <$>
    argument auto (metavar "INT" <> help "Max depth") <*>
    argument str (metavar "FILEPATH" <> help "Input File")


main :: IO ()
main = do
  opts <- execParser (info (optsParser <**> helper) fullDesc)
  str <- case optCmd opts of
    GenerateRandom x y -> Lib.generateRandom x y
    IsValid x -> Lib.isValid x
    MovesNumbers x y -> Lib.movesNumbers x y
  case file opts of
    Nothing -> putStrLn str
    Just s -> writeFile s str
