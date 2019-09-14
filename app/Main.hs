module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import System.Random

dictionaryPath:: String
dictionaryPath = "/usr/share/dict/words"

newtype Settings = Settings{count :: Int}

settings :: Parser Settings
settings = Settings
      <$> option auto
          ( long "words"
         <> short 'w'
         <> metavar "COUNT"
         <> showDefault
         <> value 4
         <> help "Number of words to use" )

main :: IO ()
main = genPassword =<< execParser opts
  where
    opts = info (settings <**> helper)
      ( fullDesc
     <> progDesc "passphrase generator inspired by xkcd 936"
     <> header "have fun!" )

genPassword :: Settings -> IO ()
genPassword (Settings count) = do
  content <- readFile dictionaryPath
  stdGen <- getStdGen
  let dictionaryWords = lines content
      dictSize = length dictionaryWords
      indexes = take count $ randoms stdGen :: [Int]
      cappedIndexes = fmap (`mod` dictSize) indexes
      usedWords = [dictionaryWords !! x | x <- cappedIndexes]
      output = unwords usedWords
  putStrLn output
