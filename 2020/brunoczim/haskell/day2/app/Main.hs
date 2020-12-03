module Main where

import Lib (readAttempts, isCorrect, oldIsCorrect)
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Builder.Int (decimal)
import qualified Data.Text.Lazy.Builder as TBuilder

main :: IO ()
main = do
  attempts <- readAttempts
  let oldCorrect = filter oldIsCorrect attempts
      oldCount = length oldCorrect
      correct = filter isCorrect attempts
      count = length correct
      tPrint = TIO.putStrLn . TBuilder.toLazyText . decimal
  TIO.putStr "Part 1: "
  tPrint oldCount
  TIO.putStr "Part 2: "
  tPrint count
