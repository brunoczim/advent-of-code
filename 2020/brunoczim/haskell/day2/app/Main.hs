module Main where

import Lib (readAttempts, isCorrect)

main :: IO ()
main = do
  attempts <- readAttempts
  let correct = filter isCorrect attempts
      count = length correct
  print count
