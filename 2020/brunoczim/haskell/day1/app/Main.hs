module Main where

import Lib (readEntries, selectEntries, entriesProd)
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Builder.Int (decimal)
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Lazy as Text

main :: IO ()
main = do
  entries <- readEntries
  let selected2 = selectEntries 2020 2 entries
      prod2 = entriesProd selected2
      selected3 = selectEntries 2020 3 entries
      prod3 = entriesProd selected3
      tPrint = TIO.putStrLn . TBuilder.toLazyText . decimal
  TIO.putStr (Text.pack "Part 1: ")
  tPrint prod2
  TIO.putStr (Text.pack "Part 2: ")
  tPrint prod3
