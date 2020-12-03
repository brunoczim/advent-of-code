module Lib
  ( Policy(..)
  , Attempt(..)
  , showPolicy
  , showAttempt
  , readPolicy
  , readAttempt
  , readAttempts
  , isCorrect
  ) where

import Data.Either (rights)
import Parse (Parser, readRest, expectChar, expectExact, parse)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TBuilder
import Data.Text.Lazy.Builder.Int (decimal)
import qualified Data.Text.Lazy.Read as TRead
import Control.Monad.Trans.State (StateT(StateT))

data Policy = Policy
  { policyMin :: Int
  , policyMax :: Int
  , policyChar :: Char
  }

data Attempt = Attempt
  { attemptPol :: Policy
  , attemptPwd :: Text
  }

isCorrect :: Attempt -> Bool
isCorrect att =
  let policy = attemptPol att
      max = policyMax policy
      min = policyMin policy
      char = policyChar policy
      pwd = attemptPwd att
      isCorrect' pwd k = case Text.uncons pwd of
        Just (c, pwd') 
          | c == char -> k < max && isCorrect' pwd' (k + 1)
          | c /= char -> isCorrect' pwd' k
        Nothing -> k >= min
  in isCorrect' pwd 0

readAttempts :: IO [Attempt]
readAttempts = do
  contents <- TIO.getContents
  let lines = Text.lines contents
      attempts = fmap (parse readAttempt) lines
  pure (rights attempts)

showPolicy :: Policy -> Builder
showPolicy pol = mconcat
  [ decimal (policyMin pol)
  , "-" 
  , decimal (policyMax pol)
  , " "
  , TBuilder.singleton (policyChar pol)
  ]

instance Show Policy where
  show = Text.unpack . TBuilder.toLazyText . showPolicy

showAttempt :: Attempt -> Builder
showAttempt att = mconcat
  [ showPolicy (attemptPol att)
  , ": " 
  , TBuilder.fromLazyText (attemptPwd att)
  ]

instance Show Attempt where
  show = Text.unpack . TBuilder.toLazyText . showAttempt

readPolicy :: Parser Policy
readPolicy = do
    min <- StateT TRead.decimal
    expectExact "-"
    max <- StateT TRead.decimal
    expectExact " "
    char <- expectChar
    pure (Policy { policyMin = min, policyMax = max, policyChar = char })

readAttempt :: Parser Attempt
readAttempt = do
  policy <- readPolicy
  expectExact ": "
  pwd <- readRest
  pure (Attempt { attemptPol = policy, attemptPwd = pwd })
