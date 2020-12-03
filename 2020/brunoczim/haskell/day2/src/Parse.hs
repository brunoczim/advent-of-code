module Parse
  ( Parser
  , raise
  , expectChar
  , expectExact
  , readRest
  , parse
  ) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Control.Monad.Trans.State (StateT, StateT(StateT), runStateT)

type Parser a = StateT Text (Either String) a

raise :: String -> Parser a
raise e = StateT $ \s -> Left (mconcat [e, "; at ", show s])

parse :: Parser a -> Text -> Either String a
parse = fmap (fmap fst) . runStateT

expectChar :: Parser Char
expectChar = StateT $ \s -> case Text.uncons s of
  Just (c, s') -> Right (c, s')
  Nothing -> runStateT (raise "Expected a character") s

expectExact :: Text -> Parser ()
expectExact ex = case Text.uncons ex of
  Just (ec, ex') -> do
    ch <- expectChar
    if ch == ec
      then expectExact ex'
      else raise ("Expected " ++ show ex)
  Nothing -> pure ()

readRest :: Parser Text
readRest = StateT $ \s -> Right (s, Text.empty)
