module Bitmap.Internal.Parsing (
    ParseState (..)
  , Parse (..)
  , (==>)
  , (==>&)
  , bail
  , identity
  , assert
  , readChar
  , readBytes
  , readString
  , readNumber
  , readWhile
  , skipWhile
  , skipSpaces
  ) where

import           Control.Applicative        ((<$>))
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (isDigit, isSpace)
import           Data.Int                   (Int64)

data ParseState = ParseState {
  string :: BS.ByteString,
  offset :: Int64
}

newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
}

instance Functor Parse where
  fmap f s = s ==> (\val -> identity $ f val)

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where chainedParser initState =
          case runParse firstParser initState of
            Left err            -> Left err
            Right (valA, state) -> runParse (secondParser valA) state

(==>&) :: Parse a -> Parse b -> Parse b
firstParser ==>& secondParser = firstParser ==> (\_ -> secondParser)

bail :: String -> Parse a
bail err = Parse (\_ -> Left err)

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

assert :: Bool -> String -> Parse ()
assert cond err =
  if cond
  then identity ()
  else bail err

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState state = Parse (\_ -> Right ((), state))

readChar :: Parse Char
readChar = getState ==> \state ->
  case BS.uncons $ string state of
    Nothing       -> bail "no more input"
    Just (b, str) -> putState newState ==>& identity b
      where newState = state { offset = 1 + offset state, string = str }

-- read exactly N bytes
readBytes :: Int64 -> Parse BS.ByteString
readBytes len = getState ==> \state ->
  if BS.length (string state) < len
  then bail "no more data"
  else let (result, str) = BS.splitAt len (string state)
           newState = state { offset = len + offset state, string = str}
       in putState newState ==>& identity result

readString :: Int64 -> Parse String
readString len = BS.unpack <$> readBytes len

readNumber :: Parse Int
readNumber = (BS.unpack <$> readWhile isDigit) ==> \digits ->
  if null digits
  then bail "no more data"
  else identity (read digits)

readWhile :: (Char -> Bool) -> Parse BS.ByteString
readWhile p = getState ==> \state ->
  let (val, rest) = BS.span p (string state)
      newState = state { offset = BS.length val + offset state, string = rest }
  in putState newState ==>& identity val

skipWhile :: (Char -> Bool) -> Parse ()
skipWhile p = readWhile p ==>& identity ()

skipSpaces :: Parse ()
skipSpaces = skipWhile isSpace
