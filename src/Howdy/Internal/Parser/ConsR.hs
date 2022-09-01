{-# LANGUAGE ViewPatterns #-}

module Howdy.Internal.Parser.ConsR where

import           Control.Applicative          (Alternative (empty, many, some, (<|>)))
import           Control.Monad                (guard)
import           Control.Monad.Identity       (Identity (runIdentity))
import           Control.Monad.Reader         (ask)
import           Data.Char                    (isSpace)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Howdy.Internal.Parser.TransR (ParserT (..), Result (..),
                                               parser, remainder, runParserT)

type Parser = ParserT Identity

runParser :: Parser a -> Text -> Result a
runParser p = runIdentity . runParserT p

char :: Char -> Parser Char
char c = anyChar `when` (c ==)

notChar :: Char -> Parser Char
notChar c = anyChar `when` (c /=)

whitespace :: Parser Char
whitespace = anyChar `when` isSpace

anyChar :: Parser Char
anyChar = parser go
    where go (T.uncons -> Just (x, xs)) = Success x xs
          go t                          = Fail t

chars :: [Char] -> Parser Char
chars = firstof char

string :: Text -> Parser Text
string (T.uncons -> Nothing)      = pure ""
string (T.uncons -> Just (x, xs)) = do
    h <- char x
    s <- string xs
    pure $ T.singleton h <> s

text :: (Char -> Bool) -> Parser Text
text c = T.pack <$> some (anyChar `unless` c)

word :: Parser Text
word = many whitespace >> text isSpace

flag :: Parser Text
flag = string "--" >> text isSpace

rest :: Parser Text
rest = parser $ flip Success ""

firstof :: (a -> Parser b) -> [a] -> Parser b
firstof f = foldr ((<|>) . f) empty

when :: Parser a -> (a -> Bool) -> Parser a
when p c = parser $ \s ->
    case runParser p s of Fail t      -> Fail t
                          Success a t -> if c a then Success a t else Fail t

unless :: Parser a -> (a -> Bool) -> Parser a
unless p c = when p (not . c)
