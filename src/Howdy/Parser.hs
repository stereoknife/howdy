{-# LANGUAGE ViewPatterns #-}

module Howdy.Parser ( module Howdy.Parser.Types
                    , char
                    , notChar
                    , anyChar
                    , chars
                    , string
                    , text
                    , word
                    , flag
                    , rest
                    , firstof
                    ) where

import           Control.Applicative (Alternative (empty, many, (<|>)))
import           Control.Monad       (guard)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Howdy.Parser.Types  (Parser (..))

char :: Char -> Parser Char
char c = Parser go
    where go (T.uncons -> Just (x, xs)) = guard (x == c) >> Just (x, xs)
          go _                          = Nothing

notChar :: Char -> Parser Char
notChar c = Parser go
    where go (T.uncons -> Just (x, xs)) = guard (x /= c) >> Just (x, xs)
          go _                          = Nothing

anyChar :: Parser Char
anyChar = Parser go
    where go (T.uncons -> Just (x, xs)) = Just (x, xs)
          go _                          = Nothing

chars :: [Char] -> Parser Char
chars = firstof char

string :: Text -> Parser Text
string (T.uncons -> Nothing)      = pure ""
string (T.uncons -> Just (x, xs)) = do
    h <- char x
    s <- string xs
    pure $ T.singleton h <> s

text :: Char -> Parser Text
text c = T.pack <$> many (notChar c)

word :: Parser Text
word = many (char ' ') >> text ' '

flag :: Parser Text
flag = string "--" >> text ' '

rest :: Parser Text
rest = Parser $ flip (curry Just) ""

firstof :: (a -> Parser b) -> [a] -> Parser b
firstof f = foldr ((<|>) . f) empty
