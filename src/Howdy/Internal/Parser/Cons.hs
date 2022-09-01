{-# LANGUAGE ViewPatterns #-}

module Howdy.Internal.Parser.Cons where

import           Control.Applicative         (Alternative (empty, many, some, (<|>)))
import           Control.Monad               (guard)
import           Data.Bifunctor              (first)
import           Data.Char                   (isSpace)
import           Data.Maybe                  (fromMaybe, isJust)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Howdy.Internal.Parser.Types (Parser (..))

char :: Char -> Parser Char
char c = anyChar `when` (c ==)

notChar :: Char -> Parser Char
notChar c = anyChar `when` (c /=)

whitespace :: Parser Char
whitespace = anyChar `when` isSpace

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

text :: (Char -> Bool) -> Parser Text
text c = T.pack <$> some (anyChar `unless` c)

word :: Parser Text
word = many whitespace >> text isSpace

opt :: Parser a -> Parser (Maybe a)
opt p = Parser $ \s ->
            Just $ maybe (Nothing, s) (first Just) $ runParser p s

flag :: Text -> Parser Bool
flag = fmap isJust . opt . string

rest :: Parser Text
rest = Parser $ flip (curry Just) ""

firstof :: (a -> Parser b) -> [a] -> Parser b
firstof f = foldr ((<|>) . f) empty

when :: Parser a -> (a -> Bool) -> Parser a
when p c = Parser $ \s ->
    case runParser p s of Just (v, rest) -> pred (c v) (v, rest)
                          Nothing        -> Nothing
    where pred True  v = Just v
          pred False _ = Nothing

unless :: Parser a -> (a -> Bool) -> Parser a
unless p c = when p (not . c)
