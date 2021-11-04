{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Howdy.Parser.Types ( Parser (..), runParser ) where

import           Control.Applicative (Alternative (..))
import           Data.Text           (Text)


newtype Parser a = Parser { runParser :: Text -> Maybe (a, Text) }

mpair :: (a -> c) -> (a, b) -> (c, b)
mpair f (a, b) = (f a, b)

instance Functor Parser where
    fmap f mp = Parser $ fmap (mpair f) . runParser mp

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    fm <*> m = Parser $ \s -> do
        (f, next) <- runParser fm s
        (a, rest) <- runParser m next
        Just (f a, rest)

instance Monad Parser where
    return = pure
    m >>= f = Parser $ \s -> do
        (a, next) <- runParser m s
        (b, rest) <- runParser (f a) next
        Just (b, rest)

instance Alternative Parser where
    empty = Parser $ const Nothing
    a <|> b = Parser $ \s -> do
        runParser a s <|> runParser b s

instance Semigroup a => Semigroup (Parser a) where
    p1 <> p2 = do
        r1 <- p1
        r2 <- p2
        return $ r1 <> r2

instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty

{-

opt :: Monad m => ParserT m a -> ParserT m (Maybe a)
opt mp = ParserT $ \s -> do
    p <- runParserT mp s
    case p of Nothing        -> pure $ Just (Nothing, s)
              Just (a, rest) -> pure $ Just (Just a, rest)

class Monad m => Parse m where
    parse :: ParserT m a -> ParserT m a

instance Monad m => Parse (ParserT m) where
    parse p = ParserT $ \s -> do
        runParserT p s

-}




