{-# LANGUAGE RankNTypes #-}
module Howdy.Internal.Parser.Trans
       {-# WARNING "Unused, this is here in case it's needed in the future again." #-}
       () where

import           Control.Applicative (Alternative (..))
import           Control.Monad.Trans (MonadTrans (..))
import           Data.Default        (Default (def))
import           Data.Text           (Text)
import           GHC.Base            (Applicative)

data Result a = Success a Text
              | Fail Text

rest :: Result a -> Text
rest (Success _ t) = t
rest (Fail t)      = t

instance Functor Result where
    fmap f (Success a t) = Success (f a) t
    fmap f (Fail t) = Fail t

instance Applicative Result where
    pure a = Success a ""
    (Success f _) <*> (Success a t) = Success (f a) t
    (Fail t) <*> _ = Fail t
    _ <*> (Fail t) = Fail t

instance Monad Result where
    return = pure
    (Success a t) >>= m = m a
    (Fail t) >>= m = Fail t

instance Alternative Result where
    empty = Fail ""
    (Success a t) <|> _ = Success a t
    _ <|> (Success a t) = Success a t
    a <|> _ = a

newtype ParserT m a = ParserT { runParserT :: Monad m => Text -> m (Result a) }

instance Functor m => Functor (ParserT m) where
    fmap f mp = ParserT $ \s -> do
        r <- runParserT mp s
        case r of Success a t -> pure $ Success (f a) t
                  Fail t -> pure $ Fail t

instance Applicative m => Applicative (ParserT m) where
    pure a = ParserT $ \s -> pure $ Success a s
    -- Big HLS-simplified nonsense thing from applying mpair to f and that to m
    f <*> m = ParserT $ \s -> do
        f' <- runParserT f s
        m' <- runParserT m (rest f')
        pure $ f' <*> m'


instance Monad m => Monad (ParserT m) where
    return = pure
    m >>= f = ParserT $ \s -> do
        m' <- runParserT m s
        case m' of Success a t -> runParserT (f a) t
                   Fail t        -> pure $ Fail t

instance Monad m => Alternative (ParserT m) where
    empty = ParserT $ pure . Fail
    ma <|> mb = ParserT $ \s -> do
        a <- runParserT ma s
        b <- runParserT mb s
        pure $ a <|> b

instance (Semigroup a, Monad m) => Semigroup (ParserT m a) where
    p1 <> p2 = do
        r1 <- p1
        r2 <- p2
        pure $ r1 <> r2

instance (Monoid a, Monad m) => Monoid (ParserT m a) where
  mempty = pure mempty

instance MonadTrans ParserT where
    lift ma = ParserT $ \s -> do
        a <- ma
        pure $ Success a s

class Monad m => Parse m where
    parse :: ParserT m a -> ParserT m a

instance Monad m => Parse (ParserT m) where
    parse p = ParserT $ \s -> do
        runParserT p s


