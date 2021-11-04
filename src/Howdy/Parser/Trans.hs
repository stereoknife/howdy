module Howdy.Parser.Trans where

import           Control.Applicative (Alternative (..))
import           Control.Monad.Trans (MonadTrans (..))
import           Data.Default        (Default (def))
import           Data.Text           (Text)
import           GHC.Base            (Applicative)


newtype ParserT m a = ParserT { runParserT :: Text -> m (Maybe (a, Text)) }

mpair :: (a -> c) -> (a, b) -> (c, b)
mpair f (a, b) = (f a, b)

instance Functor m => Functor (ParserT m) where
    fmap f mp = ParserT $ fmap (fmap (mpair f)) . runParserT mp

instance Applicative m => Applicative (ParserT m) where
    pure a = ParserT $ \s -> pure $ Just (a, s)
    -- Big HLS-simplified nonsense thing from applying mpair to f and that to m
    f <*> m = ParserT $ \s -> ((<*>) . fmap mpair) . fmap fst <$> runParserT f s <*> runParserT m s

instance Monad m => Monad (ParserT m) where
    return = pure
    m >>= f = ParserT $ \s -> do
        m' <- runParserT m s
        case m' of Just (a, rest) -> runParserT (f a) rest
                   Nothing        -> pure Nothing

instance Monad m => Alternative (ParserT m) where
    empty = ParserT $ const $ pure Nothing
    ma <|> mb = ParserT $ \s -> do
        a <- runParserT ma s
        b <- runParserT mb s
        pure $ a <|> b

instance (Semigroup a, Monad m) => Semigroup (ParserT m a) where
    p1 <> p2 = do
        r1 <- p1
        r2 <- p2
        return $ r1 <> r2

instance (Monoid a, Monad m) => Monoid (ParserT m a) where
  mempty = pure mempty

instance MonadTrans ParserT where
    lift ma = ParserT $ \s -> do
        a <- ma
        pure $ Just (a, s)


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



