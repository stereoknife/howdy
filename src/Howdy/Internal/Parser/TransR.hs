{-# LANGUAGE RankNTypes #-}
module Howdy.Internal.Parser.TransR
       --{-# WARNING "Unused, this is here in case it's needed in the future again." #-}
       (ParserT (..), parser, Result (..), runParserT, remainder) where

import           Control.Applicative  (Alternative (..))
import           Control.Monad.Reader (MonadReader (ask, reader),
                                       ReaderT (runReaderT))
import           Control.Monad.Trans  (MonadTrans (..))
import           Data.Default         (Default (def))
import           Data.Text            (Text)
import           GHC.Base             (Applicative)

data Result a = Success a Text
              | Fail Text
              deriving (Show, Eq)

remainder :: Result a -> Text
remainder (Success _ t) = t
remainder (Fail t)      = t

instance Functor Result where
    fmap f (Success a t) = Success (f a) t
    fmap f (Fail t)      = Fail t

instance Applicative Result where
    pure a = Success a ""
    (Success f _) <*> (Success a t) = Success (f a) t
    (Fail t) <*> _                  = Fail t
    _ <*> (Fail t)                  = Fail t

instance Monad Result where
    return = pure
    (Success a t) >>= m = m a
    (Fail t) >>= m      = Fail t

instance Alternative Result where
    empty = Fail ""
    (Success a t) <|> _ = Success a t
    _ <|> (Success a t) = Success a t
    a <|> _             = a

newtype ParserT m a = ParserT { ruParserT :: ReaderT Text m (Result a) }

parser :: Monad m => (Text -> Result a) -> ParserT m a
parser = ParserT . reader

runParserT :: ParserT m a -> Text -> m (Result a)
runParserT = runReaderT . ruParserT

instance Monad m => Functor (ParserT m) where
    fmap f (ParserT m) = ParserT $ fmap f <$> m

instance Monad m => Applicative (ParserT m) where
    pure a = ParserT $ reader $ \s -> Success a s -- might be worng, result should have all the text in reader
    -- Big HLS-simplified nonsense thing from applying mpair to f and that to m
    (ParserT f) <*> (ParserT m) = ParserT $ (<*>) <$> f <*> m

instance Monad m => Monad (ParserT m) where
    return = pure
    m >>= f = ParserT $ do
        a <- ruParserT m
        case a of Success a t -> lift $ runParserT (f a) t
                  Fail t      -> pure $ Fail t

instance Monad m => Alternative (ParserT m) where
    empty = parser $ \s -> Fail s
    ma <|> mb = ParserT $ do
        s <- ask
        let a = runParserT ma s
            b = runParserT mb s
        lift $ (<|>) <$> a <*> b

instance (Semigroup a, Monad m) => Semigroup (ParserT m a) where
    p1 <> p2 = do
        r1 <- p1
        r2 <- p2
        pure $ r1 <> r2

instance (Monoid a, Monad m) => Monoid (ParserT m a) where
  mempty = pure mempty

instance MonadTrans ParserT where
    lift ma = ParserT $ do
        lift $ fmap pure ma

class Monad m => Parse m where
    parse :: ParserT m a -> ParserT m a

instance Monad m => Parse (ParserT m) where
    parse p = ParserT $ do
        s <- ask
        lift $ runParserT p s



