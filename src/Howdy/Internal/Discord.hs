{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Howdy.Internal.Discord where

import Discord
    ( restCall, DiscordHandler, RestCallErrorCode, FromJSON )
import Control.Monad.Trans ( MonadTrans(..) )
import Control.Monad.Reader (ReaderT(runReaderT))
import Data.Typeable (Typeable, typeOf)
import Discord.Internal.Rest (Request)

-- runDiscordHandler :: DiscordHandler a -> DiscordHandle -> IO a
-- runDiscordHandler m r = runReaderT m r

-- newtype DiscordT m a = DiscordT { runDiscordT :: m (DiscordHandler a) }

-- instance MonadTrans DiscordT where
--     lift = DiscordT . fmap pure

-- instance Functor (DiscordT m) where
-- instance Applicative (DiscordT m) where
-- instance Monad (DiscordT m) where

class Monad m => MonadDiscord m where
    liftDiscord :: DiscordHandler a -> m a

instance (MonadTrans m, Monad (m DiscordHandler)) => MonadDiscord (m DiscordHandler) where
    liftDiscord = lift

data DiscordRequest a where
    DiscordRequest :: (FromJSON a, Request (r a), Typeable a) => r a -> DiscordRequest a

showInner :: forall a. Typeable a => DiscordRequest a -> String
showInner _ = show $ typeOf (undefined :: a)

instance Typeable a => Show (DiscordRequest a) where
    show a = show (typeOf a)

elimRequest :: (forall r a. (FromJSON a, Request (r a)) => r a -> m a) -> DiscordRequest a -> m a
elimRequest f (DiscordRequest a) = f a

-- This is here because of discord-haskell's calcified DiscordHandler (Either RestCallErrorCode a) return type
elimRequestCalcified :: (forall r a. (FromJSON a, Request (r a)) => r a -> m (n a)) -> DiscordRequest a -> m (n a)
elimRequestCalcified f (DiscordRequest a) = f a

request :: forall a. FromJSON a => DiscordRequest a -> DiscordHandler (Either RestCallErrorCode a)
request = elimRequestCalcified restCall