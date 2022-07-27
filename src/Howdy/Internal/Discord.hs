{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Howdy.Internal.Discord where

import Discord ( def, restCall, DiscordHandler, Request, FromJSON )
import Control.Monad.Trans ( MonadTrans(..) )
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader, asks)
import Data.Typeable (Typeable, typeOf)
import Data.Text (Text)
import GHC.Records (HasField)
import Control.Monad.Except (MonadError)
import Howdy.Internal.Error (HowdyException, catch)
import qualified Discord.Requests as R
import Data.Kind (Constraint, Type)
import Discord.Types
    ( Channel(channelId),
      MessageReference,
      CreateEmbed,
      ChannelId,
      UserId, Message )
import Control.Optics (MonadLens, get, MonadLenses, WithLens (focus))
import Optics (to, castOptic, (%), view)

class Monad m => MonadDiscord m where
    liftDiscord :: DiscordHandler a -> m a

-- instance (MonadTrans m, Monad (m DiscordHandler)) => MonadDiscord (m DiscordHandler) where
--     liftDiscord = lift

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

-- request :: forall a. FromJSON a => DiscordRequest a -> DiscordHandler (Either RestCallErrorCode a)
-- request = elimRequestCalcified restCall

request :: (Request (r a), FromJSON a, MonadError HowdyException m, MonadDiscord m) => r a -> m a
request = catch . liftDiscord . restCall

type MonadReply m = (MonadLens ChannelId m, MonadError HowdyException m, MonadDiscord m)

--instance 

send :: (MonadLens ChannelId m, MonadError HowdyException m, MonadDiscord m) => Text -> m ()
send t = do
    ch <- get -- @ChannelId
    er <- request $ R.CreateMessage ch t
    pure ()

embed :: (MonadLens ChannelId m, MonadError HowdyException m, MonadDiscord m) => CreateEmbed -> m ()
embed t = do
    ch <- get
    er <- request $ R.CreateMessageDetailed ch $ def { R.messageDetailedEmbeds = Just [t] }
    pure ()

reply :: (MonadLenses '[ChannelId, MessageReference] m, MonadError HowdyException m, MonadDiscord m) => Text -> m ()
reply t = do
    ch <- get
    mg <- get
    er <- request $ R.CreateMessageDetailed ch $ def { R.messageDetailedReference = (Just mg) }
    pure ()

whisper :: (MonadLens UserId m, MonadError HowdyException m, MonadDiscord m) => Text -> m ()
whisper t = do
    us <- get
    ch <- request $ R.CreateDM us
    er <- request $ R.CreateMessage ch.channelId t
    pure ()

-- TODO: Add embed, react and whisper

-- react

-- upload