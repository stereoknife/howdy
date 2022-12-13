{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Howdy.Internal.Discord where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader (reader), MonadTrans (lift), Reader, ReaderT,
                             asks, runReader)
import Control.Optics (Lensed (focus), WithLens (look), WithLenses, focus)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf)
import Discord (DiscordHandler, FromJSON, Request, def, restCall)
import qualified Discord.Requests as R
import Discord.Types (Channel (channelId), ChannelId, CreateEmbed, Message, MessageId,
                      MessageReference, UserId)
import GHC.Records (HasField)
import Howdy.Internal.Error (HowdyException, catch)

-- Discord Monad

newtype HowdyHandler r a = HowdyHandler { unHowdy :: ReaderT r DiscordHandler a }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadIO, MonadReader r)

instance Show (HowdyHandler r a) where
    show _ = "HowdyHandler"

-- Idk about these instances but it's alright for now
class Monad m => MonadDiscord m where
    liftDiscord :: DiscordHandler a -> m a

instance MonadDiscord (DiscordHandler) where
    liftDiscord = id

instance MonadDiscord (HowdyHandler r) where
    liftDiscord = HowdyHandler . lift

-- Requests

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

request :: (Request (r a), FromJSON a, MonadThrow m, MonadDiscord m) => r a -> m a
request = catch . liftDiscord . restCall

-- Reply

type MonadReply m = (WithLens ChannelId m, MonadThrow m, MonadDiscord m)

--instance

send :: (WithLens ChannelId m, MonadThrow m, MonadDiscord m) => Text -> m ()
send t = do
    ch <- look -- @ChannelId
    sendTo ch t

sendTo :: (MonadThrow m, MonadDiscord m) => ChannelId -> Text -> m ()
sendTo ch r = do
    request $ R.CreateMessage ch r
    pure ()

embed :: (WithLens ChannelId m, MonadThrow m, MonadDiscord m) => CreateEmbed -> m ()
embed t = do
    ch <- look
    embedTo ch t

embedTo :: (MonadThrow m, MonadDiscord m) => ChannelId -> CreateEmbed -> m ()
embedTo ch t = do
    er <- request $ R.CreateMessageDetailed ch $ def { R.messageDetailedEmbeds = Just [t] }
    pure ()

reply :: (WithLenses '[ChannelId, MessageReference] m, MonadThrow m, MonadDiscord m) => Text -> m ()
reply t = do
    ch <- look
    mg <- look
    replyTo ch mg t

replyTo :: (MonadThrow m, MonadDiscord m) => ChannelId -> MessageReference -> Text -> m ()
replyTo ch mg t = do
    er <- request $ R.CreateMessageDetailed ch $ def { R.messageDetailedReference = (Just mg) }
    pure ()

whisper :: (WithLens UserId m, MonadThrow m, MonadDiscord m) => Text -> m ()
whisper t = do
    us <- look
    whisperTo us t

whisperTo :: (MonadThrow m, MonadDiscord m) => UserId -> Text -> m ()
whisperTo us t = do
    ch <- request $ R.CreateDM us
    er <- request $ R.CreateMessage ch.channelId t
    pure ()

react :: (WithLenses '[ChannelId, MessageId] m, MonadThrow m, MonadDiscord m) => Text -> m ()
react e = do
    ch <- look
    mg <- look
    reactTo ch mg e

reactTo :: (MonadThrow m, MonadDiscord m) => ChannelId -> MessageId -> Text -> m ()
reactTo ch mg e = do
    er <- request $ R.CreateReaction (ch, mg) e
    pure ()

printLog :: ChannelId -> Text -> DiscordHandler ()
printLog ch t = restCall (R.CreateMessage ch $ "```" <> t <> "```") >> pure ()

-- upload
