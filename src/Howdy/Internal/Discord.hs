{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Howdy.Internal.Discord where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader (reader), Reader, asks, runReader)
import Control.Monad.Trans (MonadTrans (..))
import Control.Optics (Lensed (focus), WithLens (view), WithLenses, focus)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf)
import Discord (DiscordHandler, FromJSON, Request, def, restCall)
import qualified Discord.Requests as R
import Discord.Types (Channel (channelId), ChannelId, CreateEmbed, Message, MessageReference,
                      UserId)
import GHC.Records (HasField)
import Howdy.Internal.Error (HowdyException, catch)

-- Discord Monad

newtype HowdyHandler r i a = HowdyHandler { unHowdy :: Reader r (i -> DiscordHandler a) }
    deriving (Functor)

instance Applicative (HowdyHandler r i) where
    pure = HowdyHandler . pure . const . pure
    (HowdyHandler f) <*> (HowdyHandler a) = HowdyHandler $ go <$> f <*> a
        where go f a ci = (f ci) <*> (a ci)

instance Monad (HowdyHandler r i) where
    return = pure
    (HowdyHandler m) >>= f = HowdyHandler $ reader $ \r -> do
        let readM = runReader m r
        let readF = flip runReader r . unHowdy . f
        readM `go` readF

        where go :: (i -> DiscordHandler a) -> (a -> i -> DiscordHandler b) -> i -> DiscordHandler b
              go a b i = a' >>= b'
                where a' = a i
                      b' = flip b i

class Monad m => MonadDiscord m where
    liftDiscord :: DiscordHandler a -> m a

instance MonadDiscord DiscordHandler where
     liftDiscord = id

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
    ch <- view -- @ChannelId
    er <- request $ R.CreateMessage ch t
    pure ()

embed :: (WithLens ChannelId m, MonadThrow m, MonadDiscord m) => CreateEmbed -> m ()
embed t = do
    ch <- view
    er <- request $ R.CreateMessageDetailed ch $ def { R.messageDetailedEmbeds = Just [t] }
    pure ()

reply :: (WithLenses '[ChannelId, MessageReference] m, MonadThrow m, MonadDiscord m) => Text -> m ()
reply t = do
    ch <- view
    mg <- view
    er <- request $ R.CreateMessageDetailed ch $ def { R.messageDetailedReference = (Just mg) }
    pure ()

whisper :: (WithLens UserId m, MonadThrow m, MonadDiscord m) => Text -> m ()
whisper t = do
    us <- view
    ch <- request $ R.CreateDM us
    er <- request $ R.CreateMessage ch.channelId t
    pure ()

printLog :: ChannelId -> Text -> DiscordHandler ()
printLog ch t = restCall (R.CreateMessage ch $ "```" <> t <> "```") >> pure ()

-- TODO: Add embed, react and whisper

-- react

-- upload
