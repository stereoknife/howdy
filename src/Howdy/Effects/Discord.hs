{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeApplications #-}

module Howdy.Effects.Discord ( module Discord.Types
                             , Discord (..)
                             , Reply (..)
                             ) where

import           Control.Monad.Catch   (MonadThrow)
import           Control.Monad.Except  (MonadError, void)
import           Data.Text             (Text)
import           Discord               (DiscordHandler, FromJSON, restCall)
import           Discord.Internal.Rest (Request)
import qualified Discord.Requests      as R
import           Discord.Types
import           Howdy.Context         (Get (get), Gets)
import           Howdy.Error           (catch)
import           Howdy.Internal.Error  (HowdyException, KnownError)



class (Monad m, MonadError HowdyException m) => Discord m where
    liftDiscord :: DiscordHandler a -> m a
    catchDiscord :: KnownError e => DiscordHandler (Either e a) -> m a
    catchDiscord = catch . liftDiscord

class (Gets [ChannelId, UserId, MessageId] m, Discord m, MonadError HowdyException m) => Reply m where
    reply   :: Text -> m ()
    whisper :: Text -> m ()
    react   :: Emoji -> m ()
    embed   :: CreateEmbed -> m ()

    reply t = void $ do
        ch <- get @ChannelId
        catchDiscord $ restCall $ R.CreateMessage ch t

    whisper t = void $ do
        u <- get @UserId 
        ch <- catchDiscord $ restCall $ R.CreateDM u
        catchDiscord $ restCall $ R.CreateMessage (channelId ch) t

    react e = void $ do
        ch <- get @ChannelId 
        mg <- get @MessageId 
        catchDiscord $ restCall $ R.CreateReaction (ch, mg) $ emojiName e

    embed e = do
        ch <- get @ChannelId
        catchDiscord $ restCall $ R.CreateMessageEmbed ch "" e
        pure ()
