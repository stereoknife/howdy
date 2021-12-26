{-# LANGUAGE FlexibleContexts #-}

module Howdy.Discord.Class ( module Discord.Types
                           , MonadDiscord (..)
                           , MonadReply (..)
                           ) where

import           Control.Monad.Catch   (MonadThrow)
import           Control.Monad.Except  (MonadError, void)
import           Data.Text             (Text)
import           Discord               (DiscordHandler, FromJSON, restCall)
import           Discord.Internal.Rest (Request)
import qualified Discord.Requests      as R
import           Discord.Types
import           Howdy.Context         (Get (gets))
import           Howdy.Error           (catch)
import           Howdy.Internal.Error  (HowdyException, KnownError)



class (Monad m, MonadError HowdyException m, MonadThrow m) => MonadDiscord m where
    liftDiscord :: DiscordHandler a -> m a
    catchDiscord :: KnownError e => DiscordHandler (Either e a) -> m a
    catchDiscord = catch . liftDiscord

class (Get Message m, Get User m, MonadDiscord m, MonadError HowdyException m) => MonadReply m where
    reply   :: Text -> m ()
    whisper :: Text -> m ()
    react   :: Emoji -> m ()
    embed   :: CreateEmbed -> m ()

    reply t = void $ do
        ch <- gets messageChannel
        catchDiscord $ restCall $ R.CreateMessage ch t

    whisper t = void $ do
        u <- gets userId
        ch <- catchDiscord $ restCall $ R.CreateDM u
        catchDiscord $ restCall $ R.CreateMessage (channelId ch) t

    react e = void $ do
        ch <- gets messageChannel
        mg <- gets messageId
        catchDiscord $ restCall $ R.CreateReaction (ch, mg) $ emojiName e

    embed e = do
        ch <- gets messageChannel
        catchDiscord $ restCall $ R.CreateMessageEmbed ch "" e
        pure ()
