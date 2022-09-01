{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Howdy.Internal.Bot.ReactionManager where

import           Discord                    (DiscordHandler)
import           Discord.Types              (Emoji (..), Message (..),
                                             MessageReaction (messageReactionCount, messageReactionEmoji, messageReactionMeIncluded),
                                             ReactionInfo (..), User (..))
import           Howdy.Error                (HowdyException (CommandNotFound, DiscordError, ForbiddenCommand, Ignore, UnknownIdentifier),
                                             report)
import           Howdy.Internal.Bot.Builder (BotPreferences (..))

import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.HashMap.Lazy          ((!?))
import           Data.Text                  (Text)

import           Control.Monad.Catch        (MonadThrow (throwM), catchAll)
import           Control.Monad.Reader       (MonadIO (liftIO),
                                             MonadTrans (lift), ReaderT,
                                             runReaderT, unless, when)
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Discord.Requests           as R
import           Howdy.Internal.Command     (CommandInput (..))
import           Howdy.Internal.Discord     (MonadDiscord (..), request)
import           Howdy.Internal.Reaction    (EmojiIdentifier (..),
                                             ReactionInput (..),
                                             ReactionPreferences (..))

{-
Steps to manage commands:

Requisites:
- A reader (or other kind of environment) that holds commands and reactions
- Error handling/catching
- A way to manipulate errors later on in a similar way to functioning procedure or whatever

  ┌ Discord ───────────┐
  │ Fetch Command      │
  │                    │
  │┌ Discord, Error ──┐│
  ││ Execute command  ││
  │└──────────────────┘│
  │                    │
  │ Handle Error       │
  └────────────────────┘
-}

type Command = String

instance MonadDiscord (ExceptT HowdyException DiscordHandler) where
    liftDiscord = lift

reactionHandler :: ReactionInfo -> BotPreferences -> DiscordHandler ()
reactionHandler r b = do
                  a <- runExceptT $ dealWithReaction r b
                  pure $ either (const ()) id a

dealWithReaction :: ReactionInfo -> BotPreferences -> ExceptT HowdyException DiscordHandler ()
dealWithReaction r b = do -- Match and discard prefix
                       m <- request $ R.GetChannelMessage (r.reactionChannelId, r.reactionMessageId)
                       when (alreadyRan r m.messageReactions) $ throwM ForbiddenCommand

                       -- Get reaction
                       ri <- buildInput m r

                       when (ri.reacter.userIsBot) $ throwM Ignore

                       rp <- fetchReaction b ri.target
                       -- Add reaction
                       request $ R.CreateReaction (r.reactionChannelId, r.reactionMessageId) $ identify r.reactionEmoji

                       -- Check permissions
                       permit rp ri
                       -- Run command
                       rp.runner ri
                       -- Handle errors
                       -- Use cont here ????
                       -- catchE handleError debug
                       --runExceptT $ cmd `catchAll` const pure ()
                       --pure ()

alreadyRan :: ReactionInfo -> [MessageReaction] -> Bool
alreadyRan ri mr = foldr check False mr
    where em = ri.reactionEmoji
          check r acc = r.messageReactionEmoji == em && r.messageReactionMeIncluded

identify :: Emoji -> Text
identify e = case e.emojiId of Just id -> e.emojiName <> ":" <> (T.pack . show $ id)
                               Nothing -> e.emojiName

identifier :: Emoji -> EmojiIdentifier
identifier e = case e.emojiId of Just id -> Custom id
                                 Nothing -> Unicode e.emojiName

fetchReaction :: MonadThrow m => BotPreferences -> EmojiIdentifier -> m ReactionPreferences
fetchReaction b id = do let recs = b.reactions
                        report CommandNotFound $ recs !? id

buildInput :: (MonadThrow m, MonadDiscord m) => Message -> ReactionInfo -> m ReactionInput
buildInput m r = do -- (target, args) <- parse word t
                    user <- request $ R.GetUser r.reactionUserId
                    let author = m.messageAuthor
                        guild = m.messageGuildId
                        channel = m.messageChannelId
                        ref = m.messageReference
                        args = m.messageContent
                        target = identifier r.reactionEmoji
                    pure $ ReactionInput target user author guild channel args ref

permit :: MonadThrow m => ReactionPreferences -> ReactionInput -> m ()
permit p i = unless (p.permission i) $ throwM ForbiddenCommand

errorHandler :: Bool -> HowdyException -> DiscordHandler ()
errorHandler debug = \case -- it has cleaner syntax for many alternatives ok
    DiscordError code -> doNothing
    CommandNotFound   -> doNothing
    ForbiddenCommand  -> doNothing
    _                 -> doNothing

doNothing :: DiscordHandler ()
doNothing = pure ()
