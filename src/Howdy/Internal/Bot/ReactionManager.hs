{-# LANGUAGE LambdaCase #-}


module Howdy.Internal.Bot.ReactionManager where

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Except (ExceptT, MonadTrans (lift), runExceptT, unless)
import Data.HashMap.Lazy (empty, (!?))
import Data.Text (Text)
import Discord (DiscordHandler, restCall)
import qualified Discord.Requests as R
import Discord.Types (Message (..), ReactionInfo (..))
import Howdy.Comptime.Bot (BotDefinition (..))
import Howdy.Comptime.Command (CommandDefinition (..), CommandInput (CommandInput, target))
import Howdy.Comptime.Reaction (EmojiIdentifier, ReactionDefinition (..), ReactionInput (..),
                                toIdentifier)
import Howdy.Internal.Discord (request)
import Howdy.Internal.Error (HowdyException (CommandNotFound, DiscordError, ForbiddenCommand, Ignore, UnknownIdentifier),
                             report)
import Howdy.Internal.Parser.Class (parse, parseWithError)
import Howdy.Internal.Parser.Cons (firstof, flag, string, word)

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

reactionHandler :: ReactionInfo -> BotDefinition -> DiscordHandler ()
reactionHandler r b = do
    -- Build command input
    ri <- buildInput r $ toIdentifier r.reactionEmoji

    -- Get Command
    rd <- fetchReaction b ri.target

    -- Check permission
    permit rd ri

    -- Run command
    rd.rdRunner ri

type HowdyDebug = ()

debugHandler :: Bool -> HowdyDebug -> DiscordHandler ()
debugHandler = undefined


fetchReaction :: MonadThrow m => BotDefinition -> EmojiIdentifier -> m ReactionDefinition
fetchReaction b id =
    do let emoji = b.bdEmojis
       let recs = b.bdReactions
       -- id <- report UnknownIdentifier $ emoji !? a
       report CommandNotFound $ recs !? id


buildInput :: ReactionInfo -> EmojiIdentifier -> DiscordHandler ReactionInput
buildInput r e = do let userId = r.reactionUserId
                        guildId = r.reactionGuildId
                        channelId = r.reactionChannelId
                        messageId = r.reactionMessageId
                    user <- request $ R.GetUser userId
                    message <- request $ R.GetChannelMessage (channelId, messageId)
                    pure $ ReactionInput e user user guildId channelId message.messageContent message.messageReference


permit :: MonadThrow m => ReactionDefinition -> ReactionInput -> m ()
permit p i = unless (False {- p.permission i -}) $ throwM ForbiddenCommand
