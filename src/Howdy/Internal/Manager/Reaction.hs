{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}


module Howdy.Internal.Manager.Reaction where

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad (unless)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT), runReader)
import Data.Coerce (coerce)
import Data.HashMap.Lazy (empty, (!?))
import Data.Monoid (Any (Any, getAny))
import Data.Text (Text)
import Discord (DiscordHandler, restCall)
import qualified Discord.Requests as R
import Discord.Types (Channel (..), ChannelId, Emoji, Message (..), MessageId, MessageReaction (..),
                      MessageReference (MessageReference), ReactionInfo (..), User (userId), UserId)
import Howdy.Comptime.Bot (BotDefinition (..))
import Howdy.Comptime.Command (CommandDefinition (..), CommandInput (CommandInput, target))
import Howdy.Comptime.Reaction (EmojiIdentifier, ReactionDefinition (..), ReactionInput (..),
                                ReactionReplyData (..), asText, toIdentifier)
import Howdy.Internal.Discord (reactTo, request, unHowdy)
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
    let emId = toIdentifier r.reactionEmoji

    -- Get Command. This prevents other requests from being made if the reaction isn't a command
    com <- fetchReaction b emId
    (user, msg) <- fetchData r

    if hasReacted r.reactionEmoji msg.messageReactions
      then throwM ForbiddenCommand
      else reactTo r.reactionChannelId r.reactionMessageId $ asText r.reactionEmoji

    let ri = buildInput r emId user msg

    -- Check permission
    permit com ri

    let rrd = ReactionReplyData r.reactionChannelId r.reactionUserId
            $ MessageReference (Just r.reactionMessageId) Nothing Nothing True
    let runner = rdRunner com ri

    -- Run command
    runReaderT (unHowdy runner) rrd

type HowdyDebug = ()

debugHandler :: Bool -> HowdyDebug -> DiscordHandler ()
debugHandler = undefined

hasReacted :: Emoji -> [MessageReaction] -> Bool
hasReacted e = getAny . mconcat . fmap (\mr -> coerce $ mr.messageReactionMeIncluded && mr.messageReactionEmoji == e)

fetchReaction :: MonadThrow m => BotDefinition -> EmojiIdentifier -> m ReactionDefinition
fetchReaction b id =
    do let emoji = b.bdEmojis
       let recs = b.bdReactions
       -- id <- report UnknownIdentifier $ emoji !? a
       report CommandNotFound $ recs !? id

fetchData :: ReactionInfo -> DiscordHandler (User, Message)
fetchData ri = do
    user <- request $ R.GetUser ri.reactionUserId
    message <- request $ R.GetChannelMessage (ri.reactionChannelId, ri.reactionMessageId)
    pure (user, message)

buildInput :: ReactionInfo -> EmojiIdentifier -> User -> Message -> ReactionInput
buildInput r e usr msg = do
    let userId = r.reactionUserId
        guildId = r.reactionGuildId
        channelId = r.reactionChannelId
        messageId = r.reactionMessageId
    ReactionInput
      { target  = e
      , reacter = usr
      , author  = msg.messageAuthor
      , guild   = guildId
      , channel = channelId
      , args    = msg.messageContent
      , ref     = msg.messageReference
      }


permit :: MonadThrow m => ReactionDefinition -> ReactionInput -> m ()
permit rd i = unless (rd.rdPermission i) $ throwM ForbiddenCommand
