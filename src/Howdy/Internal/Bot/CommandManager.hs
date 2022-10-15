{-# LANGUAGE LambdaCase #-}


module Howdy.Internal.Bot.CommandManager where

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Except (ExceptT, MonadTrans (lift), runExceptT, unless)
import Data.HashMap.Lazy (empty, (!?))
import Data.Text (Text)
import Discord (DiscordHandler)
import Discord.Types (Message (messageAuthor, messageChannelId, messageContent, messageGuildId, messageReference))
import Howdy.Comptime.Bot (BotDefinition (..))
import Howdy.Comptime.Command (CommandDefinition (..), CommandInput (CommandInput, target))
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

type Command = String


commandHandler :: Message -> BotDefinition -> DiscordHandler ()
commandHandler m b = do
    -- Match and discard prefix
    (_, rest) <- parseWithError Ignore (firstof string b.bdPrefixes) m.messageContent

    -- Match and set Debug flag
    (debug, rest') <- parse (flag "#debug") rest

    -- Build command input
    ci <- buildInput m rest'

    -- Get Command
    cr <- fetchCommand b ci.target

    -- Check permission
    permit cr ci

    -- Run command
    cr.cdRunner ci


type HowdyDebug = ()

debugHandler :: Bool -> HowdyDebug -> DiscordHandler ()
debugHandler = undefined


fetchCommand :: MonadThrow m => BotDefinition -> Text -> m CommandDefinition
fetchCommand b a = do let aliases = b.bdAliases
                          coms = b.bdCommands
                      id <- report UnknownIdentifier $ aliases !? a
                      report CommandNotFound $ coms !? id


buildInput :: MonadThrow m => Message -> Text -> m CommandInput
buildInput m t = do (target, args) <- parse word t
                    let user = m.messageAuthor
                        guild = m.messageGuildId
                        channel = m.messageChannelId
                        ref = m.messageReference
                    pure $ CommandInput target user guild channel args ref

permit :: MonadThrow m => CommandDefinition -> CommandInput -> m ()
permit p i = unless (False {- p.permission i -}) $ throwM ForbiddenCommand

doNothing :: DiscordHandler ()
doNothing = pure ()
