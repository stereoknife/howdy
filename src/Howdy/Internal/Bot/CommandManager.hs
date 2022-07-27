{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Howdy.Internal.Bot.CommandManager where
import Discord ( DiscordHandler )
import Discord.Types
    ( Message(messageContent, messageReference, messageChannelId,
              messageGuildId, messageAuthor) )
import Howdy.Internal.Bot.Builder
    ( BotPreferences(prefixes, commands, aliases) )
import Howdy.Error
    ( MonadError(..),
      HowdyException(ForbiddenCommand, Ignore, UnknownIdentifier,
                     DiscordError, CommandNotFound),
      report )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )
import Data.Text (Text)
import Howdy.Internal.Parser.Class ( parse, parseWithError )
import Howdy.Internal.Parser.Cons
    ( firstof, flag, string, word )
import Howdy.Internal.Command
    ( CommandInput(..),
      CommandPreferences(..) )
import Data.HashMap.Lazy ((!?))
import Control.Monad (unless)
import Control.Monad.Reader (runReaderT, ReaderT, MonadTrans (lift))

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

commandHandler :: Message -> BotPreferences -> DiscordHandler ()
commandHandler m b = do
                  a <- runExceptT $ dealWithCommand m b
                  pure $ either (const ()) id a

dealWithCommand :: Message -> BotPreferences -> ExceptT HowdyException DiscordHandler ()
dealWithCommand m b = do -- Match and discard prefix
                         (_, rest) <- parseWithError Ignore (firstof string b.prefixes) m.messageContent
                         -- Match and set Debug flag
                         (debug, rest') <- parse (flag "#debug") rest
                         -- Get command input
                         let err = runExceptT $ do
                                   ci <- buildInput m rest'
                                   -- Get command
                                   cr <- fetchCommand b ci.target
                                   -- Check permissions
                                   permit cr ci
                                   -- Run command
                                   cr.runner ci
                                   -- Handle errors
                                   -- Use cont here ????
                                   -- catchE handleError debug
                         lift $ do
                                e <- err
                                case e of Left e' -> errorHandler debug e'
                                          Right _ -> pure ()

fetchCommand :: MonadError HowdyException m => BotPreferences -> Text -> m CommandPreferences
fetchCommand b a = do let aliases = b.aliases
                          coms = b.commands
                      id <- report UnknownIdentifier $ aliases !? a
                      report CommandNotFound $ coms !? id

buildInput :: (MonadError HowdyException m) => Message -> Text -> m CommandInput
buildInput m t = do (target, args) <- parse word t
                    let user = m.messageAuthor
                        guild = m.messageGuildId
                        channel = m.messageChannelId
                        ref = m.messageReference
                    pure $ CommandInput target user guild channel args ref

permit :: MonadError HowdyException m => CommandPreferences -> CommandInput -> m ()
permit p i = unless (p.permission i) $ throwError ForbiddenCommand

errorHandler :: Bool -> HowdyException -> DiscordHandler ()
errorHandler debug = \case -- it has cleaner syntax for many alternatives ok
    DiscordError code -> doNothing
    CommandNotFound -> doNothing
    ForbiddenCommand -> doNothing
    _ -> doNothing

doNothing :: DiscordHandler ()
doNothing = pure ()