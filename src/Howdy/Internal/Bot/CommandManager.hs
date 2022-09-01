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

module Howdy.Internal.Bot.CommandManager where
import           Control.Monad               (unless)
import           Control.Monad.Catch         (MonadThrow (throwM), catchAll)
import           Control.Monad.Reader        (MonadTrans (lift), ReaderT,
                                              runReaderT)
import           Control.Monad.Trans.Except  (ExceptT, runExceptT)
import           Data.HashMap.Lazy           ((!?))
import           Data.Text                   (Text)
import           Discord                     (DiscordHandler)
import           Discord.Types               (Message (messageAuthor, messageChannelId, messageContent, messageGuildId, messageReference))
import           Howdy.Error                 (HowdyException (CommandNotFound, DiscordError, ForbiddenCommand, Ignore, UnknownIdentifier),
                                              MonadError (..), report)
import           Howdy.Internal.Bot.Builder  (BotPreferences (aliases, commands, prefixes))
import           Howdy.Internal.Command      (CommandInput (..),
                                              CommandPreferences (..))
import           Howdy.Internal.Parser.Class (parse, parseWithError)
import           Howdy.Internal.Parser.Cons  (firstof, flag, string, word)

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
                         let cmd = do
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
                         --runExceptT $ cmd `catchAll` const pure ()
                         --pure ()
                         lift $ do
                                e <- runExceptT cmd
                                case e of Left e' -> errorHandler debug e'
                                          Right _ -> pure ()

fetchCommand :: MonadThrow m => BotPreferences -> Text -> m CommandPreferences
fetchCommand b a = do let aliases = b.aliases
                          coms = b.commands
                      id <- report UnknownIdentifier $ aliases !? a
                      report CommandNotFound $ coms !? id

buildInput :: MonadThrow m => Message -> Text -> m CommandInput
buildInput m t = do (target, args) <- parse word t
                    let user = m.messageAuthor
                        guild = m.messageGuildId
                        channel = m.messageChannelId
                        ref = m.messageReference
                    pure $ CommandInput target user guild channel args ref

permit :: MonadThrow m => CommandPreferences -> CommandInput -> m ()
permit p i = unless (p.permission i) $ throwM ForbiddenCommand

errorHandler :: Bool -> HowdyException -> DiscordHandler ()
errorHandler debug = \case -- it has cleaner syntax for many alternatives ok
    DiscordError code -> doNothing
    CommandNotFound   -> doNothing
    ForbiddenCommand  -> doNothing
    _                 -> doNothing

doNothing :: DiscordHandler ()
doNothing = pure ()
