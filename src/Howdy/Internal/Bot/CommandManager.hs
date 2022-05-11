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

module Howdy.Internal.Bot.CommandManager where

import Discord.Internal.Rest
    ( Message(messageContent, messageChannelId, messageGuildId,
              messageAuthor),
      User(userId) )
import Data.Aeson ( FromJSON )
import Discord (RestCallErrorCode, DiscordHandler, restCall)
import Control.Monad.Free (Free, liftF)
import Data.HashMap.Strict ( HashMap, (!?) )
import Data.Text
import Howdy.Internal.Command
    ( CommandData,
      CommandInput(CommandInput, target),
      CommandPreferences(runner, permission) )
import Control.Monad.Reader
    ( ReaderT, when, MonadReader(ask), unless, asks )
import Howdy.Internal.Bot.Builder
    ( BotData, BotPreferences(prefixes, aliases, commands) )
import Control.Monad.Except ( MonadError(..) )
import Howdy.Error
    ( HowdyException(ForbiddenCommand, Ignore, DiscordError,
                     UnknownIdentifier, CommandNotFound),
      contain,
      report )
import Control.Monad.Trans.Free (FreeT, MonadFree, runFreeT, runFree)
import Discord.Requests (ChannelRequest(CreateMessage, GetChannel))
import Howdy.Internal.Parser.Class ( parse, parseWithError )
import Howdy.Internal.Parser.Cons ( string, word, firstof, flag )
import Howdy.Internal.Discord (MonadDiscord (liftDiscord), DiscordRequest, request)
import GHC.Records ( HasField )


type CommandStore = HashMap Text CommandData
type AliasStore = HashMap Text Text

type BotRunner = ReaderT BotData

commandHandler :: (MonadReader BotPreferences m, MonadDiscord m, MonadError HowdyException m, HasField "runner" CommandPreferences (CommandInput -> m ())) => Message -> m ()
commandHandler m = do b <- ask
                      -- Match and discard prefix
                      (_, rest) <- parseWithError Ignore (firstof string b.prefixes) m.messageContent
                      -- Match and set Debug flag
                      (debug, rest') <- parse (flag "#debug") rest

                      -- Handling error responses here
                      flip catchError (errorHandler debug) $ do
                        -- Get command input
                        ci <- processInput m rest'
                        -- Get command
                        cr <- fetchCommand ci.target
                        -- Check permissions
                        permit cr ci
                        -- Run command
                        cr.runner ci

errorHandler :: (MonadReader BotPreferences m, MonadDiscord m, MonadError HowdyException m) => Bool -> HowdyException -> m ()
errorHandler d (DiscordError code) = when d doNothing
errorHandler d (CommandNotFound) = when d doNothing
errorHandler d (ForbiddenCommand) = when d doNothing
errorHandler d (_) = doNothing

processInput :: (MonadError HowdyException m) => Message -> Text -> m CommandInput
processInput m t = do (target, args) <- parse word t
                      let user = m.messageAuthor.userId
                          guild = m.messageGuildId
                          channel = m.messageChannelId
                      pure $ CommandInput target user guild channel args

fetchCommand :: (MonadReader BotPreferences m, MonadError HowdyException m) => Text -> m CommandPreferences
fetchCommand a = do aliases <- asks (.aliases)
                    coms <- asks (.commands)
                    id <- report UnknownIdentifier $ aliases !? a
                    report CommandNotFound $ coms !? id
                    

permit :: MonadError HowdyException m => CommandPreferences -> CommandInput -> m ()
permit p i = unless (p.permission i) $ throwError ForbiddenCommand


ext :: (MonadReader BotPreferences m, MonadError HowdyException m, MonadDiscord m, FromJSON a) => DiscordRequest a -> m a
ext d = do c <- liftDiscord $ request d
           contain c


-- thing takes discord req & returns readerT of IO

-- convenience alias
doNothing :: Applicative m => m ()
doNothing = pure ()