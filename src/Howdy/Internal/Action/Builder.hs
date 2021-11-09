{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Howdy.Internal.Action.Builder where

import           Control.Monad.Except      (ExceptT, MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.State       (MonadState, StateT)
import           Control.Monad.Writer      (MonadWriter (tell), Writer,
                                            execWriter)
import           Data.Default              (Default (def))
import           Data.Semigroup            (Semigroup)
import           Data.Text                 (Text)
import           Discord                   (DiscordHandler)
import           Discord.Types
import           Howdy.Discord.Class       (Discord)
import           Howdy.Internal.Action.Run (CommandRunner)
import           Howdy.Parser              (MonadParse)

data CommandData = CommandData { getAlias       :: [Text]
                               , getDesc        :: Text
                               , getRunner      :: CommandRunner ()
                               , getSubcommands :: [CommandData]
                               }

instance Semigroup CommandData where
    a <> b = CommandData
             (getAlias a <> getAlias b)
             (getDesc a <> getDesc b)
             (getRunner a >> getRunner b)
             (getSubcommands a <> getSubcommands b)

instance Monoid CommandData where
    mempty = CommandData mempty mempty (pure ()) mempty

newtype CommandBuilder a = CommandBuilder { run_CB :: Writer CommandData a}
    deriving (Functor, Applicative, Monad, MonadWriter CommandData)

alias :: Text -> CommandBuilder ()
alias a = tell $ mempty{getAlias = [a]}

aliases :: [Text] -> CommandBuilder ()
aliases a = tell $ mempty{getAlias = a}

desc :: Text -> CommandBuilder ()
desc d = tell $ mempty{getDesc = d}

action :: CommandRunner () -> CommandBuilder ()
action a = tell $ mempty{getRunner = a}

subcommand :: CommandBuilder () -> CommandBuilder ()
subcommand s = tell $ mempty{getSubcommands = [execWriter $ run_CB s]}

