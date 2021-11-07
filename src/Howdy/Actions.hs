{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Howdy.Actions where

import           Control.Monad.State  (MonadState, StateT)
import           Control.Monad.Writer (MonadWriter (tell), Writer, execWriter)
import           Data.Default         (Default (def))
import           Data.Semigroup       (Semigroup)
import           Data.Text            (Text)
import           Discord              (DiscordHandler)
import           Howdy.Parser         (MonadParse)

-- Command context has message/parser as reader (state but it don't matter) around a DiscordHandler output

newtype ActionContext a = ActionContext { runAction :: StateT Text DiscordHandler a }
    deriving (Functor, Applicative, Monad, MonadState Text)

-- There should be some data structure defining parsed data ??

data CommandData = CommandData { getAlias       :: [Text]
                               , getDesc        :: Text
                               , getAction      :: ActionContext ()
                               , getSubcommands :: [CommandData]
                               }

instance Semigroup CommandData where
    a <> b = CommandData
             (getAlias a <> getAlias b)
             (getDesc a <> getDesc b)
             (getAction a >> getAction b)
             (getSubcommands a <> getSubcommands b)

instance Monoid CommandData where
    mempty = CommandData mempty mempty (pure ()) mempty

newtype CommandBuilder a = CommandBuilder { run_CB :: Writer CommandData a}
    deriving (Functor, Applicative, Monad, MonadWriter CommandData)

--newtype BotBuilder a = BotBuilder {run_BB :: Writer BotData a}
--    deriving (Functor, Applicative, Monad, MonadWriter BotData)

--command :: CommandBuilder () -> BotBuilder ()
--command c = tell $ mempty{commands = [execWriter $ run_CB c]}

alias :: Text -> CommandBuilder ()
alias a = tell $ mempty{getAlias = [a]}

aliases :: [Text] -> CommandBuilder ()
aliases a = tell $ mempty{getAlias = a}

desc :: Text -> CommandBuilder ()
desc d = tell $ mempty{getDesc = d}

action :: ActionContext () -> CommandBuilder ()
action a = tell $ mempty{getAction = a}

subcommand :: CommandBuilder () -> CommandBuilder ()
subcommand s = tell $ mempty{getSubcommands = [execWriter $ run_CB s]}

