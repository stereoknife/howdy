{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

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
import           Discord.Types             (Emoji)
import           Howdy.Discord.Class       (Discord)
import           Howdy.Internal.Action.Run (CommandRunner, ReactionRunner)
import           Howdy.Parser              (MonadParse)

data CommandData = CommandData { getAlias       :: [Text]
                               , getDesc        :: Text
                               , getRunner      :: CommandRunner ()
                               , getSubcommands :: [CommandData]
                               }

data ReactionData = ReactionData { reactionEmoji  :: [Text]
                                 , reactionDesc   :: Text
                                 , reactionRunner :: ReactionRunner ()
                                 }

instance Semigroup CommandData where
    a <> b = CommandData
             (getAlias a <> getAlias b)
             (getDesc a <> getDesc b)
             (getRunner a >> getRunner b)
             (getSubcommands a <> getSubcommands b)

instance Semigroup ReactionData where
    a <> b = ReactionData
             (reactionEmoji a <> reactionEmoji b)
             (reactionDesc a <> reactionDesc b)
             (reactionRunner a >> reactionRunner b)

instance Monoid CommandData where
    mempty = CommandData mempty mempty (pure ()) mempty

instance Monoid ReactionData where
    mempty = ReactionData mempty mempty (pure ())

newtype CommandBuilder a = CommandBuilder { run_CB :: Writer CommandData a}
    deriving (Functor, Applicative, Monad, MonadWriter CommandData)

newtype ReactionBuilder a = ReactionBuilder { run_RB :: Writer ReactionData a}
    deriving (Functor, Applicative, Monad, MonadWriter ReactionData)

-- Command Functions

class ActionBuilder r m | m -> r where
    a_id :: Text -> m ()
    a_ids :: [Text] -> m ()
    a_desc :: Text -> m ()
    a_run :: r () -> m ()
    a_sub :: m () -> m ()

instance ActionBuilder CommandRunner CommandBuilder where
    a_id a = tell $ mempty{getAlias = [a]}
    a_ids a = tell $ mempty{getAlias = a}
    a_desc d = tell $ mempty{getDesc = d}
    a_run r = tell $ mempty{getRunner = r}
    a_sub m = tell $ mempty{getSubcommands = [execWriter $ run_CB m]}

instance ActionBuilder ReactionRunner ReactionBuilder where
    a_id i = tell $ mempty{reactionEmoji = [i]}
    a_ids i = tell $ mempty{reactionEmoji = i}
    a_desc d = tell $ mempty{reactionDesc = d}
    a_run r = tell $ mempty{reactionRunner = r}
    a_sub m = pure ()

-- these should be a typeclass

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


