{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PolyKinds #-}

module Howdy.Internal.Action.Builder where

import           Control.Monad.Catch       (MonadThrow)
import           Control.Monad.Except      (ExceptT, MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.State       (MonadState, StateT)
import           Control.Monad.Writer      (MonadWriter (tell), Writer,
                                            execWriter)
import           Data.Default              (Default (def))
import           Data.Kind                 (Type)
import           Data.Semigroup            (Semigroup)
import           Data.Text                 (Text)
import           Discord                   (DiscordHandler)
import           Discord.Types             (Emoji, GuildId, Message, User,
                                            UserId)
import           Howdy.Discord.Class       (MonadDiscord)
import           Howdy.Error               (HowdyException)
import           Howdy.Internal.Action.Run (CommandRunner, ReactionRunner)
import           Howdy.Internal.Builder    (Builder (..))
import           Howdy.Parser              (MonadParse)

data ActionType = Command | Reaction

newtype ActionBuilder (t :: ActionType) a = ActionBuilder { buildAction :: Writer (ActionBuilderData t) a}
    deriving (Functor, Applicative, Monad, MonadWriter (ActionBuilderData t))

type CommandBuilder = ActionBuilder 'Command
type ReactionBuilder = ActionBuilder 'Reaction

data ActionBuilderData (t :: ActionType) = ActionBuilderData { a_alias      :: [Text]
                                                             , a_desc       :: Text
                                                             , a_runner     :: ActionRunner t ()
                                                             , a_perm       :: UserId  -> GuildId -> Bool
                                                             , a_subactions :: [ActionBuilderData t]
                                                             }

type CommandBuilderData = ActionBuilderData 'Command
type ReactionBuilderData = ActionBuilderData 'Reaction

instance Semigroup (ActionBuilderData t) where
    a <> b = ActionBuilderData
             (a_alias a <> a_alias b)
             (a_desc a <> a_desc b)
             (a_runner a *> a_runner b)
             (\u g -> a_perm a u g && a_perm b u g)
             (a_subactions a <> a_subactions b)

instance Builder (ActionBuilder t a) where
    type BuilderOutput (ActionBuilder t a) = (Text, ActionBuilderData t)
    build a = (head (a_alias d), d)
        where d = execWriter $ buildAction a

instance Monoid (ActionBuilderData t) where
    mempty = ActionBuilderData mempty mempty (pure ()) everyone mempty

type family ActionRunner (t :: ActionType) :: Monad m => m where
    ActionRunner 'Command = CommandRunner
    ActionRunner 'Reaction = ReactionRunner

alias :: Text -> ActionBuilder 'Command ()
alias a = tell $ mempty{a_alias = [a]}

aliases :: [Text] -> ActionBuilder 'Command ()
aliases a = tell $ mempty{a_alias = a}

emoji :: Text -> ActionBuilder 'Reaction ()
emoji a = tell $ mempty{a_alias = [a]}

description :: Text -> ActionBuilder t ()
description d = tell $ mempty{a_desc = d}

run :: ActionRunner t () -> ActionBuilder t ()
run a = tell $ mempty{a_runner = a}

subcommand :: ActionBuilder 'Command () -> ActionBuilder 'Command ()
subcommand s = tell $ mempty{a_subactions = [execWriter $ buildAction s]}

everyone :: a -> b -> Bool
everyone _ _ = True
