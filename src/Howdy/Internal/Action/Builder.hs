{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Howdy.Internal.Action.Builder where

import           Control.Monad.Catch       (MonadThrow)
import           Control.Monad.Except      (ExceptT, MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.State       (MonadState, StateT)
import           Control.Monad.Writer      (MonadWriter (tell), Writer,
                                            execWriter)
import           Data.Default              (Default (def))
import           Data.Kind                 (Type, Constraint)
import           Data.Semigroup            (Semigroup)
import           Data.Text                 (Text)
import           Discord                   (DiscordHandler)
import           Discord.Types             (Emoji, GuildId, Message, User,
                                            UserId)
import           Howdy.Effects.Discord       (Discord, Reply)
import           Howdy.Error               (HowdyException)
import           Howdy.Internal.Builder    (Builder (..))
import           Howdy.Parser              (Parse)
import Howdy.Context (Gets)

data ActionType = Command | Reaction

newtype ActionBuilder (t :: ActionType) a = ActionBuilder { buildAction :: Writer (ActionBuilderData t) a}
    deriving (Functor, Applicative, Monad, MonadWriter (ActionBuilderData t))

type CommandBuilder = ActionBuilder 'Command
type ReactionBuilder = ActionBuilder 'Reaction

data ActionBuilderData (t :: ActionType) =
    ActionBuilderData { a_alias      :: [Text]                                          -- ^ List of aliases for a command, first one is default
                      , a_desc       :: Text                                            -- ^ Description, it will show in help
                      , a_runner     :: DiscordHandler ()
                      , a_perm       :: UserId  -> GuildId -> Bool                      -- ^ Permissions function, returns true or false when given a user and a guild
                      , a_subactions :: [ActionBuilderData t]                           -- ^ Subcommands (not used for reactions)
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

-- | Add an alias to a command, it appends to existing aliases
alias :: Text -> ActionBuilder 'Command ()
alias a = tell $ mempty{a_alias = [a]}

-- | Add multiple aliases to a command, it appends to existing aliases
aliases :: [Text] -> ActionBuilder 'Command ()
aliases a = tell $ mempty{a_alias = a}

-- | Add emoji that will trigger a reaction
emoji :: Text -> ActionBuilder 'Reaction ()
emoji a = tell $ mempty{a_alias = [a]}

-- | Set description of command or reaction, it will show when the help command is used
description :: Text -> ActionBuilder t ()
description d = tell $ mempty{a_desc = d}

-- | Set the command or reaction to run
run :: DiscordHandler () -> ActionBuilder t ()
run a = tell $ mempty{a_runner = a}

-- | Add a subcommand
subcommand :: ActionBuilder 'Command () -> ActionBuilder 'Command ()
subcommand s = tell $ mempty{a_subactions = [execWriter $ buildAction s]}

-- | Permission function that always returns True
everyone :: a -> b -> Bool
everyone _ _ = True
