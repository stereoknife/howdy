module Howdy.Action ( CommandData (..)
                    , CommandBuilder (..)
                    , CommandRunner
                    , ReactionRunner
                    , alias
                    , aliases
                    , desc
                    , run
                    , subcommand
                    , emoji
                    , emojis
                    ) where

import           Data.Text                     (Text)
import           Discord.Types                 (Emoji)
import           Howdy.Internal.Action.Builder (ActionBuilder (..),
                                                CommandBuilder (..),
                                                CommandData (..),
                                                ReactionBuilder (..))
import           Howdy.Internal.Action.Run     (CommandRunner, ReactionRunner)

-- Global helpers

desc :: (ActionBuilder r m) => Text -> m ()
desc = a_desc

run :: (ActionBuilder r m) => r () -> m ()
run = a_run

-- Command helpers

alias :: Text -> CommandBuilder ()
alias = a_id

aliases :: [Text] -> CommandBuilder ()
aliases = a_ids

subcommand :: CommandBuilder () -> CommandBuilder ()
subcommand = a_sub

-- Reaction helpers

emoji :: Text -> ReactionBuilder ()
emoji = a_id

emojis :: [Text] -> ReactionBuilder ()
emojis = a_ids
