{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Howdy.Bot ( BotData (..)
                 , BotBuilder (..)
                 , command
                 , prefix
                 , prefixes
                 , reaction
                 ) where

import           Howdy.Internal.Bot.Builder (BotBuilder (..), BotData (..),
                                             command, prefix, prefixes,
                                             reaction)
