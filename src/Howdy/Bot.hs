{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Howdy.Bot ( BotData (..)
                 , BotBuilder (..)
                 , Bot (..)
                 , command
                 , prefix
                 , prefixes
                 , reaction
                 , bot
                 , run
                 ) where

import           Howdy.Internal.Bot.Builder (BotBuilder (..), BotData (..),
                                             command, prefix, prefixes,
                                             reaction)

import           Howdy.Internal.Bot.Run     (Bot (..), bot, run)
