{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Howdy.Bot ( BotData (..)
                 , BotBuilder (..)
                 , Bot (..)
                 , command
                 , prefix
                 , prefixes
                 , reaction
                 , bot
                 ) where

import           Howdy.Internal.Bot.Builder (BotBuilder (..), BotData (..),
                                             command, prefix, prefixes,
                                             reaction)

import           Howdy.Internal.Bot.Run     (Bot (..), mkBot, runBot)

bot :: BotBuilder () -> IO ()
bot = runBot . mkBot
