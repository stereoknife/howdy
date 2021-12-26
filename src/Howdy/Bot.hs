module Howdy.Bot ( BotData (..)
                 , BotBuilder (..)
                 , Bot (..)
                 , command
                 , prefix
                 , prefixes
                 , reaction
                 , bot
                 ) where

import           Howdy.Internal.Bot.Builder (BotBuilder (..))
import           Howdy.Internal.Bot.Run     (Bot (..), runBot)

bot :: BotBuilder () -> IO ()
bot = runBot . build
