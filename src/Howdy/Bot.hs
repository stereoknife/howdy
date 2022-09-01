module Howdy.Bot ( B.prefixes
                 , B.command
                 , B.token
                 , B.author
                 , B.repo
                 , B.note
                 , B.name
                 , B.reaction
                 , L.start
                 , bot
                 ) where

import           Howdy.Internal.Bot.Builder        (BotBuilder)
import qualified Howdy.Internal.Bot.Builder        as B
import qualified Howdy.Internal.Bot.CommandManager as CM
import qualified Howdy.Internal.Bot.Lifecycle      as L

bot :: B.BotBuilder () -> IO ()
bot = L.start . snd . B.mkBot
