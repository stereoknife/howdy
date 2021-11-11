module Howdy.Internal.Help where
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Howdy.Discord.Class           (reply)
import           Howdy.Internal.Action.Builder (CommandData (CommandData, getAlias, getDesc))
import           Howdy.Internal.Action.Run     (CommandRunner)
import           Howdy.Internal.Bot.Builder    (BotData (getCommands))

help :: [CommandData] -> Text -> CommandData
help cs p = CommandData ["help"] "" go []
    where go = reply $ foldr join "" cs
          join c t = printCommand c p <> "\n" <> t

printCommand :: CommandData -> Text -> Text
printCommand cd p = "**" <> p <> " " <> T.pack (show alias) <> ":** " <> desc
               where alias = getAlias cd
                     desc = getDesc cd

-- bless: blesses the chat
--
-- usage: [prefix] bless

-- t: translates text to english
--
-- aliases: t, translate
-- usage: t <text>
