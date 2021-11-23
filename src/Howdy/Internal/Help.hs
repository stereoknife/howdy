module Howdy.Internal.Help where
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Howdy.Discord.Class           (reply)
import           Howdy.Internal.Action.Builder (CommandData (CommandData, getAlias, getDesc))
import           Howdy.Internal.Action.Run     (CommandRunner)
import           Howdy.Internal.Bot.Builder    (BotData (getCommands))

help :: [CommandData] -> CommandData
help cs = CommandData ["help"] "" go []
    where go = reply $ foldr join "" cs
          join c t = printCommand c <> "\n" <> t

printCommand :: CommandData -> Text
printCommand cd = "🤠 **" <> showAlias alias <> ":** " <> desc
               where alias = getAlias cd
                     desc = getDesc cd

showAlias :: [Text] -> Text
showAlias [x]    = x
showAlias (x:xs) = x <> ", " <> showAlias xs
showAlias []     = mempty

-- bless: blesses the chat
--
-- usage: [prefix] bless

-- t: translates text to english
--
-- aliases: t, translate
-- usage: t <text>
