module Howdy.Internal.Help where
import           Data.Text                     (Text)
import           Howdy.Discord.Class           (reply)
import           Howdy.Internal.Action.Builder (CommandData (CommandData, getAlias, getDesc))
import           Howdy.Internal.Action.Run     (CommandRunner)
import           Howdy.Internal.Bot.Builder    (BotData (getCommands))

help :: [CommandData] -> CommandData
help cs = CommandData ["help"] "" go []
    where go = reply $ foldr join "" cs
          join c t = printCommand c <> "\n\n" <> t

printCommand :: CommandData -> Text
printCommand cd = alias <> ": " <> desc <> "\n"
               <> "aliases: " <> foldr1 join aliases

               where aliases = getAlias cd
                     alias = head aliases
                     desc = getDesc cd
                     join t1 t2 = t1 <> ", " <> t2

-- bless: blesses the chat
--
-- usage: [prefix] bless

-- t: translates text to english
--
-- aliases: t, translate
-- usage: t <text>
