module Howdy.Internal.Help where
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Howdy.Discord.Class           (reply)
import           Howdy.Internal.Action.Builder (ActionBuilderData (..),
                                                CommandBuilder,
                                                CommandBuilderData, alias, run)
import           Howdy.Internal.Action.Run     (CommandRunner)
import           Howdy.Internal.Bot.Builder    (BotBuilderData (bbCommands))

help :: [CommandBuilderData] -> CommandBuilder ()
help cs = do
          alias "help"
          run go
    where go = reply $ foldr join "" cs
          join c t = printCommand c <> "\n" <> t

printCommand :: CommandBuilderData -> Text
printCommand cd = "🤠 **" <> showAlias alias <> ":** " <> desc
               where alias = a_alias cd
                     desc = a_desc cd

showAlias :: [Text] -> Text
showAlias [x]    = x
showAlias (x:xs) = x <> ", " <> showAlias xs
showAlias []     = mempty
