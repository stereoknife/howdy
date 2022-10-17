-- Useless file, just to describe the howdy API
module Examples.Api where
{-
main :: Bot
main = bot
    $ token "bot_token.secret"
    . name "my bot"
    . super "snowflake here"

    $ command "mcCommand"
        . desc "prepends mc to a word"
        . run $ \t -> send "mc" <> t

    $ reaction "😀"
        . desc "does nothing"

bot :: Bot m -> IO ()

token :: Text -> Bot m
name :: Text -> Bot m
super :: Snowflake -> Bot m
-- .. etc

command :: Text -> Command m -> Bot m
command = _ where
    desc :: Text -> Command m
    lambda :: (Text -> DiscordHandler ()) -> Command m
    echo :: (Text -> Text) -> Command m
    run :: Type1 -> Command m

reaction :: Emoji -> Reaction m -> Bot m
    -- same as command

-- Data
data Bot = Bot { tokenLoc :: Text -- Token location (file/env)
               , meta     :: Meta -- Bot metadata
               , commands :: Commands -- Commands data structure
               }

data Meta = Meta { name      :: Text
                 , author    :: Maybe Text
                 , homeGuild :: Maybe Snowflake
                 , desc      :: Maybe Text
                 , status    :: Maybe Text
                 , info      :: Maybe Text
                 }

type Commands = HashMap Text Command
 -}
