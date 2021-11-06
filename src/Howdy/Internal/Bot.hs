module Howdy.Internal.Bot where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as S
import           Data.Text           (Text)
import           Discord.Types       (Emoji)
import           Howdy.Actions       (ActionContext (ActionContext),
                                      CommandData (CommandData))

type PrefixesStore = HashSet Text
type CommandsStore = HashMap Text (ActionContext ())
type ReactionsStore = HashMap Emoji (ActionContext ())

data Bot = Bot { prefixesStore  :: PrefixesStore
               , commandsStore  :: CommandsStore
               , reactionsStore :: ReactionsStore
               }

convertTextList :: [Text] -> HashSet Text
convertTextList = S.fromList

convertCommands :: [CommandData] -> HashMap Text (ActionContext ())
convertCommands = undefined

convertCommand :: CommandData -> [(Text, CommandData)]

{-}

import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           Discord       (DiscordHandler)
import           Discord.Types (Event (MessageCreate), Message (messageText))
import           Howdy.Bot     (BotData (..))
import           Howdy.Parser  (Parser (runParser), firstof, string, word)


eventHandler :: BotData -> Event -> DiscordHandler ()
eventHandler bot (MessageCreate m) = go $ do
    firstof string $ prefixes bot
    alias <- word
    pure $ runCommands alias $ commands bot
  where
    parsed p = runParser p $ messageText m

    go p' = seq (parsed p') (pure ())
eventHandler _ _ = pure ()

runCommands :: T.Text -> t0 -> a0
runCommands = error "not implemented"


run :: Bot () -> IO ()
run b' = do
    tok <- TIO.readFile "./token.secret" <|> T.pack <$> getEnv "DISCORD_TOKEN"
    let b = getBot b'
    -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
    t <- runDiscord $ def
        { discordToken   = tok
        , discordOnStart = startHandler
        , discordOnEnd   = liftIO $ putStrLn "Ended"
        , discordOnEvent = eventHandler b
        , discordOnLog   = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        }
    TIO.putStrLn t

startHandler :: DiscordHandler ()
startHandler = error "not implemented"

-- TODO: Make action mcmonad so it can have the nice cool data it needs
runCommand :: Text -> Command -> Maybe (DiscordHandler ())
runCommand t (Command a _ c) = guard (t == a) >> Just c

runCommands :: Text -> [ Command ] -> DiscordHandler ()
runCommands t = fromMaybe (pure ()) . go
  where
    go = foldr ((<|>) . runCommand t) empty

-}
