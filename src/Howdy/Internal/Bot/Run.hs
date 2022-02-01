module Howdy.Internal.Bot.Run where

import Data.HashMap.Strict (HashMap, (!?))
import Data.Text (Text)
import Howdy.Internal.Action.Builder
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Discord
import Control.Applicative (Alternative((<|>)))
import System.Environment (getEnv)
import Discord.Types hiding (ParseError)
import Howdy.Error (HowdyException (CommandMissing, ParseError), report)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Except (ExceptT, runExceptT, MonadIO (liftIO))
import Howdy.Effects.Discord hiding (ParseError)
import Howdy.Parser (Parser(runParser), firstof, string, Parse (parse), word)
import Control.Monad.State (StateT)

type CommandsStore = HashMap Text CommandBuilderData
type ReactionsStore = HashMap Text (DiscordHandler ())

data Bot = Bot { prefixesStore  :: [Text]
               , commandsStore  :: CommandsStore
               , reactionsStore :: ReactionsStore
               }

type DiscordE = StateT Text (ExceptT HowdyException DiscordHandler) ()

runBot :: Bot -> IO ()
runBot b = do
    tok <- TIO.readFile "./token.secret" <|> T.pack <$> getEnv "DISCORD_TOKEN"
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
startHandler = undefined 

eventHandler :: Bot -> Event -> DiscordHandler ()
eventHandler b (MessageCreate m) = seq runMessageHandler $ pure ()
    where runMessageHandler = undefined
eventHandler b (MessageReactionAdd r) = seq runReactionHandler $ pure ()
    where runReactionHandler = undefined
eventHandler _ _ = pure ()

messageHandler :: Bot -> Message -> DiscordE
messageHandler b m = do
    p <- parse $ firstof string (prefixesStore b)
    c <- parse word
    a <- report CommandMissing $ commandsStore b !? c
    pure ()