module Howdy.Internal.Bot.Lifecycle where

import Howdy.Internal.Bot.CommandManager (commandHandler)
import Discord.Types ( Event(MessageCreate) )
import Control.Monad.Free (_Free)
import Discord
    ( def,
      DiscordHandler,
      runDiscord,
      RunDiscordOpts(discordOnLog, discordToken, discordOnEvent) )
import qualified Data.Text.IO as TIO
import Howdy.Secrets (fromList, defaultTokenEnv, defaultTokenPath)
import Howdy.Internal.Bot.Builder (BotPreferences (..), BotData)
import Howdy.Internal.Bot.Debug
import Control.Monad.Reader (ReaderT (runReaderT), Reader, runReader)
import Data.Text (Text)

start :: BotPreferences -> IO ()
start b = do
    tok <- fromList (defaultTokenPath:defaultTokenEnv:b.tokens)
    userFacingError <- runDiscord $ def
             { discordToken = tok
             , discordOnEvent = flip runReaderT b . eventHandler
             , discordOnLog = logHandler b
             } -- if you see OnLog error, post in the discord / open an issue

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> ReaderT BotPreferences DiscordHandler ()
eventHandler (MessageCreate m) = undefined -- commandHandler m
eventHandler _                 = pure ()

logHandler :: BotPreferences -> Text -> IO ()
logHandler b s | b.debug.pipeLogs = pure () -- TIO.putStrLn s >> TIO.putStrLn ""
               | otherwise        = TIO.putStrLn s >> TIO.putStrLn ""