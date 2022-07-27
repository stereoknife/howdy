{-# LANGUAGE FlexibleInstances #-}
module Howdy.Internal.Bot.Lifecycle where

import Howdy.Internal.Bot.CommandManager (commandHandler)
import Discord.Types ( Event(MessageCreate), Message (messageContent) )
import Control.Monad.Free (_Free)
import Discord
    ( def,
      DiscordHandler,
      runDiscord,
      RunDiscordOpts(discordOnLog, discordToken, discordOnEvent) )
import qualified Data.Text.IO as TIO
import Howdy.Secrets (fromList, defaultTokenEnv, defaultTokenPath)
import Howdy.Internal.Bot.Builder (BotPreferences (..), BotData)
import Howdy.Internal.Bot.Debug ( DebugOptions(pipeLogs) )
import Control.Monad.Reader (ReaderT (runReaderT), Reader, runReader, MonadReader (ask))
import Data.Text (Text)
import Control.Monad.Except (runExceptT, ExceptT, MonadTrans (lift), MonadIO (liftIO))
import Howdy.Internal.Discord (MonadDiscord (liftDiscord), liftDiscord)
import Howdy.Internal.Error ( HowdyException )

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

instance MonadDiscord m => MonadDiscord (ExceptT HowdyException m) where
    liftDiscord = lift . liftDiscord

eventHandler :: Event -> ReaderT BotPreferences DiscordHandler ()
eventHandler (MessageCreate m) = do b <- ask
                                    lift $ commandHandler m b
eventHandler _                 = pure ()

logHandler :: BotPreferences -> Text -> IO ()
logHandler b s | b.debug.pipeLogs = pure () -- TIO.putStrLn s >> TIO.putStrLn ""
               | otherwise        = TIO.putStrLn s >> TIO.putStrLn ""