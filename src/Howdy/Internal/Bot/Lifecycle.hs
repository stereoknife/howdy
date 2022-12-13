{-# LANGUAGE LambdaCase #-}

module Howdy.Internal.Bot.Lifecycle where

import Control.Monad.Catch (MonadCatch (catch))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import Discord (DiscordHandler, RunDiscordOpts (..), def, runDiscord)
import Discord.Types (Event (MessageCreate, MessageReactionAdd))
import Howdy.Comptime.Bot (BotDefinition (..))
import Howdy.Internal.Bot.CommandManager (commandHandler)
import Howdy.Internal.Bot.ReactionManager (reactionHandler)
import Howdy.Internal.Error (HowdyException (..), errorHandler)
import Howdy.Secrets (defaultTokenEnv, defaultTokenPath, fromList)

bot :: (BotDefinition -> BotDefinition) -> IO ()
bot b = do
    putStrLn "Loading bot..."
    let bot = b def
    putStrLn $ "Aliases: " ++ show bot.bdAliases
    putStrLn $ "Commands: " ++ show bot.bdCommands
    start bot

start :: BotDefinition -> IO ()
start b = do
    putStrLn "Starting bot..."
    tok <- fromList (defaultTokenPath:defaultTokenEnv:b.bdTokens)
    userFacingError <- runDiscord $ def
        { discordToken = tok
        , discordOnEvent = eventHandler b
        , discordOnLog = logHandler b
        , discordOnStart = liftIO $ putStrLn "Started"
        , discordOnEnd = liftIO $ putStrLn "Shutting down"
        } -- if you see OnLog error, post in the discord / open an issue

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)


eventHandler :: BotDefinition -> Event -> DiscordHandler ()
eventHandler bd = \case
    MessageCreate m      -> commandHandler m bd `catch` errorHandler
    MessageReactionAdd r -> reactionHandler r bd `catch` errorHandler
    _                    -> pure ()

logHandler :: BotDefinition -> Text -> IO ()
logHandler _ _ = pure ()
