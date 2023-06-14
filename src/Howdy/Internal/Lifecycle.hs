{-# LANGUAGE LambdaCase #-}

module Howdy.Internal.Lifecycle where

import Control.Monad.Catch (MonadCatch (catch))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import Discord (DiscordHandler, RunDiscordOpts (..), def, runDiscord)
import Discord.Types (Event (MessageCreate, MessageReactionAdd))
import Howdy.Comptime.Bot (BotDefinition (..))
import Howdy.Internal.Manager.Command (commandHandler)
import Howdy.Internal.Manager.Reaction (reactionHandler)
import Howdy.Internal.Error (HowdyException (..), errorHandler)
import Howdy.Secrets (credentials, defaultTokenEnv, defaultTokenPath, fromList)
import System.Environment

exec :: Applicative a => a b -> a ()
exec f = seq f $ pure ()

-- Defines the bot then starts it
bot :: (BotDefinition -> BotDefinition) -> IO ()
bot b = do
    putStrLn "Loading bot..."
    let bot = b def
    start bot

-- Starts the bot
start :: BotDefinition -> IO ()
start b = do
    putStrLn "Starting bot..."
    tok <- credentials

    userFacingError <- runDiscord $ def
        { discordToken = tok
        , discordOnEvent = \case
            MessageCreate m      -> commandHandler m b `catch` errorHandler
            MessageReactionAdd r -> reactionHandler r b `catch` errorHandler
            _                    -> pure ()

        , discordOnLog = \_log -> do
            pure ()

        , discordOnStart = do
            liftIO $ putStrLn "Starting"

        , discordOnEnd = do
            liftIO $ putStrLn "Shutting down"
            --liftIO $ disconnect redis

        } -- if you see OnLog error, post in the discord / open an issue

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)
