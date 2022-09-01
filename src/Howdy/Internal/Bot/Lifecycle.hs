{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Howdy.Internal.Bot.Lifecycle where

import           Control.Monad                      (when)
import           Control.Monad.Catch                (Exception,
                                                     Handler (Handler),
                                                     MonadThrow (throwM),
                                                     SomeException, catchAll,
                                                     catchIf, catches,
                                                     handleAll, handleIf,
                                                     onError)
import           Control.Monad.Except               (ExceptT, MonadIO (liftIO),
                                                     MonadTrans (lift),
                                                     runExceptT)
import           Control.Monad.Free                 (_Free)
import           Control.Monad.Reader               (MonadReader (ask), Reader,
                                                     ReaderT (runReaderT),
                                                     runReader)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as TIO
import           Discord                            (DiscordHandler,
                                                     RunDiscordOpts (discordOnEvent, discordOnLog, discordToken),
                                                     def, restCall, runDiscord)
import           Discord.Requests                   (ChannelRequest (CreateMessage))
import           Discord.Types                      (Event (MessageCreate, MessageReactionAdd),
                                                     Message (messageContent))
import           Howdy.Internal.Bot.Builder         (BotData,
                                                     BotPreferences (..))
import           Howdy.Internal.Bot.CommandManager  (commandHandler)
import           Howdy.Internal.Bot.Debug           (DebugOptions (pipeLogs, pipeLogsTarget))
import           Howdy.Internal.Bot.ReactionManager (reactionHandler)
import           Howdy.Internal.Discord             (MonadDiscord (liftDiscord),
                                                     liftDiscord, printLog)
import           Howdy.Internal.Error               (HowdyException (Ignore))
import           Howdy.Secrets                      (defaultTokenEnv,
                                                     defaultTokenPath, fromList)
import           Network.HTTP.Client                (HttpException)

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

eventHandler :: forall e. Event -> ReaderT BotPreferences DiscordHandler ()
eventHandler (MessageCreate m) = do
    b <- ask
    lift $ handleAll (errorHandler b)
         $ handleIf isIgnore ignore
         $ (commandHandler m b)

eventHandler (MessageReactionAdd r) = do
    b <- ask
    lift $ handleAll (errorHandler b)
         $ handleIf isIgnore ignore
         $ (reactionHandler r b)

eventHandler _                 = pure ()

isIgnore :: HowdyException -> Bool
isIgnore Ignore = True
isIgnore _      = False

ignore :: Applicative f => p -> f ()
ignore _ = pure ()

errorHandler :: BotPreferences -> SomeException -> DiscordHandler ()
errorHandler b s = do when (b.debug.pipeLogs) $
                        printLog b.debug.pipeLogsTarget (T.pack $ show s)
                      liftIO $ putStrLn (show s)
                      liftIO $ putStrLn ""
                      throwM s

logHandler :: BotPreferences -> Text -> IO ()
logHandler b s | b.debug.pipeLogs = pure () -- TIO.putStrLn s >> TIO.putStrLn ""
               | otherwise        = TIO.putStrLn s >> putStrLn ""
