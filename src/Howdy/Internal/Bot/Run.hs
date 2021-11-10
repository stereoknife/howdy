module Howdy.Internal.Bot.Run where

import           Control.Applicative           (Alternative ((<|>)))
import           Control.Monad.Except          (MonadError (throwError),
                                                runExceptT, void)
import           Control.Monad.State           (MonadTrans (lift), evalStateT)
import           Control.Monad.Writer          (MonadIO (liftIO), execWriter)
import           Data.HashMap.Strict           (HashMap, findWithDefault, (!?))
import qualified Data.HashMap.Strict           as M
import           Data.HashSet                  (HashSet)
import qualified Data.HashSet                  as S
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Discord                       (DiscordHandler,
                                                RunDiscordOpts (..), def,
                                                runDiscord)
import           Discord.Types                 (Emoji, Event (..),
                                                Message (messageAuthor, messageText),
                                                ReactionInfo)
import           Howdy.Internal.Action.Builder (CommandData (getAlias, getRunner))
import           Howdy.Internal.Action.Run     (CommandRunner (runCommand),
                                                Exec (exec), ReactionRunner)
import           Howdy.Internal.Bot.Builder    (BotBuilder (BotBuilder),
                                                BotData (..))
import           Howdy.Internal.Error          (HowdyException (CommandMissing, ParseError))
import           Howdy.Internal.Parser.Class   (MonadParse (..))
import           Howdy.Internal.Parser.Cons    (firstof, string, word)
import           Prelude                       hiding (or)
import           System.Environment            (getEnv)

--type PrefixesStore = [Text]
type CommandsStore = HashMap Text CommandData
type ReactionsStore = HashMap Emoji ()

data Bot = Bot { prefixesStore  :: [Text]
               , commandsStore  :: CommandsStore
               , reactionsStore :: ReactionsStore
               }

convertTextList :: [Text] -> HashSet Text
convertTextList = S.fromList

preprocessCommand :: CommandData -> (Text, CommandData)
preprocessCommand cd = (head $ getAlias cd, cd)

convertCommands :: [CommandData] -> HashMap Text CommandData
convertCommands = M.fromList . fmap preprocessCommand

mkBot :: BotBuilder () -> Bot
mkBot (BotBuilder bb) = Bot { prefixesStore = getPrefixes b
                            , commandsStore = convertCommands $ getCommands b
                            , reactionsStore = M.empty
                            }
                      where b = execWriter bb

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
startHandler = liftIO $ putStrLn "Start" -- error "not implemented"

eventHandler :: Bot -> Event -> DiscordHandler ()
eventHandler bot (MessageCreate m)      = exec (messageText m, m, messageAuthor m) $ messageHandler bot
eventHandler bot (MessageReactionAdd r) = undefined
eventHandler _ _                        = pure ()

messageHandler :: Bot -> CommandRunner ()
messageHandler b = do
    prefix <- parse $ firstof string $ prefixesStore b
    alias <- parse word
    cmd <- liftMaybe CommandMissing $ commandsStore b !? alias
    getRunner cmd

attempt :: MonadError e m => m (Maybe a) -> e -> m a
attempt = flip attemptWith

attemptWith :: MonadError e m => e -> m (Maybe a) -> m a
attemptWith e ma = ma >>= liftMaybe e

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e Nothing  = throwError e
liftMaybe _ (Just a) = pure a

reactionHandler :: Bot -> ReactionInfo -> ReactionRunner ()
reactionHandler = undefined

exe :: Functor f => f a -> f ()
exe = fmap go
    where go a = seq a ()
