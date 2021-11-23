module Howdy.Internal.Bot.Run where

import           Control.Applicative           (Alternative ((<|>)))
import           Control.Monad                 (when)
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
                                                restCall, runDiscord)
import           Discord.Internal.Rest.Channel (ChannelRequest (GetChannelMessage))
import           Discord.Internal.Rest.User    (UserRequest (GetUser))
import           Discord.Types                 (Emoji (emojiName), Event (..),
                                                Message (messageAuthor, messageText),
                                                ReactionInfo (reactionChannelId, reactionEmoji, reactionMessageId, reactionUserId))
import           Howdy.Discord.Class           (MonadDiscord (catchDiscord))
import           Howdy.Internal.Action.Builder (CommandData (getAlias, getRunner),
                                                ReactionData (reactionRunner))
import           Howdy.Internal.Action.Run     (CommandRunner (runCommand),
                                                MonadExec (exec),
                                                ReactionRunner)
import           Howdy.Internal.Bot.Builder    (BotBuilder (BotBuilder),
                                                BotData (..))
import           Howdy.Internal.Error          (HowdyException (..))
import           Howdy.Internal.Help           (help)
import           Howdy.Internal.Parser.Class   (MonadParse (..))
import           Howdy.Internal.Parser.Cons    (firstof, string, word)
import           Prelude                       hiding (or)
import           System.Environment            (getEnv)

--type PrefixesStore = [Text]
type CommandsStore = HashMap Text CommandData
type ReactionsStore = HashMap Text ReactionData

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
                            , commandsStore = convertCommands (help coms : coms)
                            , reactionsStore = M.empty
                            }
                      where b = execWriter bb
                            coms = getCommands b

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
eventHandler bot (MessageReactionAdd r) = do
                                          m <- restCall $ GetChannelMessage (reactionChannelId r, reactionMessageId r)
                                          u <- restCall $ GetUser (reactionUserId r)
                                          case (m, u) of (Right m', Right u') -> exec (m', u') $ reactionHandler bot r
                                                         _                    -> pure ()
eventHandler _ _                        = pure ()

messageHandler :: Bot -> CommandRunner ()
messageHandler b = do
    prefix <- parse $ firstof string $ prefixesStore b
    alias <- parse word
    cmd <- liftMaybe CommandMissing $ commandsStore b !? alias
    getRunner cmd

reactionHandler :: Bot -> ReactionInfo -> ReactionRunner ()
reactionHandler b r = do
    let e = emojiName . reactionEmoji $ r
    recData <- liftMaybe ReactionMissing $ reactionsStore b !? e
    reactionRunner recData

exe :: Functor f => f a -> f ()
exe = fmap go
    where go a = seq a ()

attempt :: MonadError e m => m (Maybe a) -> e -> m a
attempt = flip attemptWith

attemptWith :: MonadError e m => e -> m (Maybe a) -> m a
attemptWith e ma = ma >>= liftMaybe e

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e Nothing  = throwError e
liftMaybe _ (Just a) = pure a
