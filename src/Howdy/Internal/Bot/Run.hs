module Howdy.Internal.Bot.Run where

import           Control.Applicative         (Alternative ((<|>)))
import           Control.Monad.State         (evalStateT)
import           Control.Monad.Writer        (MonadIO (liftIO), execWriter)
import           Data.HashMap.Strict         (HashMap, findWithDefault)
import qualified Data.HashMap.Strict         as M
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as S
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Discord                     (DiscordHandler,
                                              RunDiscordOpts (..), def,
                                              runDiscord)
import           Discord.Types               (Emoji, Event (..),
                                              Message (messageText),
                                              ReactionInfo)
import           Howdy.Actions               (ActionContext (ActionContext, runAction),
                                              CommandData (CommandData, getAction, getAlias))
import           Howdy.Internal.Bot.Builder  (BotBuilder (BotBuilder),
                                              BotData (..))
import           Howdy.Internal.Parser.Class (MonadParse (..))
import           Howdy.Internal.Parser.Cons  (firstof, string, word)
import           System.Environment          (getEnv)

type PrefixesStore = [Text]
type CommandsStore = HashMap Text CommandData
type ReactionsStore = HashMap Emoji (ActionContext ())

data Bot = Bot { prefixesStore  :: [Text]
               , commandsStore  :: CommandsStore
               , reactionsStore :: ReactionsStore
               }

convertTextList :: [Text] -> HashSet Text
convertTextList = S.fromList

preprocessCommand :: CommandData -> (Text, CommandData)
preprocessCommand cd = (head $ getAlias cd, cd)

convertCommands :: [CommandData] -> HashMap Text CommandData
convertCommands = M.fromList . fmap preprocessCommand

mkbot :: BotBuilder () -> Bot
mkbot (BotBuilder bb) = Bot { prefixesStore = getPrefixes b
                          , commandsStore = convertCommands $ getCommands b
                          , reactionsStore = M.empty
                          }
                      where b = execWriter bb

run :: Bot -> IO ()
run b = do
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
startHandler = error "not implemented"

eventHandler :: Bot -> Event -> DiscordHandler ()
eventHandler bot (MessageCreate m) = flip evalStateT (messageText m) $ runAction $ messageHandler bot m
eventHandler bot (MessageReactionAdd r) = undefined
eventHandler _ _                   = pure ()

messageHandler :: Bot -> Message -> ActionContext ()
messageHandler b m = do
    prefix <- parse $ firstof string $ prefixesStore b
    alias <- parse word
    undefined

reactionHandler :: Bot -> ReactionInfo -> ActionContext ()
reactionHandler = undefined
