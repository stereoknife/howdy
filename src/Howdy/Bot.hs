{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Howdy.Bot where

import           Control.Applicative    ( Alternative(empty, (<|>)) )
import           Control.Monad          ( guard )
import           Control.Monad.IO.Class ( MonadIO(liftIO) )
import           Control.Monad.Reader   ( ReaderT(runReaderT), void, when )
import           Control.Monad.Writer
    ( MonadWriter(tell), Writer, WriterT(WriterT), execWriter, when )

import           Data.Default           ( Default(..) )
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import           Data.Maybe             ( fromMaybe )
import           Data.Text              ( Text )
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           Discord
    ( DiscordHandler
    , RunDiscordOpts(discordToken, discordOnEvent, discordOnStart,
               discordOnEnd, discordOnLog), def
    , runDiscord )
import           Discord.Types
    ( Emoji, Event(MessageCreate), Message(messageText) )

import           Howdy.Parser           ( firstof, runParser, string, word )

import           System.Environment     ( getEnv )

type Commands = M.HashMap Text ()

type Reactions = M.HashMap Emoji ()

type Prefixes = S.HashSet Text

{-}

data BotData = BotData { prefixes :: [Text]
                       , commands :: Commands
                       , reactions :: Reactions
                       }

instance Default BotData where
    def = BotData mempty mempty mempty

instance Semigroup BotData where
    a <> b = BotData { prefixes  = prefixes a <> prefixes b
                     , commands  = commands a <> commands b
                     , reactions = reactions a <> reactions b
                     }

instance Monoid BotData where
    mempty = def

newtype Bot a = MkBot (Writer BotData a)
    deriving ( Functor, Applicative, Monad, MonadWriter BotData )

getBot :: Bot a -> BotData
getBot (MkBot w) = execWriter w

prefix :: [ Text ] -> Bot ()
prefix pf = tell $ def { prefixes = pf }

command :: Text -> Text -> DiscordHandler () -> Bot ()
command alias desc action
    = tell $ def { commands = M.singleton alias $ Command alias desc action }

reaction :: Emoji -> Text -> DiscordHandler () -> Bot ()
reaction emoji desc action
    = tell $ def { reactions = M.singleton emoji $ Reaction emoji desc action }

eventHandler :: BotData -> Event -> DiscordHandler ()
eventHandler bot (MessageCreate m) = go $ do
    firstof string $ prefixes bot
    alias <- word
    pure $ runCommands alias $ commands bot
  where
    parsed p = runParser p $ messageText m

    go p' = seq (parsed p') (pure ())
eventHandler _ _ = pure ()

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
