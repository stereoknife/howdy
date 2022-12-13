{-# LANGUAGE ExtendedDefaultRules #-}

module Howdy.Comptime.Bot where

import Control.Monad.Reader (Reader)
import Data.Default (Default (..))
import Data.HashMap.Internal.Strict (HashMap, empty, insert)
import Data.Text (Text)
import Debug.Trace (trace)
import Discord.Types (ChannelId, UserId)
import Howdy.Comptime.Command (CommandDefinition (..))
import Howdy.Comptime.Reaction (EmojiIdentifier, ReactionDefinition)
import Howdy.Internal.Debug (DebugOptions (..))
import Howdy.Internal.Logging (Fmt (..), (<+), (<<))

data BotMeta = BotMeta
    { bmAuthor :: Maybe UserId
    , bmRepo   :: Maybe Text
    , bmNote   :: Maybe Text
    , bmName   :: Maybe Text
    } deriving (Show)

data BotDefinition = BotDefinition
    { bdPrefixes  :: [Text]
    , bdCommands  :: HashMap Text CommandDefinition
    , bdReactions :: HashMap EmojiIdentifier ReactionDefinition
    , bdAliases   :: HashMap Text Text
    , bdEmojis    :: HashMap EmojiIdentifier Text
    , bdTokens    :: [String]
    , bdDebug     :: DebugOptions
    , bdMeta      :: BotMeta
    } deriving (Show)

instance Default BotDefinition where
    def = BotDefinition
        { bdPrefixes = []
        , bdCommands = empty
        , bdReactions = empty
        , bdAliases = empty
        , bdEmojis = empty
        , bdTokens = []
        , bdDebug = def
        , bdMeta = def
        }

instance Default BotMeta where
    def = BotMeta
        { bmAuthor = Nothing
        , bmRepo = Nothing
        , bmNote = Nothing
        , bmName = Nothing
        }

--loadDefaults :: BotData -> BotData
--loadDefaults (m, p) = (m, p)--(m, p { commands = insert "help" undefined p.commands})

(>>) :: (b -> c) -> (a -> b) -> a -> c
(>>) = (.)

--default (Text)

prefixes :: [Text] -> BotDefinition -> BotDefinition
prefixes x bd = bd{ bdPrefixes = x }
    << "Setting prefixes: " -- ++ show x

command :: Text -> (CommandDefinition -> CommandDefinition) -> BotDefinition -> BotDefinition
command name cd bd = bd
    { bdCommands = insert name cd' bd.bdCommands
    , bdAliases = registerAliases (name:cd'.cdAlias) name bd.bdAliases }
    << "Adding command: " <+ name
    where cd' = cd def

reaction :: EmojiIdentifier -> (ReactionDefinition -> ReactionDefinition) -> BotDefinition -> BotDefinition
reaction name rd bd = bd{ bdReactions = insert name rd' bd.bdReactions }
    << "Adding reaction: " <+ name
    where rd' = rd def

token :: String -> BotDefinition -> BotDefinition
token x bd = bd { bdTokens = (x:bd.bdTokens) }
    << "Setting token: ***"

author :: UserId -> BotDefinition -> BotDefinition
author x bd = bd{ bdMeta = bd.bdMeta { bmAuthor = Just x } }
    << "Setting author: " <+ x

repo :: Text -> BotDefinition -> BotDefinition
repo x bd = bd{ bdMeta = bd.bdMeta { bmRepo = Just x } }
    << "Setting repo: " <+ x

note :: Text -> BotDefinition -> BotDefinition
note x bd = bd{ bdMeta = bd.bdMeta { bmNote = Just x } }
    << "Setting note: " <+ x

name :: Text -> BotDefinition -> BotDefinition
name x bd = bd{ bdMeta = bd.bdMeta { bmName = Just x } }
    << "Setting name: " <+ x

registerAliases :: [Text] -> Text -> HashMap Text Text -> HashMap Text Text
registerAliases as t map = go as map
    where go (x:xs) m = insert x t (go xs m)
          go [] m     = m
