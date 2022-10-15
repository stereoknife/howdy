module Howdy.Comptime.Bot where

import Control.Monad.Free (Free (Free), liftF)
import Control.Monad.Reader (Reader)
import Data.Default (Default (..))
import Data.HashMap.Internal.Strict (HashMap, empty, insert)
import Data.Text (Text)
import Discord.Types (ChannelId, UserId)
import Howdy.Comptime.Command (CommandDefinition)
import Howdy.Comptime.Reaction (EmojiIdentifier, ReactionDefinition)
import Howdy.Internal.Debug (DebugOptions (..))

data BotMeta = BotMeta
    { bmAuthor :: Maybe UserId
    , bmRepo   :: Maybe Text
    , bmNote   :: Maybe Text
    , bmName   :: Maybe Text
    }

data BotDefinition = BotDefinition
    { bdPrefixes  :: [Text]
    , bdCommands  :: HashMap Text CommandDefinition
    , bdReactions :: HashMap EmojiIdentifier ReactionDefinition
    , bdAliases   :: HashMap Text Text
    , bdEmojis    :: HashMap EmojiIdentifier Text
    , bdTokens    :: [String]
    , bdDebug     :: DebugOptions
    , bdMeta      :: BotMeta
    }

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

-- Prefs

prefixes :: [Text] -> BotDefinition -> BotDefinition
prefixes x bd = bd{ bdPrefixes = x }

command :: Text -> CommandDefinition -> BotDefinition -> BotDefinition
command n x bd = bd{ bdCommands = insert n x bd.bdCommands }

reaction :: EmojiIdentifier -> ReactionDefinition -> BotDefinition -> BotDefinition
reaction n x bd = bd{ bdReactions = insert n x bd.bdReactions }


token :: String -> BotDefinition -> BotDefinition
token x bd = bd { bdTokens = (x:bd.bdTokens) }

-- Meta

author :: UserId -> BotDefinition -> BotDefinition
author x bd = bd{ bdMeta = bd.bdMeta { bmAuthor = Just x } }

repo :: Text -> BotDefinition -> BotDefinition
repo x bd = bd{ bdMeta = bd.bdMeta { bmRepo = Just x } }

note :: Text -> BotDefinition -> BotDefinition
note x bd = bd{ bdMeta = bd.bdMeta { bmNote = Just x } }

name :: Text -> BotDefinition -> BotDefinition
name x bd = bd{ bdMeta = bd.bdMeta { bmName = Just x } }
