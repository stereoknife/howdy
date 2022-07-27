{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Howdy.Internal.Bot.Builder where

import Control.Monad.Reader (Reader)
import Data.HashMap.Internal.Strict (HashMap, insert, empty)
import Howdy.Internal.Command (CommandData, CommandPreferences (ident), CommandMeta (alias), CommandDefinition, mkCommand)
import Data.Text (Text)
import Control.Monad.Free ( Free(Free), liftF )
import Discord.Types ( ChannelId, UserId )
import Howdy.Internal.Bot.Debug (DebugOptions (..))
import Howdy.Internal.Reaction
    ( mkReaction,
      EmojiIdentifier,
      ReactionData,
      ReactionDefinition,
      ReactionMeta,
      ReactionPreferences(ident) )

data BotMeta = BotMeta
    { author :: Maybe UserId
    , repo   :: Maybe Text
    , note   :: Maybe Text
    , name   :: Maybe Text
    }

data BotPreferences = BotPreferences 
    { prefixes      :: [Text]
    , commandsData  :: [CommandMeta]
    , commands      :: HashMap Text CommandPreferences
    , reactionsData :: [ReactionMeta]
    , reactions     :: HashMap EmojiIdentifier ReactionPreferences
    , aliases       :: HashMap Text Text
    , emojis        :: HashMap EmojiIdentifier Text
    , tokens        :: [String]
    , debug         :: DebugOptions
    }

emptyBM :: BotMeta
emptyBM = BotMeta Nothing Nothing Nothing Nothing

emptyBP :: BotPreferences
emptyBP = BotPreferences [] [] empty [] empty empty empty [] emptyDO

emptyDO :: DebugOptions
emptyDO = DebugOptions True (788506879869452288 :: ChannelId) True

type BotData = (BotMeta, BotPreferences)

data BBCMD a = Prefixes [Text] a
             | Command CommandData a
             | Reaction ReactionData a
             | Token String a
             | Author UserId a
             | Repo Text a
             | Note Text a
             | Name Text a
             | Debug DebugOptions a
             deriving Functor

type BotBuilder = Free BBCMD

build' :: BotBuilder a -> BotData -> BotData
build' (Free (Prefixes      x next)) (b, p) = build' next $ (b, p { prefixes = x })
build' (Free (Token         x next)) (b, p) = build' next $ (b, p { tokens = (x:p.tokens)})
build' (Free (Command  (m, r) next)) (b, p) = build' next $ (b, p { commandsData = (m:p.commandsData)
                                                                  , commands = insert r.ident r p.commands
                                                                  , aliases = foldr (\a acc -> insert r.ident a acc) p.aliases m.alias
                                                                  })
build' (Free (Reaction (m, r) next)) (b, p) = build' next $ (b, p { reactionsData = (m:p.reactionsData)
                                                                  , reactions = insert r.ident r p.reactions
                                                                  -- , aliases = foldr (\a acc -> insert r.ident a acc) p.aliases m.alias
                                                                  })

build' (Free (Author        x next)) (b, p) = build' next $ (b { author = Just x }, p)
build' (Free (Repo          x next)) (b, p) = build' next $ (b { repo   = Just x }, p)
build' (Free (Note          x next)) (b, p) = build' next $ (b { note   = Just x }, p)
build' (Free (Name          x next)) (b, p) = build' next $ (b { name   = Just x }, p)
build' _ x = x


mkBot :: BotBuilder a -> BotData
mkBot b = loadDefaults . build' b $ ( emptyBM
                                    , emptyBP
                                    )

loadDefaults :: BotData -> BotData
loadDefaults (m, p) = (m, p)--(m, p { commands = insert "help" undefined p.commands})

-- Prefs

prefixes :: [Text] -> BotBuilder ()
prefixes x = liftF ((Prefixes x) ())

commandData :: CommandData -> BotBuilder ()
commandData x = liftF ((Command x) ())

command :: Text -> CommandDefinition () -> BotBuilder ()
command x = commandData . mkCommand x

reactionData :: ReactionData -> BotBuilder ()
reactionData x = liftF ((Reaction x) ())

reaction :: EmojiIdentifier -> ReactionDefinition () -> BotBuilder ()
reaction x = reactionData . mkReaction x


token :: String -> BotBuilder ()
token x = liftF ((Token x) ())

-- Meta

author :: UserId -> BotBuilder ()
author x = liftF ((Author x) ())

repo :: Text -> BotBuilder ()
repo x = liftF ((Repo x) ())

note :: Text -> BotBuilder ()
note x = liftF ((Note x) ())

name :: Text -> BotBuilder ()
name x = liftF ((Name x) ())