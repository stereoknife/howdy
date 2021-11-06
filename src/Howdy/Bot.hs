{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Howdy.Bot where

import           Control.Monad.Writer (MonadWriter (tell), Writer,
                                       WriterT (WriterT), execWriter)
import           Data.Text            (Text)
import           Howdy.Actions        (CommandBuilder (run_CB), CommandData)



--type Commands = M.HashMap Text ()

--type Reactions = M.HashMap Emoji ()

--type Prefixes = S.HashSet Text


data BotData = BotData { getPrefixes  :: [Text]
                       , getCommands  :: [CommandData]
                       , getReactions :: [()]
                       }

instance Semigroup BotData where
    a <> b = BotData { getPrefixes  = getPrefixes a <> getPrefixes b
                     , getCommands  = getCommands a <> getCommands b
                     , getReactions = getReactions a <> getReactions b
                     }

instance Monoid BotData where
    mempty = BotData mempty mempty mempty

newtype BotBuilder a = BotBuilder {run_BB :: Writer BotData a}
    deriving (Functor, Applicative, Monad, MonadWriter BotData)

command :: CommandBuilder () -> BotBuilder ()
command c = tell $ mempty{getCommands = [execWriter $ run_CB c]}

prefix :: Text -> BotBuilder ()
prefix p = tell $ mempty{getPrefixes = [p]}

prefixes :: [Text] -> BotBuilder ()
prefixes p = tell $ mempty{getPrefixes = p}

reaction :: () -> BotBuilder ()
reaction r = tell $ mempty{getReactions = [r]}
