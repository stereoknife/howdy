{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Howdy.Internal.Bot.Builder where

import           Control.Monad.Writer          (MonadWriter (tell), Writer,
                                                WriterT (WriterT), execWriter)
import           Data.HashMap.Strict           (HashMap, findWithDefault, (!?))
import qualified Data.HashMap.Strict           as M
import           Data.Text                     (Text)
import           Discord.Types                 (UserId)
import           Howdy.Internal.Action.Builder (CommandBuilder,
                                                CommandBuilderData,
                                                ReactionBuilder,
                                                ReactionBuilderData)
import           Howdy.Internal.Builder        (Builder (..))

type CommandsStore = HashMap Text CommandBuilderData
type ReactionsStore = HashMap Text ReactionBuilderData

data Bot = Bot { botPrefixes  :: [Text]
               , botCommands  :: CommandsStore
               , botReactions :: ReactionsStore
               }

data BotBuilderData = BotBuilderData { bbPrefixes  :: [Text]
                                     , bbCommands  :: [CommandBuilder ()]
                                     , bbReactions :: [ReactionBuilder ()]
                                     , bbAdmins    :: [UserId]
                                     }

instance Semigroup BotBuilderData where
    a <> b = BotBuilderData { bbPrefixes  = bbPrefixes a <> bbPrefixes b
                            , bbCommands  = bbCommands a <> bbCommands b
                            , bbReactions = bbReactions a <> bbReactions b
                            , bbAdmins    = bbAdmins a <> bbAdmins b
                            }

instance Monoid BotBuilderData where
    mempty = BotBuilderData mempty mempty mempty mempty

newtype BotBuilder a = BotBuilder (Writer BotBuilderData a)
    deriving (Functor, Applicative, Monad, MonadWriter BotBuilderData)

instance Builder (BotBuilder a) where
    type BuilderOutput (BotBuilder a) = Bot
    build (BotBuilder bb) = Bot { botPrefixes = bbPrefixes b
                                , botCommands = M.fromList $ fmap build commands
                                , botReactions = M.fromList $ fmap build reactions
                                }
                      where b         = execWriter bb
                            commands  = let c = bbCommands b in c --(help c:c)
                            reactions = bbReactions b

command :: CommandBuilder () -> BotBuilder ()
command c = tell $ mempty{bbCommands = [c]}

prefix :: Text -> BotBuilder ()
prefix p = tell $ mempty{bbPrefixes = [p]}

prefixes :: [Text] -> BotBuilder ()
prefixes p = tell $ mempty{bbPrefixes = p}

reaction :: ReactionBuilder () -> BotBuilder ()
reaction r = tell $ mempty{bbReactions = [r]}
