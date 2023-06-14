{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Howdy.Comptime.Command
    ( Command
    , Permission
    , CommandDefinition (..)
    , CommandInput (..)
    , CommandReplyData (..)
    , alias
    , desc
    , hide
    , legacy
    , permission
    , run
    , (Howdy.Comptime.Command.>>)
    ) where

import Control.Monad.Identity (Identity)
import Control.Monad.Trans.Except (ExceptT)
import Control.Optics (Lensed (focus), WithLens (..))
import Data.Default (Default (def))
import Data.Text (Text)
import Discord (DiscordHandler)
import Discord.Types (ChannelId, GuildId, MessageReference, User, UserId)
import Howdy.Internal.Discord (HowdyHandler, MonadReply)
import Howdy.Internal.Error (HowdyException)
import Howdy.Internal.Logging ( (<<), (<+) )
import Lens.Micro (to)
import Control.Monad.IO.Class ( MonadIO )
import Data.Kind (Constraint, Type)
import Howdy.Internal.Util (ToConstraints)

type Permission = CommandInput -> Bool

type Command = forall m. (MonadIO m, MonadReply m) => CommandInput -> m ()

data CommandReplyData = CommandReplyData
    { crdChannelId  :: ChannelId
    , crdWhisperId  :: UserId
    , crdMessageRef :: MessageReference
    }
 
instance Lensed CommandReplyData ChannelId where
    focus = to crdChannelId

instance Lensed CommandReplyData MessageReference where
    focus = to crdMessageRef

instance Lensed CommandReplyData UserId where
    focus = to crdWhisperId

data CommandDefinition = CommandDefinition
    { cdAlias      :: [Text]
    , cdDesc       :: Maybe Text
    , cdHidden     :: Bool
    , cdIdent      :: Text
    , cdPermission :: Permission
    , cdRunner     :: Command
    , cdDebug      :: Bool -- TODO: change to debug flags and later to customizable flags
    }


data CommandInput = CommandInput
    { target  :: Text
    , author  :: User
    , guild   :: Maybe GuildId
    , channel :: ChannelId
    , args    :: Text
    , ref     :: Maybe MessageReference
    } deriving (Show, Eq)


instance Default CommandDefinition where
    def = CommandDefinition
        { cdAlias      = []
        , cdDesc       = Nothing
        , cdHidden     = False
        , cdIdent      = ""
        , cdPermission = const True
        , cdRunner     = const $ pure ()
        , cdDebug      = False
        }

infixl 1 >>
(>>) :: (b -> c) -> (a -> b) -> a -> c
(>>) = (.)

alias :: [Text] -> CommandDefinition -> CommandDefinition
alias value cd = cd{ cdAlias = cd.cdAlias ++ value }
    << "Adding aliases: " <+ value


desc :: Text -> CommandDefinition -> CommandDefinition
desc value cd = cd{ cdDesc = Just value }
    << "Setting desc: " <+ value


hide :: CommandDefinition -> CommandDefinition
hide cd = cd{ cdHidden = True }
    << "Hide: True"


legacy :: Text -> CommandDefinition -> CommandDefinition
legacy value cd = cd{ cdIdent = value }
    << "Legacy id: " <+ value


permission :: Permission -> CommandDefinition -> CommandDefinition
permission value cd = cd{ cdPermission = value }
    << "Permission updated"

run :: Command -> CommandDefinition -> CommandDefinition
run x cd = cd{ cdRunner = x }
    << "Set run action."
