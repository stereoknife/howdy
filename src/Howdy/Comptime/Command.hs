{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
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
import Control.Monad.Reader
import Control.Monad.Trans.Except (ExceptT)
import Control.Optics (Lensed (focus))
import Data.Default (Default (def))
import Data.Text (Text)
import Discord (DiscordHandler)
import Discord.Types (ChannelId, GuildId, MessageReference, User, UserId)
import Howdy.Internal.Discord (HowdyHandler)
import Howdy.Internal.Error (HowdyException)
import Lens.Micro (to)

type Permission = CommandInput -> Bool
type Command = HowdyHandler CommandReplyData CommandInput ()

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
        , cdRunner     = pure ()
        , cdDebug      = False
        }


(>>) :: (b -> c) -> (a -> b) -> a -> c
(>>) = (.)

alias :: [Text] -> CommandDefinition -> CommandDefinition
alias value cd = cd{ cdAlias = cd.cdAlias ++ value }


desc :: Text -> CommandDefinition -> CommandDefinition
desc value cd = cd{ cdDesc = Just value }


hide :: CommandDefinition -> CommandDefinition
hide cd = cd{ cdHidden = True }


legacy :: Text -> CommandDefinition -> CommandDefinition
legacy value cd = cd{ cdIdent = value }


permission :: Permission -> CommandDefinition -> CommandDefinition
permission value cd = cd{ cdPermission = value }

run :: Command -> CommandDefinition -> CommandDefinition
run x cd = cd{ cdRunner = x }
