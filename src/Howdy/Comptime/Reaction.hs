{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Howdy.Comptime.Reaction
    ( Reaction
    , Permission
    , ReactionDefinition (..)
    , ReactionInput (..)
    , EmojiIdentifier (..)
    , ReactionReplyData (..)
    , toIdentifier
    , alias
    , desc
    , hide
    , legacy
    , permission
    , run
    , (Howdy.Comptime.Reaction.>>)
    ) where

import Control.Monad.Except (ExceptT)
import Control.Optics (Lensed (..))
import Data.Default (Default (..))
import Data.Hashable (Hashable (..))
import Data.Text (Text)
import Discord (DiscordHandler)
import Discord.Types (ChannelId, DiscordId (DiscordId), Emoji (..), EmojiId, GuildId,
                      MessageReference, Snowflake (Snowflake), User, UserId)
import Howdy.Internal.Discord (HowdyHandler)
import Howdy.Internal.Error (HowdyException)
import Lens.Micro (to)


data EmojiIdentifier
    = Unicode Text
    | Custom EmojiId
    deriving (Eq)

toIdentifier :: Emoji -> EmojiIdentifier
toIdentifier e =
    case e.emojiId of
        Just id -> Custom id
        Nothing -> Unicode e.emojiName

instance Hashable EmojiIdentifier where
    hashWithSalt salt (Unicode a)                        = hashWithSalt salt a
    hashWithSalt salt (Custom (DiscordId (Snowflake a))) = hashWithSalt salt a

type Reaction = HowdyHandler ReactionReplyData ReactionInput ()
type Permission = ReactionInput -> Bool

data ReactionReplyData = ReactionReplyData
    { rrdChannelId  :: ChannelId
    , rrdWhisperId  :: UserId
    , rrdMessageRef :: MessageReference
    }

instance Lensed ReactionReplyData ChannelId where
    focus = to rrdChannelId

instance Lensed ReactionReplyData MessageReference where
    focus = to rrdMessageRef

instance Lensed ReactionReplyData UserId where
    focus = to rrdWhisperId

data ReactionDefinition = ReactionDefinition
    { rdAlias      :: [EmojiIdentifier]
    , rdDesc       :: Maybe Text
    , rdHidden     :: Bool
    , rdIdent      :: EmojiIdentifier
    , rdPermission :: Permission
    , rdRunner     :: Reaction
    , rdDebug      :: Bool -- TODO: change to debug flags and later to customizable flags
    }

data ReactionInput = ReactionInput
    { target  :: EmojiIdentifier
    , reacter :: User
    , author  :: User
    , guild   :: Maybe GuildId
    , channel :: ChannelId
    , args    :: Text
    , ref     :: Maybe MessageReference
    }

instance Default ReactionDefinition where
    def = ReactionDefinition
        { rdAlias = []
        , rdDesc = Nothing
        , rdHidden = False
        , rdIdent = Unicode ""
        , rdPermission = const True
        , rdRunner = pure ()
        , rdDebug = False
        }


(>>) :: (b -> c) -> (a -> b) -> a -> c
(>>) = (.)


alias :: [EmojiIdentifier] -> ReactionDefinition -> ReactionDefinition
alias x rd = rd{ rdAlias = x }


desc :: Text -> ReactionDefinition -> ReactionDefinition
desc x rd = rd{ rdDesc = Just x }


hide :: ReactionDefinition -> ReactionDefinition
hide rd = rd{ rdHidden = True }


legacy :: EmojiIdentifier -> ReactionDefinition -> ReactionDefinition
legacy x rd = rd{ rdIdent = x }


permission :: Permission -> ReactionDefinition -> ReactionDefinition
permission x rd = rd{ rdPermission = x }


run :: Reaction -> ReactionDefinition -> ReactionDefinition
run x rd = rd{ rdRunner = x }
