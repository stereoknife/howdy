{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Howdy.Internal.Reaction where

import Discord ( def, DiscordHandler )
import Data.Text ( Text )
import Data.Hashable ( Hashable(..) )
import Control.Monad.Free ( Free(Free), liftF )
import Data.Default ( Default(..) )
import Howdy.Error (HowdyException, MonadError)
import Control.Monad.Except (ExceptT, runExceptT, MonadIO)
import Discord.Types
    ( DiscordId(DiscordId), EmojiId, Snowflake(Snowflake) )
import Howdy.Internal.Discord (MonadDiscord, request)
import Control.Monad.Reader (MonadReader, asks, ReaderT)
import qualified Discord.Requests as R
import Data.Kind (Type, Constraint)
import GHC.Records (HasField)
import Howdy.Internal.Command (CommandInput)
import Control.Monad.Catch (MonadThrow)

data EmojiIdentifier = Unicode Text
                     | Custom EmojiId
                     deriving (Eq)

instance Hashable Snowflake where
    hashWithSalt i (Snowflake s) = hashWithSalt i s

instance Hashable EmojiId where
    hashWithSalt i (DiscordId s) = hashWithSalt i s

instance Hashable EmojiIdentifier where
    hashWithSalt i (Unicode t) = hashWithSalt i t
    hashWithSalt i (Custom e) = hashWithSalt i e

data ReactionMeta = ReactionMeta { alias      :: [EmojiIdentifier]
                                 , desc       :: Maybe Text
                                 , hidden     :: Bool
                                 }

data ReactionPreferences = ReactionPreferences { ident      :: EmojiIdentifier
                                               , permission :: ReactionInput -> Bool
                                               , runner     :: Reaction
                                               , debug      :: Bool -- TODO: change to debug flags and later to customizable flags
                                               }

type ReactionInput = CommandInput

data RBCMD a = Alias [EmojiIdentifier] a
             | Desc Text a
             | Hide a
             | Ident EmojiIdentifier a
             | Permission (ReactionInput -> Bool) a
             | Runner Reaction a
             deriving Functor

type ReactionData = (ReactionMeta, ReactionPreferences)

type ReactionDefinition = Free RBCMD

type Reaction = forall m. ReaderT CommandInput (ExceptT HowdyException DiscordHandler) ()

type ReactionReqs m = (MonadDiscord m, MonadReader CommandInput m, MonadError HowdyException m, MonadIO m, MonadThrow m)

type ReactionWith a = forall m. (ReactionWithReqs a m) => m ()

type family ReactionWithReqs (c :: [(Type -> Type) -> Constraint]) (m :: Type -> Type):: Constraint where
    ReactionWithReqs (c : '[]) m = (c m, ReactionReqs m)
    ReactionWithReqs (c : cs) m = (c m, ReactionWithReqs cs m)

build' :: ReactionDefinition a -> ReactionData -> ReactionData
build' (Free (Alias      x next)) (m,r) = build' next (m { alias = m.alias ++ x }, r)
build' (Free (Desc       x next)) (m,r) = build' next (m { desc = Just x }, r)
build' (Free (Hide         next)) (m,r) = build' next (m { hidden = True }, r)
build' (Free (Ident      x next)) (m,r) = build' next (m, r { ident = x })
build' (Free (Permission x next)) (m,r) = build' next (m, r { permission = x })
build' (Free (Runner     x next)) (m,r) = build' next (m, r { runner = x })
build' _ x = x

mkReaction :: EmojiIdentifier -> ReactionDefinition () -> ReactionData
mkReaction x f = build' f (m, r)
    where m = ReactionMeta [x] Nothing False
          r = ReactionPreferences x (const True) (undefined) False

-- Meta

alias :: [EmojiIdentifier] -> ReactionDefinition ()
alias x = liftF (Alias x ())

desc :: Text -> ReactionDefinition ()
desc x = liftF (Desc x ())

hide :: ReactionDefinition ()
hide = liftF (Hide ())

-- Runner

legacy :: EmojiIdentifier -> ReactionDefinition ()
legacy x = liftF (Ident x ())

permission :: (ReactionInput -> Bool) -> ReactionDefinition ()
permission x = liftF (Permission x ())

run :: Reaction -> ReactionDefinition ()
run x = liftF (Runner x ())