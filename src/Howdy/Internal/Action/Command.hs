{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Howdy.Internal.Action.Command where

import Discord (DiscordHandler)
import Data.Text ( Text )
import Data.Hashable (Hashable (hash))
import Data.Kind (Constraint)
import GHC.Records (HasField)
import Howdy.Parser (Parser (runParser), firstof, string)
import Discord.Types (UserId, GuildId, ChannelId, Message (messageContent))
import Howdy.Effects.Discord (Discord)
import Control.Monad.Except (MonadError)
import Howdy.Internal.Error (HowdyException)
import Data.Data (dataTypeName, Data (dataTypeOf))
import Data.Functor.Classes (Show1 (liftShowsPrec), showsUnaryWith, showsPrec1)
import Data.Bifunctor (Bifunctor(..))
import Control.Monad.Free
import Control.Monad.Free.Church
import Data.Default (Default (def))

type TY_Permission = Bool
type TY_Runner = DiscordHandler ()

data CommandMeta = CommandMeta { alias      :: [Text]
                               , desc       :: Maybe Text
                               , hidden     :: Bool
                               }

instance Default CommandMeta where
    def = CommandMeta [] Nothing False

data CommandRunner = CommandRunner { ident      :: Int
                                   , permission :: TY_Permission
                                   , runner     :: TY_Runner
                                   }

instance Default CommandRunner where
    def = CommandRunner 0 True (pure ())

data BC a = Alias [Text] a
          | Desc Text a
          | Hide a
          | Ident Int a
          | Permission Bool a
          | Runner (DiscordHandler ()) a
          deriving Functor

type Command = (CommandMeta, CommandRunner)

instance Default Command where
    def = (def, def)

build' :: CommandBuilder a -> Command -> Command
build' (Free (Alias      x next)) (m,r) = build' next (m { alias = m.alias ++ x }, r)
build' (Free (Desc       x next)) (m,r) = build' next (m { desc = Just x }, r)
build' (Free (Hide         next)) (m,r) = build' next (m { hidden = True }, r)
build' (Free (Ident      x next)) (m,r) = build' next (m, r { ident = x })
build' (Free (Permission x next)) (m,r) = build' next (m, r { permission = x })
build' (Free (Runner     x next)) (m,r) = build' next (m, r { runner = x })

type CommandBuilder = Free BC

command :: Text -> CommandBuilder () -> Command
command x f = build' f (m, r)
    where m = CommandMeta [x] Nothing False
          r = CommandRunner (hash x) True (pure ())

-- Meta

alias :: [Text] -> CommandBuilder ()
alias x = liftF (Alias x ())

desc :: Text -> CommandBuilder ()
desc x = liftF (Desc x ())

hide :: CommandBuilder ()
hide = liftF (Hide ())

-- Runner

legacy :: Text -> CommandBuilder ()
legacy x = liftF (Ident (hash x) ())

permission :: TY_Permission -> CommandBuilder ()
permission x = liftF (Permission x ())

run :: TY_Runner -> CommandBuilder ()
run x = liftF (Runner x ())

-- Something Else --

data CommandInput = CommandInput { target  :: Int
                                 , user    :: UserId
                                 , guild   :: Maybe GuildId
                                 , channel :: ChannelId
                                 , args    :: Text
                                 }

permit :: MonadError HowdyException m => CommandRunner -> CommandInput -> m ()
permit = undefined