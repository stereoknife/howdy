{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Howdy.Internal.Action.Command where

import Discord ( def, DiscordHandler )
import Data.Text ( Text )
import Data.Hashable ( Hashable(hash) )
import Control.Monad.Free ( Free(Free), liftF )
import Data.Default ( Default(..) )

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
