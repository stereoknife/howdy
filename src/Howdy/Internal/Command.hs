{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Howdy.Internal.Command where

import Discord ( def, DiscordHandler )
import Data.Text ( Text )
import Data.Hashable ( Hashable(hash) )
import Control.Monad.Free ( Free(Free), liftF )
import Data.Default ( Default(..) )
import Howdy.Error (HowdyException, MonadError)
import Control.Monad.Except (ExceptT, runExceptT)
import Discord.Types ( ChannelId, GuildId, UserId )
import Howdy.Internal.Discord (MonadDiscord)

data CommandMeta = CommandMeta { alias      :: [Text]
                               , desc       :: Maybe Text
                               , hidden     :: Bool
                               }

data CommandPreferences = CommandRunner { ident      :: Text
                                        , permission :: CommandInput -> Bool
                                        , runner     :: Command
                                        , debug      :: Bool -- TODO: change to debug flags and later to customizable flags
                                        }

data CommandInput = CommandInput { target  :: Text
                                 , user    :: UserId
                                 , guild   :: Maybe GuildId
                                 , channel :: ChannelId
                                 , args    :: Text
                                 }

data CBCMD a = Alias [Text] a
            | Desc Text a
            | Hide a
            | Ident Text a
            | Permission (CommandInput -> Bool) a
            | Runner Command a
            deriving Functor

type CommandData = (CommandMeta, CommandPreferences)

type CommandDefinition = Free CBCMD

type Command = forall m. (MonadError HowdyException m, MonadDiscord m) => CommandInput -> m ()

build' :: CommandDefinition a -> CommandData -> CommandData
build' (Free (Alias      x next)) (m,r) = build' next (m { alias = m.alias ++ x }, r)
build' (Free (Desc       x next)) (m,r) = build' next (m { desc = Just x }, r)
build' (Free (Hide         next)) (m,r) = build' next (m { hidden = True }, r)
build' (Free (Ident      x next)) (m,r) = build' next (m, r { ident = x })
build' (Free (Permission x next)) (m,r) = build' next (m, r { permission = x })
build' (Free (Runner     x next)) (m,r) = build' next (m, r { runner = x })


command :: Text -> CommandDefinition () -> CommandData
command x f = build' f (m, r)
    where m = CommandMeta [x] Nothing False
          r = CommandRunner x (const True) (const $ undefined) False

-- Meta

alias :: [Text] -> CommandDefinition ()
alias x = liftF (Alias x ())

desc :: Text -> CommandDefinition ()
desc x = liftF (Desc x ())

hide :: CommandDefinition ()
hide = liftF (Hide ())

-- Runner

legacy :: Text -> CommandDefinition ()
legacy x = liftF (Ident x ())

permission :: (CommandInput -> Bool) -> CommandDefinition ()
permission x = liftF (Permission x ())

run :: Command -> CommandDefinition ()
run x = liftF (Runner x ())
