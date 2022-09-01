{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Howdy.Internal.Command where

import           Control.Monad.Catch    (Exception, MonadThrow)
import           Control.Monad.Except   (ExceptT, MonadIO, MonadTrans (lift),
                                         runExceptT)
import           Control.Monad.Free     (Free (Free), _Free, liftF)
import           Control.Monad.Reader   (MonadReader, ReaderT (runReaderT), ask)
import           Control.Optics         (MonadLens (get), WithLens (..))
import           Data.Default           (Default (..))
import           Data.Hashable          (Hashable (hash))
import           Data.Kind              (Constraint, Type)
import           Data.Text              (Text)
import           Discord                (DiscordHandler, def)
import qualified Discord.Requests       as R
import           Discord.Types          (ChannelId, GuildId, MessageReference,
                                         User, UserId)
import           GHC.Records            (HasField)
import           Howdy.Error            (HowdyException, MonadError)
import           Howdy.Internal.Discord (MonadDiscord (liftDiscord), request)
import           Optics                 (to, view)

data CommandMeta = CommandMeta { alias  :: [Text]
                               , desc   :: Maybe Text
                               , hidden :: Bool
                               }

data CommandPreferences = CommandPreferences { ident      :: Text
                                             , permission :: CommandInput -> Bool
                                             , runner     :: CommandInput -> ExceptT HowdyException DiscordHandler ()
                                             , debug      :: Bool -- TODO: change to debug flags and later to customizable flags
                                             }

data CommandInput = CommandInput { target  :: Text
                                 , author  :: User
                                 , guild   :: Maybe GuildId
                                 , channel :: ChannelId
                                 , args    :: Text
                                 , ref     :: Maybe MessageReference
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

type CommandWrapper e = ReaderT CommandInput (ExceptT e DiscordHandler)
type Command = forall e. Exception e => CommandWrapper e ()

instance WithLens CommandInput ChannelId where
  focus = to (.channel)

instance Monad m => MonadLens ChannelId (ReaderT CommandInput m) where
  get = view focus <$> ask

instance MonadDiscord (CommandWrapper e) where
  liftDiscord = lift . lift

type CommandReqs m = (MonadDiscord m, MonadIO m, MonadThrow m)

type CommandWith a = forall m. (CommandWithReqs a m) => m ()

type family CommandWithReqs (c :: [(Type -> Type) -> Constraint]) (m :: Type -> Type):: Constraint where
    CommandWithReqs (c : '[]) m = (c m, CommandReqs m)
    CommandWithReqs (c : cs) m = (c m, CommandWithReqs cs m)

build' :: CommandDefinition a -> CommandData -> CommandData
build' (Free (Alias      x next)) (m,r) = build' next (m { alias = m.alias ++ x }, r)
build' (Free (Desc       x next)) (m,r) = build' next (m { desc = Just x }, r)
build' (Free (Hide         next)) (m,r) = build' next (m { hidden = True }, r)
build' (Free (Ident      x next)) (m,r) = build' next (m, r { ident = x })
build' (Free (Permission x next)) (m,r) = build' next (m, r { permission = x })
build' (Free (Runner     x next)) (m,r) = build' next (m, r { runner = (\ci -> runReaderT x ci) })
build' _ x = x


mkCommand :: Text -> CommandDefinition () -> CommandData
mkCommand x f = build' f (m, r)
    where m = CommandMeta [x] Nothing False
          r = CommandPreferences x (const True) (undefined) False

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
