{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Howdy.Internal.Action.Run where

import           Control.Applicative         (Alternative (..))
import           Control.Monad.Except        (ExceptT, MonadError (throwError),
                                              MonadTrans (lift), runExceptT,
                                              void)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader, ReaderT (runReaderT),
                                              asks)
import           Control.Monad.State         (MonadState, StateT, evalStateT)
import           Control.Monad.Writer        (MonadWriter (tell), Writer,
                                              execWriter)
import           Data.Default                (Default (def))
import           Data.Either                 (fromRight)
import           Data.Semigroup              (Semigroup)
import           Data.Text                   (Text)
import           Discord                     (DiscordHandler)
import           Discord.Types               (Message, User)
import           Howdy.Context               (Context (ctx))
import           Howdy.Discord.Class         (Discord (liftDiscord), Reply)
import           Howdy.Internal.Error        (HowdyException (CommandMissing))
import           Howdy.Internal.Parser.Class (MonadParse)


-- TODO: These stacks are terrible, please change them for something nicer eventually
newtype CommandRunner a = CommandRunner { runCommand :: StateT Text (ReaderT (Message, User) (ExceptT HowdyException DiscordHandler)) a }
    deriving (Functor, Applicative, Monad, MonadError HowdyException, MonadParse, MonadReader (Message, User))

-- TODO: Change store for a nicer data structure
newtype ReactionRunner a = ReactionRunner { runReaction :: ReaderT (Message, User) (ExceptT HowdyException DiscordHandler) a }
    deriving (Functor, Applicative, Monad, MonadError HowdyException, MonadReader (Message, User))

class Discord m => Exec e m where
    exec :: Default a => e -> m a -> DiscordHandler a

-- Command Instances

instance Discord CommandRunner where
    liftDiscord = CommandRunner . lift . lift . lift

instance Reply CommandRunner

instance Exec (Text, Message, User) CommandRunner where
    exec (t, m, u) = fmap (fromRight def) . runExceptT . flip runReaderT (m, u) . flip evalStateT t . runCommand

instance Context Message CommandRunner where
    ctx = asks fst

instance Context User CommandRunner where
    ctx = asks snd

instance Alternative CommandRunner where
  empty = throwError CommandMissing
  a <|> b = CommandRunner $ runCommand a <|> runCommand b


-- Reaction Instances

instance Discord ReactionRunner where
    liftDiscord = ReactionRunner . lift . lift

instance Exec (Message, User) ReactionRunner where
    exec e = fmap (fromRight def) . runExceptT . flip runReaderT e . runReaction

instance Context Message ReactionRunner where
    ctx = asks fst

instance Context User ReactionRunner where
    ctx = asks snd
