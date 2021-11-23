{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Howdy.Internal.Action.RunFree where
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.Reader
import           Data.Default
import           Data.Text            (Text)
import           Discord
import           Howdy.Discord.Class
import           Howdy.Error




-- TODO: These stacks are terrible, please change them for something nicer eventually
--newtype CommandRunner a = CommandRunner { runCommand :: StateT Text (ReaderT (Message, User) (ExceptT HowdyException DiscordHandler)) a }
--    deriving (Functor, Applicative, Monad, MonadError HowdyException, MonadParse, MonadReader (Message, User), MonadThrow, MonadIO  )

-- TODO: Change store for a nicer data structure
newtype ReactionRunner a = ReactionRunner { runReaction :: ReaderT (Message, User) (ExceptT HowdyException DiscordHandler) a }
    deriving (Functor, Applicative, Monad, MonadError HowdyException, MonadReader (Message, User), MonadThrow, MonadIO)

class MonadDiscord m => MonadExec e m where
    exec :: Default a => e -> m a -> DiscordHandler a

data CommandRunnerF next = DoDiscord (DiscordHandler ()) next
                         | DoNetwork (IO ()) next
                         | DoReadMessage (Message -> next)
                         | DoReadUser (User -> next)
                         deriving (Functor)

type CommandRunner = Free CommandRunnerF

interpret :: CommandRunner a -> DiscordHandler a
interpret = foldFree $ \case
    DoDiscord dh next -> dh >> pure next
    DoNetwork io next -> liftIO io >> pure next
    DoReadMessage f   -> undefined
    DoReadUser f      -> undefined

reply :: Text -> CommandRunner ()
reply t = Free $ DoDiscord rep $ Pure ()
    where rep = undefined
