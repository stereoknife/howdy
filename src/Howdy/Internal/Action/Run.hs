{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Howdy.Internal.Action.Run where

import Control.Monad.Except ( ExceptT, MonadError, MonadTrans (lift) )
import Control.Monad.State ( StateT, MonadState (put) )
import Data.Text (Text)
import Discord ( DiscordHandler )
import Howdy.Internal.Error ( HowdyException )
import Howdy.Effects.Discord (Discord, Reply, liftDiscord)
import Howdy.Context
import Howdy.Effects.Parse (Parse)

newtype ActionRunner a = ActionRunner { runAction :: ExceptT HowdyException (StateT Text DiscordHandler) a }
    deriving (Functor, Applicative, Monad, MonadError HowdyException, MonadState Text)

instance Get Text ActionRunner where
    get = get

instance Set Text ActionRunner where
    set = put

instance Parse ActionRunner
instance Discord ActionRunner where
    liftDiscord = ActionRunner . lift . lift
instance Reply ActionRunner