{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE NoFieldSelectors #-}

module Howdy.Internal.Bot.Debug where

import           Control.Monad.Free (Free (Free), liftF)
import           Data.Coerce        (coerce)
import           Discord.Types      (ChannelId, DiscordId (DiscordId),
                                     Snowflake (Snowflake))

data DebugOptions = DebugOptions
    { pipeLogs         :: Bool
    , pipeLogsTarget   :: ChannelId
    , enableDebugFlags :: Bool
    }

data DBCMD a = PipeLogs Snowflake a
             | DisableDebugging a
             deriving Functor

type DebugBuilder = Free DBCMD

build :: DebugBuilder a -> DebugOptions -> DebugOptions
build (Free (PipeLogs         x next)) o = build next $ o { pipeLogs = True, pipeLogsTarget = coerce x }
build (Free (DisableDebugging   next)) o = build next $ o { enableDebugFlags = False }

pipeLogsTo :: Snowflake -> DebugBuilder ()
pipeLogsTo x = liftF ((PipeLogs x) ())

disableDebugging :: DebugBuilder ()
disableDebugging = liftF (DisableDebugging ())
