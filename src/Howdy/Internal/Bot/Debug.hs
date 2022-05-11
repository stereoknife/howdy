{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DeriveFunctor #-}

module Howdy.Internal.Bot.Debug where

import Discord.Types
import Control.Monad.Free (Free (Free), liftF)

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
build (Free (PipeLogs         x next)) o = build next $ o { pipeLogs = True, pipeLogsTarget = x }
build (Free (DisableDebugging   next)) o = build next $ o { enableDebugFlags = False }

pipeLogsTo :: Snowflake -> DebugBuilder ()
pipeLogsTo x = liftF ((PipeLogs x) ())

disableDebugging :: DebugBuilder ()
disableDebugging = liftF (DisableDebugging ())