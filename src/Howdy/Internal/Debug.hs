{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE NoFieldSelectors #-}

module Howdy.Internal.Debug where

import Control.Monad.Free (Free (Free), liftF)
import Data.Coerce (coerce)
import Data.Default (Default (def))
import Discord.Types (ChannelId, DiscordId (DiscordId), Snowflake (Snowflake))

data DebugOptions = DebugOptions
    { pipeLogs         :: Bool
    , pipeLogsTarget   :: Maybe ChannelId
    , enableDebugFlags :: Bool
    }

instance Default DebugOptions where
    def = DebugOptions
        { pipeLogs = False
        , pipeLogsTarget = Nothing
        , enableDebugFlags = False
        }

data DBCMD a = PipeLogs Snowflake a
             | DisableDebugging a
             deriving Functor

type DebugBuilder = Free DBCMD

pipeLogsTo :: Snowflake -> DebugBuilder ()
pipeLogsTo x = liftF ((PipeLogs x) ())

disableDebugging :: DebugBuilder ()
disableDebugging = liftF (DisableDebugging ())
