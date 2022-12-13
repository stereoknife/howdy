{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE NoFieldSelectors #-}

module Howdy.Internal.Debug where

import Data.Coerce (coerce)
import Data.Default (Default (def))
import Discord.Types (ChannelId, DiscordId (DiscordId), Snowflake (Snowflake))

data DebugOptions = DebugOptions
    { pipeLogs         :: Bool
    , pipeLogsTarget   :: Maybe ChannelId
    , enableDebugFlags :: Bool
    } deriving (Show)

instance Default DebugOptions where
    def = DebugOptions
        { pipeLogs = False
        , pipeLogsTarget = Nothing
        , enableDebugFlags = False
        }
