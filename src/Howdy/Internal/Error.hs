{-# LANGUAGE GADTs #-}

module Howdy.Internal.Error where

import           Discord (RestCallErrorCode)

data HowdyException where
    ParseError :: HowdyException
    DiscordError :: HowdyException
    CommandMissing :: HowdyException
    UnknownError :: HowdyException

class KnownError e where
    absorb :: e -> HowdyException

instance KnownError RestCallErrorCode where
    absorb _ = DiscordError
