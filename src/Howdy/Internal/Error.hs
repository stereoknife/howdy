{-# LANGUAGE GADTs #-}

module Howdy.Internal.Error where

import           Data.Semigroup (Semigroup)
import           Discord        (RestCallErrorCode)

-- TODO: Combine multiple errors

data HowdyException where
    ParseError :: HowdyException
    CommandMissing :: HowdyException
    ReactionMissing :: HowdyException
    DiscordError :: HowdyException
    UnknownError :: HowdyException
    deriving (Eq, Ord, Bounded)

-- Keep the first error
instance Semigroup HowdyException where
    (<>) = const

instance Monoid HowdyException where
    mempty = UnknownError

class KnownError e where
    absorb :: e -> HowdyException

instance KnownError RestCallErrorCode where
    absorb _ = DiscordError
