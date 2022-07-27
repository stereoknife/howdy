{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Howdy.Internal.Error where

import           Data.Semigroup (Semigroup)
import           Discord        (RestCallErrorCode)
import           Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Control.Monad.Except (MonadError (throwError), ExceptT (ExceptT), runExceptT)

-- TODO: Combine multiple errors

type Alias = Text
type Identifier = Text
type ResponseCode = Int

type DebugException = (Bool, HowdyException)

data HowdyException where
    -- Command Errors
    UnknownIdentifier :: HowdyException
    CommandNotFound   :: HowdyException
    ForbiddenCommand  :: HowdyException

    -- Reaction Errors
    ReactionMissing   :: HowdyException

    -- Parse errors
    ParseError        :: HowdyException

    -- Discord Errors
    DiscordError      :: RestCallErrorCode -> HowdyException

    -- Other
    Ignore            :: HowdyException
    UnknownError      :: HowdyException
    HTTPError         :: ResponseCode -> Text -> HowdyException

    deriving (Eq, Ord)

class KnownError e where
    absorb :: e -> HowdyException

instance KnownError RestCallErrorCode where
    absorb = DiscordError

instance KnownError HowdyException where
    absorb = id

instance KnownError DebugException where
    absorb = snd

contain :: (MonadError HowdyException m, KnownError e) => Either e a -> m a
contain (Right a) = pure a
contain (Left e)  = throwError . absorb $ e

catch :: (MonadError HowdyException m, KnownError e) => m (Either e a) -> m a
catch e = e >>= contain

report :: (MonadError HowdyException m) => HowdyException -> Maybe a -> m a
report e Nothing  = throwError e
report _ (Just v) = pure v

ignore :: HowdyException -> DebugException
ignore = pair False

debug :: HowdyException -> DebugException
debug = pair True

pair :: a -> b -> (a, b)
pair = curry id

-- Not used atm but leaving it here for future reference

-- data TestErr where
--     TestErr :: Typeable a => a -> TestErr

-- pattern Opt1 :: String -> TestErr
-- pattern Opt1 a <- ((\(TestErr e) -> cast e) -> Just a)
