{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Howdy.Error ( contain
                   , catch
                   , report
                   , HowdyException (..)
                   , MonadError (..)
                   ) where

import           Control.Monad.Except (ExceptT (ExceptT),
                                       MonadError (throwError), runExceptT)
import           Howdy.Internal.Error (HowdyException (..), KnownError (..),
                                       catch, contain, report)

-- contain :: (MonadError HowdyException m, KnownError e) => Either e a -> m a
-- contain (Right a) = pure a
-- contain (Left e)  = throwError . absorb $ e

-- catch :: (MonadError HowdyException m, KnownError e) => m (Either e a) -> m a
-- catch e = e >>= contain

-- report :: (MonadError HowdyException m) => HowdyException -> Maybe a -> m a
-- report e Nothing  = throwError e
-- report _ (Just v) = pure v

-- recover :: (e -> ExceptT e m a) -> ExceptT e m a
-- recover f = ExceptT (m (Either e a))
--     where ei = runExceptT ei
