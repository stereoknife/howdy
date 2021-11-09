{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Howdy.Error (contain, catch, HowdyException) where

import           Control.Monad.Except (MonadError (throwError))
import           Howdy.Internal.Error (HowdyException, KnownError (..))

contain :: (MonadError HowdyException m, KnownError e) => Either e a -> m a
contain (Right a) = pure a
contain (Left e)  = throwError . absorb $ e

catch :: (MonadError HowdyException m, KnownError e) => m (Either e a) -> m a
catch e = e >>= contain
