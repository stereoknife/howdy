{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Howdy.Context where

import           Data.Kind    (Constraint, Type)
import           GHC.TypeLits (Symbol)

-- Get is a wrapper around Reader
class Monad m => Get a m where
    get :: m a
    gets :: (a -> b) -> m b
    gets f = fmap f get

type family Gets k m :: Constraint where
    Gets '[] _ = ()
    Gets (k : ks) m = (Get k m, Gets ks m)

-- Set is a wrapper around State
class Get a m => Set a m where
    set :: a -> m a
    sets :: (a -> a) -> m a
    sets f = gets f >>= set

type family Sets k m :: Constraint where
    Sets '[] _ = ()
    Sets (k : ks) m = (Set k m, Sets ks m)

class Functor f => Property (a :: Symbol) f where
    type Key a :: Type
    prop :: f (Key a)

class Exposes a b where
    exp :: a -> b
