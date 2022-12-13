{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Optics where

import Control.Monad.Reader (MonadReader (..), asks)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy)
import GHC.Records (HasField (getField))
import Lens.Micro (SimpleGetter)
import Lens.Micro.Extras (view)


-- Even though this is called Lens in reality it only uses Getters
-- because the library does not need setters

--data Lensed a where
--    Lensed :: WithLens s a => s -> Lensed a

class Lensed s a where
    focus :: SimpleGetter s a

{- data LensT l m a where
    LensT :: (Lensed l -> m a) -> LensT l m a

runLensT :: LensT l m a -> Lensed l -> m a
runLensT = undefined

instance Functor m => Functor (LensT l m) where
    fmap f a = LensT $ \l -> fmap f $ runLensT a l

instance Applicative m => Applicative (LensT l m) where
    f <*> a = LensT $ \l -> runLensT f l <*> runLensT a l
    pure = LensT . const . pure

instance Monad m => Monad (LensT l m) where
    return = pure
    m >>= f = LensT $ \l -> do v <- runLensT m l
                               runLensT (f v) l
 -}
-- instance HasField k s a => WithLens s a where
--   focus = to $ getField @k

class WithLens a m where
    look :: m a

instance (MonadReader r m, Lensed r a) => WithLens a m where
    look = asks $ view focus


type family WithLenses (s :: [Type]) (m :: Type -> Type) :: Constraint where
    WithLenses (s:ss) m = (WithLens s m, WithLenses ss m)
    WithLenses '[] m = ()
