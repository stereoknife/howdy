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

import           Control.Monad.Reader (MonadReader (..), asks)
import           Data.Kind            (Constraint, Type)
import           Data.Proxy           (Proxy)
import           Data.Tuple.Optics    (Field1 (_1), Field2 (_2))
import           GHC.Records          (HasField (getField))
import           Optics               (Getter, Lens, castOptic, to, view, (%))
import qualified Optics


-- Even though this is called Lens in reality it only uses Getters
-- because the library does not need setters

data Lensed a where
    Lensed :: WithLens s a => s -> Lensed a

class WithLens s a where
    focus :: Getter s a

data LensT l m a where
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

-- instance HasField k s a => WithLens s a where
--   focus = to $ getField @k

class MonadLens a m where
    get :: m a

type family MonadLenses (s :: [Type]) (m :: Type -> Type) :: Constraint where
    MonadLenses (s:ss) m = (MonadLens s m, MonadLenses ss m)
    MonadLenses '[] m = ()
