{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Howdy.Internal.Util where
import Data.Kind

type family ToConstraints (cs :: [(Type -> Type) -> Constraint]) (m :: (Type -> Type)) :: Constraint where
    ToConstraints (c:cs) m = (c m, ToConstraints cs m)
    ToConstraints '[] _ = ()