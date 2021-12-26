{-# LANGUAGE TypeFamilies #-}
module Howdy.Internal.Builder where
import           Data.Kind (Type)

class Builder a where
    type BuilderOutput a :: Type
    build :: a -> BuilderOutput a
