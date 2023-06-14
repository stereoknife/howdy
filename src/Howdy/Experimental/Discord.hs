{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Howdy.Experimental.Discord where

type family HowdyCombined r a where
    HowdyCombined (r:rs) a = r a