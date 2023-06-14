{-# LANGUAGE TypeFamilies #-}

module Howdy
    ((Howdy.>>)
    , bot
    , start
    ) where

import Howdy.Bot

infixl 1 >>
(>>) :: (b -> c) -> (a -> b) -> a -> c
(>>) = (.)

type family CommandWith