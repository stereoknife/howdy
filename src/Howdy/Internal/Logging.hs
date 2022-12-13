{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Howdy.Internal.Logging ((<<), Fmt(..), (<+)) where

import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import qualified Debug.Trace
import Howdy.Internal.Logging.Fmt (Fmt (format), render)
import System.IO.Unsafe (unsafePerformIO)

-- Everybody loves some good old C++ cout << "str"
infixr 0 <<
(<<) :: b -> String -> b
(<<) b a = seq (unsafePerformIO $ putStr a) b

(<+) :: Show a => String -> a -> String
(<+) s a = s ++ show a

-- infixr 0 `line`
-- line :: a -> String -> a
-- line a t = put "\n" $ put t a

-- infixr 0 <..<
-- (<..<) :: a -> String -> a
-- (<..<) = inline

-- infixr 0 `inline`
-- inline :: a -> String -> a
-- inline = flip put

-- put :: String -> a -> a
-- put t a = seq (unsafePerformIO (putStr t)) a

