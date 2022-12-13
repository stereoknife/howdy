{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Howdy.Internal.Logging.Fmt where

import Data.Text (Text, unpack)

class Fmt a where
    format :: a -> Text
    formatList :: [a] -> Text
    formatList = formatList'

instance Fmt Text where
    format = id

-- instance Fmt String where
--     format = text
formatList' :: Fmt a => [a] -> Text
formatList' a = brackets (go a)
    where go (x:[]) = format x
          go (x:xs) = format x `comma` go xs

render :: Text -> String
render = unpack

brackets :: Text -> Text
brackets a = "(" <> a <> ")"

comma :: Text -> Text -> Text
comma a b = a <> ", " <> b
