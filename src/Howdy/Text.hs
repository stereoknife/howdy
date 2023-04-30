module Howdy.Text (wrap) where

import Data.Text (Text)

wrap :: Text -> Text -> Text
wrap w t = w <> t <> w