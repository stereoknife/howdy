module Howdy.Discord
    ( send
    , sendTo
    , embed
    , embedTo
    , reply
    , replyTo
    , whisper
    , whisperTo
    , react
    , reactTo
    , bold
    , italics
    , underline
    , code
    , codeblock
    ) where

import Howdy.Internal.Discord
import Howdy.Text ( wrap )
import Data.Text (Text)

bold :: Text -> Text
bold = wrap "**"

italics :: Text -> Text
italics = wrap "_"

underline :: Text -> Text
underline = wrap "__"

code :: Text -> Text
code = wrap "`"

codeblock :: Text -> Text -> Text
codeblock lang t = "```" <> lang <> "\n" <> t <> "```"