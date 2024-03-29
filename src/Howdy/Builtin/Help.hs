{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}

module Howdy.Builtin.Help where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
--import Howdy.Comptime.Bot (BotPreferences (commandsData))
import Howdy.Comptime.Command

{- help :: BotPreferences -> Command
help b i = do let text = fmap formatCommand b.commandsData
              pure ()

formatCommand :: CommandMeta -> Text
formatCommand CommandMeta { hidden = True }           = ""
formatCommand CommandMeta { alias, desc = Just desc } = "**" <> head alias <> ":** " <> desc
formatCommand CommandMeta { alias }                   = "**" <> head alias <> "**"
 -}
