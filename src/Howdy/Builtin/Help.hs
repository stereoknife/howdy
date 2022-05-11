{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Howdy.Builtin.Help where

import Howdy.Internal.Bot.Builder (BotPreferences (commandsData))
import Howdy.Internal.Command
import Data.Text (Text)
import Data.Maybe (fromMaybe)

help :: BotPreferences -> Command
help b i = do let text = fmap formatCommand b.commandsData
              pure ()

formatCommand :: CommandMeta -> Text
formatCommand CommandMeta { hidden = True } = ""
formatCommand CommandMeta { alias, desc = Just desc } = "**" <> head alias <> ":** " <> desc
formatCommand CommandMeta { alias } = "**" <> head alias <> "**"