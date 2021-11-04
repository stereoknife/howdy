module Howdy.Actions where

import Data.Text (Text)
import Discord (DiscordHandler)
import Control.Monad.State (StateT)

-- Command context has message/parser as reader (state but it don't matter) around a DiscordHandler output

newtype ActionContext a = ActionContext { runAction :: StateT Text DiscordHandler a }

data Command = Command { alias :: Text
                       , desc  :: Text
                       , action :: ActionContext ()
                       }
