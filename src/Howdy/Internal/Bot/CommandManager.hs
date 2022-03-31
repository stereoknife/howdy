{-# LANGUAGE FlexibleContexts #-}

module Howdy.Internal.Bot.CommandManager where

import Discord.Types
    ( Event(MessageCreate),
      Message(messageContent, messageChannelId, messageGuildId,
              messageAuthor),
      ChannelId,
      GuildId,
      UserId,
      User(userId) )
import Howdy.Error ( HowdyException, MonadError )
import Howdy.Internal.Action.Command ( CommandRunner )
import Data.Text ( Text )
import Howdy.Parser ( string, word, firstof, parse )
import Discord ( DiscordHandler )
import Data.Hashable (hash)

data BotPreferences = BotPreferences { prefixes :: [Text]
                                     , yeehaw :: Bool
                                     }


data CommandInput = CommandInput { target  :: Int
                                 , user    :: UserId
                                 , guild   :: Maybe GuildId
                                 , channel :: ChannelId
                                 , args    :: Text
                                 }

permit :: MonadError HowdyException m => CommandRunner -> CommandInput -> m ()
permit = undefined

-- prcess command
-- from messagecreate to discordhandler

-- checkForPrefix

fetchCommand :: MonadError HowdyException m => Int -> m CommandRunner
fetchCommand = undefined

extractData :: (MonadError HowdyException m) => Message -> Text -> m CommandInput
extractData m t = do
                  (target, args) <- parse word t
                  let user = m.messageAuthor.userId
                      guild = m.messageGuildId
                      channel = m.messageChannelId
                  pure $ CommandInput (hash target) user guild channel args

eventHandler :: Event -> Either HowdyException (DiscordHandler ())
eventHandler (MessageCreate m) = do
    (_, text) <- parse (firstof string ["prefix", "es"]) m.messageContent
    ci <- extractData m text
    cr <- fetchCommand ci.target
    permit cr ci
    -- run the thing
    pure(pure ())

responseHandler :: Either HowdyException (DiscordHandler ()) -> DiscordHandler () 
responseHandler (Right dh) = dh
responseHandler (Left e) =
    case e of _ -> pure ()