{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Howdy.Internal.Manager.Command where

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad (unless)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (runReaderT), runReader)
import Data.HashMap.Lazy (empty, (!?))
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import Discord (DiscordHandler)
import Discord.Types (Message (..), MessageReference (MessageReference), User (userId), ChannelId, UserId)
import Howdy.Comptime.Bot (BotDefinition (..))
import Howdy.Comptime.Command (CommandDefinition (..), CommandInput (CommandInput, target),
                               CommandReplyData (..))
import Howdy.Internal.Discord (request, unHowdy)
import Howdy.Internal.Error (HowdyException (CommandNotFound, DiscordError, ForbiddenCommand, Ignore, UnknownIdentifier),
                             report)
import Howdy.Internal.Parser.Class (parse, parseWithError)
import Howdy.Internal.Parser.Cons (firstof, flag, string, word)
import Control.Optics (Lensed (focus))
import Lens.Micro (to)

(<<) :: c -> String -> c
(<<) = flip trace

noop :: Applicative a => a ()
noop = pure ()

commandHandler :: Message -> BotDefinition -> DiscordHandler ()
commandHandler m b = do
    -- Match and discard prefix
    (_, rest) <- parseWithError Ignore (firstof string b.bdPrefixes) m.messageContent

    -- Match and set Debug flag
    (debug, rest') <- parse (flag "#debug") rest

    -- Build command input
    ci <- trace "Processing input"
        $ buildInput m rest'
    trace ("Command input: " ++ show ci) noop

    -- Get Command
    cr <- trace "Fetching command"
        $ fetchCommand b ci.target
    trace ("Found command " ++ show ci.target) noop

    -- Check permission
    trace "Checking permission"
        $ permit cr ci

    let crd = CommandReplyData m.messageChannelId m.messageAuthor.userId
            $ MessageReference (Just m.messageId) Nothing Nothing True
    let runner = cdRunner cr ci

    -- Run command
    trace "Running command"
        $ runReaderT (unHowdy runner) crd


type HowdyDebug = ()

debugHandler :: Bool -> HowdyDebug -> DiscordHandler ()
debugHandler = undefined


fetchCommand :: MonadThrow m => BotDefinition -> Text -> m CommandDefinition
fetchCommand b a = do
    let aliases = b.bdAliases
        coms    = b.bdCommands
    id <- report UnknownIdentifier $ aliases !? a
    trace ("Identified command " ++ show id) noop
    report CommandNotFound $ coms !? id


buildInput :: MonadThrow m => Message -> Text -> m CommandInput
buildInput m t = do
    (target, args) <- parse word t
    let user = m.messageAuthor
        guild = m.messageGuildId
        channel = m.messageChannelId
        ref = m.messageReference
    pure $ CommandInput target user guild channel args ref

permit :: MonadThrow m => CommandDefinition -> CommandInput -> m ()
permit p i = unless (p.cdPermission i) $ throwM ForbiddenCommand

doNothing :: DiscordHandler ()
doNothing = pure ()
