{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Discord.Application.Command where

-- change
type Snowflake = String

data Either3 a b c = First a | Second b | Third c

data Type = C_CHAT_INPUT | C_USER | C_MESSAGE
    deriving (Show, Eq, Ord, Enum)

data Object 
    = Object 
        { id :: Snowflake
        , c_type :: Maybe Type
        , application_id :: Snowflake
        , guild_id :: Maybe Snowflake
        , name :: String
        , description :: String
        , options :: Maybe [Option]
        , default_permission :: Bool
        , version :: Snowflake
        }

data OptionType
    = O_SUB_COMMAND
    | O_SUB_COMMAND_GROUP
    | O_STRING
    | O_INTEGER
    | O_BOOLEAN
    | O_USER
    | O_CHANNEL
    | O_ROLE
    | O_MENTIONABLE
    | O_NUMBER
    deriving (Show, Eq, Ord, Enum)

data ChannelType
    = GUILD_TEXT
    | DM
    | GUILD_VOICE
    | GROUP_DM
    | GUILD_CATEGORY
    | GUILD_NEWS
    | GUILD_STORE
    | GUILD_NEWS_THREAD
    | GUILD_PUBLIC_THREAD
    | GUILD_PRIVATE_THREAD
    | GUILD_STAGE_VOICE

data Option
    = Option
        { o_type :: OptionType
        , name :: String
        , description :: String
        , required :: Maybe Bool
        , choices :: Maybe [OptionChoice]
        , options :: Maybe [Option]
        , channel_types :: Maybe [ChannelType]
        , min_value :: Maybe (Either Integer Double)
        , max_value :: Maybe (Either Integer Double)
        , autocomplete :: Maybe Bool
        }

data OptionChoice
    = OptionChoice
        { name :: String
        , value :: Either3 String Integer Double
        }

data InteractionDataOption
    = InteractionDataOption
        { name :: String
        , type' :: OptionType
        , value :: Maybe (Either3 String Integer Double)
        , options :: Maybe [InteractionDataOption]
        , focused :: Maybe Bool
        }