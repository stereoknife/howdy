module Howdy.Secrets where
import           Control.Applicative (Alternative (empty), (<|>))
import qualified Data.ByteString     as BS
import           Data.List           (foldl')
import           Data.Text           (Text, pack)
import           Data.Text.Encoding  (decodeUtf8)
import           System.Environment  (getEnv)

defaultTokenEnv :: String
defaultTokenEnv = "DISCORD_TOKEN"

defaultTokenPath :: String
defaultTokenPath = "./token.nightly.secret"

fromList :: [String] -> IO Text
fromList = foldl' (\acc x -> acc <|> fromAny x) empty

fromAny :: String -> IO Text
fromAny x = fromEnv x <|> fromFile x

fromDef :: IO Text
fromDef = fromEnv defaultTokenEnv <|> fromFile defaultTokenPath

fromFile :: String -> IO Text
fromFile = fmap decodeUtf8 . BS.readFile

fromEnv :: String -> IO Text
fromEnv = fmap pack . getEnv
