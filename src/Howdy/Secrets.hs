module Howdy.Secrets where
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS
import System.Environment (getEnv)
import Control.Applicative ((<|>), Alternative (empty))
import Data.List (foldl')

defaultTokenEnv :: String
defaultTokenEnv = ""

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