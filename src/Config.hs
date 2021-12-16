module Config where

import Data.Aeson ( FromJSON, eitherDecodeFileStrict )
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text (Text)
import Data.Set (Set)


newtype ServerConfig = ServerConfig { port :: Int } deriving (Generic, FromJSON, Show)

data TelegramConfig = TelegramConfig { token :: String
                                     , apiUrl :: Text } deriving (Generic, FromJSON, Show)

data BatmanConfig = BatmanConfig { cronExp    :: Text
                                 , cryptos    :: [String]
                                 , dataApiUrl :: Text } deriving (Generic, FromJSON, Show)

data Config = Config { telegram :: TelegramConfig
                     , server :: ServerConfig
                     , batman :: BatmanConfig } deriving (Generic, FromJSON, Show)



getConfig :: IO (Either String Config)
getConfig = eitherDecodeFileStrict "./config.json"


getConfig' :: IO Config
getConfig' = do
    config <- getConfig
    case config of
        Left err  -> error err
        Right cfg -> pure cfg