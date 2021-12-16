module Config where

import Data.Aeson ( FromJSON, eitherDecodeFileStrict )
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 (pack)


newtype ServerConfig = ServerConfig { port :: Int } deriving (Generic, Show)

data TelegramConfig = TelegramConfig { token :: String
                                     , apiUrl :: String } deriving (Generic, Show)

data Config = Config { telegram :: TelegramConfig
                     , server :: ServerConfig } deriving (Generic, Show)

instance FromJSON ServerConfig
instance FromJSON TelegramConfig
instance FromJSON Config


getConfig :: IO (Either String Config)
getConfig = eitherDecodeFileStrict "./config.json"


getConfig' :: IO Config
getConfig' = do
    config <- getConfig
    case config of
        Left err  -> error err
        Right cfg -> pure cfg
