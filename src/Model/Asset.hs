module Model.Asset where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Config (BatmanConfig)
import qualified Config
import Network.HTTP.Req (req, GET (GET), http, NoReqBody (NoReqBody), runReq, jsonResponse, responseBody, defaultHttpConfig, (/:), useHttpURI, port)
import Data.Text (pack)


data Candle = Candle { high  :: Double
                     , low   :: Double
                     , open  :: Double
                     , close :: Double } deriving (Generic, FromJSON, Show)


data Asset = Asset { candlesticks :: [Candle]
                   , macd         :: [Double]
                   , signal       :: [Double]
                   , rsi          :: [Double] } deriving (Generic, FromJSON, Show)



getAsset :: BatmanConfig -> String -> IO (String, Asset)
getAsset config symbol = runReq defaultHttpConfig $ do
        r <- req GET url NoReqBody jsonResponse (port 8000)
        pure (symbol, responseBody r :: Asset)

    where apiUrl = Config.dataApiUrl config
          url = http apiUrl /: "binance" /: pack symbol