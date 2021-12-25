module Model.Asset where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Config (BatmanConfig)
import qualified Config
import Network.HTTP.Req
import Data.Text (pack)


data Candle = Candle { high  :: Double
                     , low   :: Double
                     , open  :: Double
                     , close :: Double } deriving (Generic, FromJSON, Show)





data Asset = Asset { candlesticks :: [Candle]
                   , macd         :: [Double]
                   , signal       :: [Double]
                   , rsi          :: [Double] } deriving (Generic, FromJSON, Show)



