module Model.Analysis.Analysis where

import Model.Asset as Asset
import Data.List.Extra (takeEnd)

data AnalysisResult = BUY | SELL | HODL deriving Show

type Analysis = Asset -> AnalysisResult


base :: (Asset -> Bool) -> (Asset -> Bool) -> Analysis
base overbought oversold asset
    | overbought asset = SELL
    | oversold asset   = BUY
    | otherwise        = HODL


rsi :: Analysis
rsi = base overbought oversold
    where overbought asset = previousRSI asset >= 70 && currentRSI asset < 70
          oversold asset   = previousRSI asset <= 30 && currentRSI asset > 30
          currentRSI  = current . Asset.rsi
          previousRSI = previous . Asset.rsi


macd :: Analysis
macd = base overbought oversold
    where overbought asset = previousDelta asset >= 0 && currentDelta asset < 0
          oversold asset = previousDelta asset <= 0 && currentDelta asset > 0
          currentSignal = current . Asset.signal
          previousSignal = previous . Asset.signal
          currentMACD = current . Asset.macd
          previousMACD = previous . Asset.macd
          currentDelta asset = currentMACD asset - currentSignal asset
          previousDelta asset = previousMACD asset - previousSignal asset



indexEnd :: Int -> [a] -> a
indexEnd i = head . takeEnd i

current :: [a] -> a
current = indexEnd 1

previous :: [a] -> a
previous = indexEnd 2