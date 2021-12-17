module Model.Analysis.Analysis where

import Model.Asset as Asset
import Model.Utils ( current, previous )
import Data.Foldable.Extra (find)
import Model.Analysis.Candlesticks (match, suggestion, patterns, CandlesticksPattern (patternSize, description))
import Model.Analysis.Suggestion (Suggestion (SELL, HODL, BUY))
import Data.List.Extra (sortOn)


data AnalysisResult = AnalysisResult Suggestion String deriving Show

type Analysis = Asset -> AnalysisResult


base :: String -> (Asset -> Bool) -> (Asset -> Bool) -> Analysis
base description overbought oversold asset
    | overbought asset = result SELL
    | oversold asset   = result BUY 
    | otherwise        = result HODL
    
    where result s = AnalysisResult s description


rsi :: Analysis
rsi = base "RSI" overbought oversold
    where overbought asset = previousRSI asset >= 70 && currentRSI asset < 70
          oversold asset   = previousRSI asset <= 30 && currentRSI asset > 30
          currentRSI  = current . Asset.rsi
          previousRSI = previous . Asset.rsi


macd :: Analysis
macd = base "MACD" overbought oversold
    where overbought asset = previousDelta asset >= 0 && currentDelta asset < 0
          oversold asset = previousDelta asset <= 0 && currentDelta asset > 0
          currentSignal = current . Asset.signal
          previousSignal = previous . Asset.signal
          currentMACD = current . Asset.macd
          previousMACD = previous . Asset.macd
          currentDelta asset = currentMACD asset - currentSignal asset
          previousDelta asset = previousMACD asset - previousSignal asset


candlesticks :: Analysis
candlesticks asset = maybe defaultResult result firstMatch
    where candles = init . Asset.candlesticks $ asset -- Take all candles except the last one (unconfirmed)
          orderedPatterns = sortOn patternSize patterns 
          firstMatch = find (`match` candles) orderedPatterns
          defaultResult = AnalysisResult HODL "candlesticks"
          result pattern = AnalysisResult (suggestion pattern) (description pattern)