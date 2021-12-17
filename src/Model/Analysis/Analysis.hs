module Model.Analysis.Analysis where

import Model.Asset as Asset
import Model.Utils ( current, previous )
import Data.Foldable.Extra (find)
import Model.Analysis.Candlesticks (match, suggestion, patterns, CandlesticksPattern (patternSize))
import Model.Analysis.Suggestion (Suggestion (SELL, HODL, BUY))
import Data.List.Extra (sortOn)


type AnalysisResult = Suggestion
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


candlesticks :: Analysis
candlesticks asset = maybe HODL suggestion (find (`match` candles) patterns)
    where candles = init . Asset.candlesticks $ asset -- Take all candles except the last one (unconfirmed)
          orderedPatterns = sortOn patternSize patterns 