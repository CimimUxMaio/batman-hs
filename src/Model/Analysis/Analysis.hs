module Model.Analysis.Analysis where

import Model.Asset as Asset
import Model.Utils ( current, previous )
import Data.Foldable.Extra (find)
import Model.Analysis.Candlesticks (patterns, CandlesticksPattern (match, patternSize, name)) 
import qualified Model.Analysis.Candlesticks as Candlesticks
import Model.Analysis.Suggestion (Suggestion (SELL, HODL, BUY))
import Data.List.Extra (sortOn)


data AnalysisResult = AnalysisResult { analysis   :: String
                                     , reason     :: String 
                                     , suggestion :: Suggestion } deriving Show

type Analysis = Asset -> AnalysisResult


base :: String -> (Asset -> Bool) -> (Asset -> Bool) -> Analysis
base analysisName overbought oversold asset
    | overbought asset = result SELL "overbought"
    | oversold asset   = result BUY "oversold"
    | otherwise        = result HODL ""
    
    where result suggestion reason = AnalysisResult { analysis = analysisName
                                                    , reason = reason
                                                    , suggestion = suggestion }


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
candlesticks asset = maybe defaultResult toResult firstMatch
    where candles = init . Asset.candlesticks $ asset -- Take all candles except the last one (unconfirmed)
          orderedPatterns = sortOn patternSize patterns 
          firstMatch = find (`match` candles) orderedPatterns
          defaultResult = result HODL "no match"
          toResult pattern = result (Candlesticks.suggestion pattern) (name pattern ++ " pattern")
          result suggestion reason = AnalysisResult { analysis = "candlesticks" 
                                                    , reason = reason
                                                    , suggestion = suggestion }
        