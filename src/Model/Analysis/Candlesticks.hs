module Model.Analysis.Candlesticks where

import Model.Asset (Candle, close, open, high, low)
import Model.Analysis.Suggestion ( Suggestion(SELL, BUY) )
import Model.Utils ( average, current )
import Data.List.Extra (takeEnd)


data CandlesticksPattern = CandlesticksPattern { description :: String
                                               , patternSize :: Int
                                               , match       :: [Candle] -> Bool
                                               , suggestion  :: Suggestion }


patterns :: [CandlesticksPattern]
patterns = [ bullishHammer
           , bullishInvertedHammer
           , hangingMan
           , shootingStar ]


bullishHammer :: CandlesticksPattern
bullishHammer = CandlesticksPattern { description = "bullish hammer"
                                    , patternSize = 1
                                    , match = match
                                    , suggestion = BUY }
    where match candles = downwardTrend candles && isStraightHammer (current candles)


bullishInvertedHammer :: CandlesticksPattern
bullishInvertedHammer = CandlesticksPattern { description = "bullish inverted hammer"
                                            , patternSize = 1
                                            , match = match
                                            , suggestion = BUY }
    where match candles = downwardTrend candles && isInvertedHammer (current candles)


hangingMan :: CandlesticksPattern
hangingMan = CandlesticksPattern { description = "hanging man"
                                 , patternSize = 1
                                 , match = match
                                 , suggestion = SELL }
    where match candles = upwardTrend candles && isStraightHammer (current candles)


shootingStar :: CandlesticksPattern
shootingStar = CandlesticksPattern { description = "shooting star"
                                   , patternSize = 1
                                   , match = match
                                   , suggestion = SELL }
    where match candles = upwardTrend candles && isInvertedHammer (current candles)




type TrendCondition = [Candle] -> Bool

trend :: (Double -> Double -> Bool) -> TrendCondition
trend glComparator candles = length candles >= trendSize && (averageGain sample `glComparator` averageLoss sample)
    where trendSize = 4
          sample = takeEnd trendSize candles


upwardTrend :: TrendCondition
upwardTrend = trend (>)


downwardTrend :: TrendCondition
downwardTrend = trend (<)



isHammer :: (Candle -> Double) -> Candle -> Bool
isHammer longWickCalculator candle =
    longWick >= lwK * candleHeight
    && smallWick < candleBody
    && candleBody >= bK * candleHeight

    where longWick = longWickCalculator candle
          smallWick = height candle - longWick - body candle
          candleBody = body candle
          candleHeight = height candle
          lwK = 0.45
          bK = 0.2

isStraightHammer :: Candle -> Bool
isStraightHammer = isHammer (\c -> min (close c - low c) (open c - low c))

isInvertedHammer :: Candle -> Bool
isInvertedHammer = isHammer (\c -> min (high c - open c) (high c - close c))



gains :: [Candle] -> [Double]
gains = filter (> 0) . map gain


losses :: [Candle] -> [Double]
losses = filter (< 0) . map gain


averageGain :: [Candle] -> Double
averageGain = average . gains


averageLoss :: [Candle] -> Double
averageLoss = average . losses


height :: Candle -> Double
height candle = high candle - low candle


gain :: Candle -> Double
gain candle = close candle - open candle


body :: Candle -> Double
body = abs . gain