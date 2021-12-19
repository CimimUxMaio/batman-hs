module Model.Analysis.Candlesticks where

import Model.Asset (Candle, close, open, high, low)
import Model.Analysis.Suggestion ( Suggestion(SELL, BUY) )
import Model.Utils ( average, previous )
import Data.List.Extra (takeEnd)


data CandlesticksPattern = CandlesticksPattern { name :: String
                                               , patternSize :: Int
                                               , match       :: [Candle] -> Bool
                                               , suggestion  :: Suggestion }


patterns :: [CandlesticksPattern]
patterns = [ bullishHammer
           , bullishInvertedHammer
           , hangingMan
           , shootingStar ]


bullishHammer :: CandlesticksPattern
bullishHammer = CandlesticksPattern { name = "bullish hammer"
                                    , patternSize = 1
                                    , match = match
                                    , suggestion = BUY }
    where match candles = downwardTrend candles && isStraightHammer (last candles)


bullishInvertedHammer :: CandlesticksPattern
bullishInvertedHammer = CandlesticksPattern { name = "bullish inverted hammer"
                                            , patternSize = 1
                                            , match = match
                                            , suggestion = BUY }
    where match candles = downwardTrend candles && isInvertedHammer (last candles)


hangingMan :: CandlesticksPattern
hangingMan = CandlesticksPattern { name = "hanging man"
                                 , patternSize = 1
                                 , match = match
                                 , suggestion = SELL }
    where match candles = upwardTrend candles && isStraightHammer (last candles)


shootingStar :: CandlesticksPattern
shootingStar = CandlesticksPattern { name = "shooting star"
                                   , patternSize = 1
                                   , match = match
                                   , suggestion = SELL }
    where match candles = upwardTrend candles && isInvertedHammer (last candles)




type TrendCondition = [Candle] -> Bool

trend :: (Double -> Double -> Bool) -> TrendCondition
trend glComparator candles = length candles >= trendSize && (totalGain sample `glComparator` totalLoss sample)
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


totalGain :: [Candle] -> Double
totalGain = sum . gains


totalLoss :: [Candle] -> Double
totalLoss = abs . sum .losses


height :: Candle -> Double
height candle = high candle - low candle


gain :: Candle -> Double
gain candle = close candle - open candle


body :: Candle -> Double
body = abs . gain