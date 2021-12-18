module TestUtils where

import Model.Asset ( Candle(..) )


trendSize :: Num a => a
trendSize = 10


bullishTrend :: [Candle]
bullishTrend = trend nextIncreasing

nextIncreasing :: Double -> Double
nextIncreasing = (100.0 *)


bearishTrend :: [Candle]
bearishTrend = trend nextDecreasing

nextDecreasing :: Double -> Double
nextDecreasing i = 1000.0 - 100.0 * i


trend :: (Double -> Double) -> [Candle]
trend next = map aux [1..trendSize]
    where aux i = Candle { open = next (i - 1)
                         , close = next i
                         , high = 0.0
                         , low = 0.0 }


type PatternBuilder = Double -> [Candle]


buildCandles :: [Candle] -> PatternBuilder -> PatternBuilder -> [Candle]
buildCandles trend patternBuilder currentBuilder = trend ++ p ++ currentCandle
    where lastClose = close . last
          p = patternBuilder (lastClose trend)
          currentCandle = currentBuilder (lastClose p)


redHammer :: PatternBuilder   -- body 30%
redHammer open = [Candle { high = open + 20
                         , low = open - 80
                         , open = open
                         , close = open - 30 }]


greenHammer :: PatternBuilder
greenHammer open = [Candle { high = open + 50
                           , low = open - 50
                           , open = open
                           , close = open + 40 }]


nonHammer1 :: PatternBuilder   -- Hammer but with a very thin body
nonHammer1 open = [Candle { high = open + 10
                          , low = open - 90
                          , open = open
                          , close = open - 10 }]


nonHammer2 :: PatternBuilder  -- Hammer but with a very wide body
nonHammer2 open = [Candle { high = open + 10
                          , low = open - 90
                          , open = open
                          , close = open - 80 }]


greenInvertedHammer :: PatternBuilder
greenInvertedHammer open = [Candle { high = open + 90
                                   , low = open - 10
                                   , open = open
                                   , close = open + 35 }]


redInvertedHammer :: PatternBuilder
redInvertedHammer open = [Candle { high = open + 40
                                 , low = open - 60
                                 , open = open
                                 , close = open - 40 }]