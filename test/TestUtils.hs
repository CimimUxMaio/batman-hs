module TestUtils where

import Model.Asset ( Candle(..) )

bullishTrend :: [Candle]
bullishTrend = trend (100.0 *)


bearishTrend :: [Candle]
bearishTrend = trend (\i -> 100.0 - 10.0 * i)


trend :: (Double -> Double) -> [Candle]
trend closeFromIndex = map aux [1..10]
    where aux i = Candle { open = if i == 0 then i else closeFromIndex (i - 1)
                         , close = closeFromIndex i
                         , high = 0.0
                         , low = 0.0 }



redHammer :: Candle
redHammer = Candle { high = 50567.91
                   , low = 46930.0
                   , open = 50047.84
                   , close = 49066.77 }


greenHammer :: Candle 
greenHammer = Candle { high = 57076.24
                     , low = 53329.96
                     , open = 55633.14
                     , close = 56425.0 }


nonHammer1 :: Candle 
nonHammer1 = Candle { high = 54356.62
                    , low = 48753.44
                    , open = 49066.76
                    , close = 54001.39 }


nonHammer2 :: Candle 
nonHammer2 = Candle { high = 40018.49
                    , low = 48753.44
                    , open = 49066.76
                    , close = 54001.39 }



nonHammer3 :: Candle   -- Hammer but with a very thin body
nonHammer3 = Candle { high = 100.0
                    , low = 0.0
                    , open = 90.0
                    , close = 80.0 }


nonHammer4 :: Candle -- Hammer but with a very wide body
nonHammer4 = Candle { high = 100.0
                    , low = 0.0
                    , open = 90.0
                    , close = 40.0 }


greenInvertedHammer :: Candle 
greenInvertedHammer = Candle { high = 5940.0
                             , low = 5093.1
                             , open = 5172.48
                             , close = 5361.3 }


redInvertedHammer :: Candle
redInvertedHammer = Candle { high = 200.0
                           , low = 0.0
                           , open = 80.0
                           , close = 0.0 }