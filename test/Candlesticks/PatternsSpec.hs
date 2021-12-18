module Candlesticks.PatternsSpec where
import Test.Hspec (Spec, describe, it, shouldSatisfy, shouldNotSatisfy)
import TestUtils
import Model.Analysis.Candlesticks
    ( bullishHammer, CandlesticksPattern(match) )


spec :: Spec
spec = do
    describe "patterns" $ do
        describe "bullishHammer" $ do
            it "matches with a red hammer candle" $ do
                let candles = buildCandles bearishTrend redHammer nonHammer1
                candles `shouldSatisfy` match bullishHammer

            it "matches with a green hammer candle" $ do
                let candles = buildCandles bearishTrend greenHammer nonHammer2
                candles `shouldSatisfy` match bullishHammer

            it "does not match with an inverted hammer candle" $ do
                let candles = buildCandles bearishTrend redInvertedHammer nonHammer1
                candles `shouldNotSatisfy` match bullishHammer

            it "does not match with a non hammer-like candle 1" $ do
                let candles = buildCandles bearishTrend nonHammer1 nonHammer2
                candles `shouldNotSatisfy` match bullishHammer

            it "does not match with a non hammer-like candle 2" $ do
                let candles = buildCandles bearishTrend nonHammer2 nonHammer2
                candles `shouldNotSatisfy` match bullishHammer

            it "does not match with a bullish trend" $ do
                let candles = buildCandles bullishTrend redHammer nonHammer1
                candles `shouldNotSatisfy` match bullishHammer