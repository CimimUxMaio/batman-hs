module CandlesticksSpec where

import Test.Hspec.Runner
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import TestUtils
import Model.Analysis.Candlesticks (CandlesticksPattern(match), bullishHammer, downwardTrend, upwardTrend)



spec :: Spec
spec = do
    describe "Model.Analysis.Candlesticks" $ do
        describe "trends" $ do
            describe "downwardTrend" $ do
                it "is true for a bearish list of candlesticks data" $ do
                    bearishTrend `shouldSatisfy` downwardTrend

                it "is false for a bullish list fo candlesticks data" $ do
                    bullishTrend `shouldSatisfy` downwardTrend

            describe "upwardTrend" $ do
                it "is true for a bullish list of candlesticks data" $ do
                    bullishTrend `shouldSatisfy` upwardTrend

                it "is false for a bearish list fo candlesticks data" $ do
                    bearishTrend `shouldSatisfy` upwardTrend

        describe "bullishHammer" $ do
            it "matches with a red hammer candle" $ do
                let candles = bearishTrend ++ [redHammer, nonHammer1]
                candles `shouldSatisfy` match bullishHammer

            it "matches with a green hammer candle" $ do
                let candles = bearishTrend ++ [greenHammer, nonHammer2]
                candles `shouldSatisfy` match bullishHammer

            it "does not match with an inverted hammer candle" $ do
                let candles = bearishTrend ++ [redInvertedHammer, nonHammer1]
                candles `shouldSatisfy` match bullishHammer

            it "does not match with a non hammer-like candle 1" $ do
                let candles = bearishTrend ++ [nonHammer3, nonHammer4]
                candles `shouldSatisfy` match bullishHammer

            it "does not match with a non hammer-like candle 2" $ do
                let candles = bearishTrend ++ [nonHammer4, nonHammer3]
                candles `shouldSatisfy` match bullishHammer

            it "does not match with a bullish trend" $ do
                let candles = bullishTrend ++ [redHammer, nonHammer1]
                candles `shouldSatisfy` match bullishHammer