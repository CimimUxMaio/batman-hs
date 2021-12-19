module Model.Analysis.Candlesticks.HelpersSpec where

import Test.Hspec (Spec, describe, it, shouldNotSatisfy, shouldSatisfy)
import TestUtils ( bullishTrend, bearishTrend )
import Model.Analysis.Candlesticks (downwardTrend, upwardTrend)


spec :: Spec
spec = do
    describe "helpers" $ do
        describe "trends" $ do
            describe "downwardTrend" $ do
                it "is true for a bearish list of candlesticks data" $ do
                    bearishTrend `shouldSatisfy` downwardTrend

                it "is false for a bullish list of candlesticks data" $ do
                    bullishTrend `shouldNotSatisfy` downwardTrend

            describe "upwardTrend" $ do
                it "is true for a bullish list of candlesticks data" $ do
                    bullishTrend `shouldSatisfy` upwardTrend

                it "is false for a bearish list of candlesticks data" $ do
                    bearishTrend `shouldNotSatisfy` upwardTrend