module Model.Analysis.Candlesticks.Patterns.HammerSpec where

import Test.Hspec
    ( describe, it, shouldNotSatisfy, shouldSatisfy, Spec )
import TestUtils
    ( bullishTrend,
      bearishTrend,
      redHammer,
      greenHammer,
      nonHammer1,
      nonHammer2,
      greenInvertedHammer,
      redInvertedHammer,
      buildCandles )
import Model.Analysis.Candlesticks
    ( bullishHammer,
      bullishInvertedHammer,
      hangingMan,
      shootingStar,
      CandlesticksPattern(match) )


spec :: Spec
spec = do
    describe "hammer patterns" $ do

        describe "bullishHammer" $ do
            it "matches with a red hammer pattern" $ do
                let candles = buildCandles bearishTrend redHammer
                candles `shouldSatisfy` match bullishHammer

            it "matches with a green hammer pattern" $ do
                let candles = buildCandles bearishTrend greenHammer
                candles `shouldSatisfy` match bullishHammer

            it "does not match with an inverted hammer pattern" $ do
                let candles = buildCandles bearishTrend redInvertedHammer
                candles `shouldNotSatisfy` match bullishHammer

            it "does not match with a non hammer-like pattern 1" $ do
                let candles = buildCandles bearishTrend nonHammer1
                candles `shouldNotSatisfy` match bullishHammer

            it "does not match with a non hammer-like pattern 2" $ do
                let candles = buildCandles bearishTrend nonHammer2
                candles `shouldNotSatisfy` match bullishHammer

            it "does not match with a bullish trend" $ do
                let candles = buildCandles bullishTrend redHammer
                candles `shouldNotSatisfy` match bullishHammer


        describe "hangingMan" $ do
            it "matches with a red hammer pattern" $ do
                let candles = buildCandles bullishTrend redHammer
                candles `shouldSatisfy` match hangingMan 

            it "matches with a green hammer pattern" $ do
                let candles = buildCandles bullishTrend greenHammer
                candles `shouldSatisfy` match hangingMan 

            it "does not match with an inverted hammer pattern" $ do
                let candles = buildCandles bullishTrend redInvertedHammer
                candles `shouldNotSatisfy` match hangingMan 

            it "does not match with a non hammer-like pattern 1" $ do
                let candles = buildCandles bullishTrend nonHammer1
                candles `shouldNotSatisfy` match hangingMan

            it "does not match with a non hammer-like pattern 2" $ do
                let candles = buildCandles bullishTrend nonHammer2
                candles `shouldNotSatisfy` match hangingMan 

            it "does not match with a bearish trend" $ do
                let candles = buildCandles bearishTrend redHammer
                candles `shouldNotSatisfy` match hangingMan 


        describe "bullishInvertedHammer" $ do
            it "matches with a red inverted hammer pattern" $ do
                let candles = buildCandles bearishTrend redInvertedHammer
                candles `shouldSatisfy` match bullishInvertedHammer 
                
            it "matches with a green inverted hammer pattern" $ do
                let candles = buildCandles bearishTrend greenInvertedHammer
                candles `shouldSatisfy` match bullishInvertedHammer 

            it "does not match with a straight hammer pattern" $ do
                let candles = buildCandles bearishTrend redHammer
                candles `shouldNotSatisfy` match bullishInvertedHammer

            it "does not match with a non hammer-like pattern 1" $ do
                let candles = buildCandles bearishTrend nonHammer2
                candles `shouldNotSatisfy` match bullishInvertedHammer 

            it "does not match with a non hammer-like pattern 2" $ do
                let candles = buildCandles bearishTrend nonHammer1
                candles `shouldNotSatisfy` match bullishInvertedHammer

            it "does not match with a bullish trend" $ do
                let candles = buildCandles bullishTrend redInvertedHammer
                candles `shouldNotSatisfy` match bullishInvertedHammer


        describe "shootingStar" $ do
            it "matches with a red inverted hammer pattern" $ do
                let candles = buildCandles bullishTrend redInvertedHammer
                candles `shouldSatisfy` match shootingStar   
                
            it "matches with a green inverted hammer pattern" $ do
                let candles = buildCandles bullishTrend greenInvertedHammer
                candles `shouldSatisfy` match shootingStar  

            it "does not match with a straight hammer pattern" $ do
                let candles = buildCandles bullishTrend redHammer
                candles `shouldNotSatisfy` match shootingStar 

            it "does not match with a non hammer-like pattern 1" $ do
                let candles = buildCandles bullishTrend nonHammer2
                candles `shouldNotSatisfy` match shootingStar  

            it "does not match with a non hammer-like pattern 2" $ do
                let candles = buildCandles bullishTrend nonHammer1
                candles `shouldNotSatisfy` match shootingStar 

            it "does not match with a bearish trend" $ do
                let candles = buildCandles bearishTrend redInvertedHammer
                candles `shouldNotSatisfy` match shootingStar 