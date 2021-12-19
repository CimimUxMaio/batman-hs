module Model.Analysis.CandlesticksAnalysisSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import TestUtils (buildCandles, bearishTrend, nonHammer1, redHammer, nonHammer2, bullishTrend)
import qualified Model.Analysis.Analysis as Analysis
import Model.Asset (Asset(Asset, macd, signal, rsi, candlesticks), Candle)
import qualified Model.Asset as Asset
import qualified Model.Analysis.Analysis as Asset
import Model.Analysis.Analysis (AnalysisResult(suggestion))
import Model.Analysis.Suggestion (Suggestion(BUY, SELL, HODL))



spec :: Spec
spec = do
    describe "candlesticks analysis" $ do
        it "returns a BUY suggestion if a bullish pattern is matched" $ do
            let candles = buildCandles bearishTrend redHammer ++ nonHammer1 0
            let result = Analysis.candlesticks . candlesToAsset $ candles
            suggestion result `shouldBe` BUY

        it "returns a SELL suggestion if a bullish pattern is matched" $ do
            let candles = buildCandles bullishTrend redHammer ++ nonHammer2 0
            let result = Analysis.candlesticks . candlesToAsset $ candles
            suggestion result `shouldBe` SELL
        
        it "returns a HODL suggestion if no pattern is matched" $ do
            let candles = buildCandles bullishTrend nonHammer2 ++ nonHammer1 0
            let result = Analysis.candlesticks . candlesToAsset $ candles
            suggestion result `shouldBe` HODL



candlesToAsset :: [Candle] -> Asset
candlesToAsset candles = Asset { candlesticks = candles, macd = [], signal = [], rsi = [] }