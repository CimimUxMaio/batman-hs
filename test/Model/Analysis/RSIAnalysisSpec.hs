module Model.Analysis.RSIAnalysisSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Model.Asset (Asset(Asset, rsi, candlesticks, macd, signal))
import qualified Model.Analysis.Analysis as Analysis
import Model.Analysis.Suggestion (Suggestion(SELL, BUY, HODL))
import Model.Analysis.Analysis (AnalysisResult(suggestion), Analysis)



spec :: Spec
spec = do
    describe "RSI analysis" $ do
        it "returns a SELL suggestion if the RSI curve crosses the upper bound line downwards" $ do
            let asset = rsiToAsset [72, 68]
            let result = analysis asset
            suggestion result `shouldBe` SELL

        it "returns a SELL suggestion if the previous RSI was in exactly the upper bound and the current is lower" $ do
            let asset = rsiToAsset [70, 68]
            let result = analysis asset
            suggestion result `shouldBe` SELL

        it "returns a BUY suggestion if the RSI curve crosses the lower bound line upwards" $ do
            let asset = rsiToAsset [28, 31]
            let result = analysis asset
            suggestion result `shouldBe` BUY

        it "returns a BUY suggestion if the previous RSI was in exactly the lower bound and the current is higher" $ do
            let asset = rsiToAsset [30, 32]
            let result = analysis asset
            suggestion result `shouldBe` BUY

        it "returns a HODL suggestion if the RSI curve remains between the lower and upper bounds" $ do
            let asset = rsiToAsset [55, 58]
            let result = analysis asset
            suggestion result `shouldBe` HODL 

        it "returns a HODL suggestion if the RSI curve remains below the lower bound" $ do
            let asset = rsiToAsset [25, 27]
            let result = analysis asset
            suggestion result `shouldBe` HODL 

        it "returns a HODL suggestion if the RSI curve remains over the upper bound" $ do
            let asset = rsiToAsset [72, 73]
            let result = analysis asset
            suggestion result `shouldBe` HODL

        it "returns a HODL suggestion if the RSI curve crosses the upper bound line upwards" $ do
            let asset = rsiToAsset [69, 72]
            let result = analysis asset
            suggestion result `shouldBe` HODL

        it "returns a HODL suggestion if the RSI curve crosses the lower bound line downwards" $ do
            let asset = rsiToAsset [31, 29]
            let result = analysis asset
            suggestion result `shouldBe` HODL


rsiToAsset :: [Double] -> Asset
rsiToAsset rsi = Asset { rsi = rsi, candlesticks = [], macd = [], signal = [] }


analysis :: Analysis.Analysis 
analysis = Analysis.rsi (30, 70)