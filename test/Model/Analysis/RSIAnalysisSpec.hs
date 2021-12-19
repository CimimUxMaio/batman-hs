module Model.Analysis.RSIAnalysisSpec where
import Test.Hspec (Spec, describe, it, shouldBe)
import Model.Asset (Asset(Asset, rsi, candlesticks, macd, signal))
import qualified Model.Analysis.Analysis as Analysis
import Model.Analysis.Suggestion (Suggestion(SELL, BUY, HODL))
import Model.Analysis.Analysis (AnalysisResult(suggestion))



spec :: Spec
spec = do
    describe "RSI analysis" $ do
        it "returns a SELL suggestion if the RSI curve crosses the 70 line downwards" $ do
            let asset = rsiToAsset [72, 68]
            let result = Analysis.rsi asset
            suggestion result `shouldBe` SELL

        it "returns a SELL suggestion if the previous RSI was 70 and the current is lower" $ do
            let asset = rsiToAsset [70, 68]
            let result = Analysis.rsi asset
            suggestion result `shouldBe` SELL

        it "returns a BUY suggestion if the RSI curve crosses the 30 line upwards" $ do
            let asset = rsiToAsset [28, 31]
            let result = Analysis.rsi asset
            suggestion result `shouldBe` BUY

        it "returns a BUY suggestion if the previous RSI was 30 and the current is higher" $ do
            let asset = rsiToAsset [30, 32]
            let result = Analysis.rsi asset
            suggestion result `shouldBe` BUY

        it "returns a HODL suggestion if the RSI curve remains between the 30 and 70 lines" $ do
            let asset = rsiToAsset [55, 58]
            let result = Analysis.rsi asset
            suggestion result `shouldBe` HODL 

        it "returns a HODL suggestion if the RSI curve remains below the 30 line" $ do
            let asset = rsiToAsset [25, 27]
            let result = Analysis.rsi asset
            suggestion result `shouldBe` HODL 

        it "returns a HODL suggestion if the RSI curve remains over the 70 line" $ do
            let asset = rsiToAsset [72, 73]
            let result = Analysis.rsi asset
            suggestion result `shouldBe` HODL

        it "returns a HODL suggestion if the RSI curve crosses the 70 line upwards" $ do
            let asset = rsiToAsset [69, 72]
            let result = Analysis.rsi asset
            suggestion result `shouldBe` HODL

        it "returns a HODL suggestion if the RSI curve crosses the 30 line downwards" $ do
            let asset = rsiToAsset [31, 29]
            let result = Analysis.rsi asset
            suggestion result `shouldBe` HODL


rsiToAsset :: [Double] -> Asset
rsiToAsset rsi = Asset { rsi = rsi, candlesticks = [], macd = [], signal = [] }