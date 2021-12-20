module Model.Analysis.MACDAnalysisSpec where
import Test.Hspec (Spec, describe, shouldBe, it)
import Model.Asset (Asset(Asset, candlesticks, rsi, macd, signal))
import Model.Analysis.Analysis (AnalysisResult(suggestion))
import qualified Model.Analysis.Analysis as Analysis
import Model.Analysis.Suggestion (Suggestion(SELL, BUY, HODL))
import Control.Exception (assert)



spec :: Spec
spec = do
    describe "MACD analysis" $ do
        it "returns a SELL suggestion if the MACD curve crosses the signal curve downwards" $ do
            let asset = macdSignalToAsset [100, 90] [90, 100]
            let result = Analysis.macd asset
            suggestion result `shouldBe` SELL

        it "returns a BUY suggestion if the MACD curve crosses the signal curve upwards" $ do
            let asset = macdSignalToAsset [30, 60] [35, 45]
            let result = Analysis.macd asset
            suggestion result `shouldBe` BUY
        
        it "returns a HODL suggestion if the MACD curve does not cross the signal curve" $ do
            let asset = macdSignalToAsset [200, 250] [190, 210]
            let result = Analysis.macd asset
            suggestion result `shouldBe` HODL

        it "returns a HODL suggestion if the MACD curve remains greater than the signal curve" $ do
            let asset = macdSignalToAsset [20, 15] [5, 11]
            let result = Analysis.macd asset
            suggestion result `shouldBe` HODL

        it "returns a HODL suggestion if the MACD curve remains lower than the signal curve" $ do
            let asset = macdSignalToAsset [110, 100] [200, 230]
            let result = Analysis.macd asset
            suggestion result `shouldBe` HODL


macdSignalToAsset :: [Double] -> [Double] -> Asset
macdSignalToAsset macd signal = 
    Asset { candlesticks = [], rsi = [], macd = macd, signal = signal }
