module Model.Analysis.Spec where
    
import Test.Hspec (Spec, describe)
import qualified Model.Analysis.Candlesticks.Spec as CandlesticksSpec
import qualified Model.Analysis.CandlesticksAnalysisSpec as CandlesticksAnalysisSpec


spec :: Spec
spec = do
    describe "analysis" $ do
        CandlesticksSpec.spec
        CandlesticksAnalysisSpec.spec