module Model.Spec where
    
import Test.Hspec (Spec, describe)
import qualified Model.Analysis.Spec as AnalysisSpec


spec :: Spec
spec = do
    describe "analysis" $ do
        AnalysisSpec.spec