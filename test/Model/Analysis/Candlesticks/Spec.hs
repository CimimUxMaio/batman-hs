module Model.Analysis.Candlesticks.Spec where 

import Test.Hspec (Spec, describe)
import qualified Model.Analysis.Candlesticks.HelpersSpec as HelperSpec
import qualified Model.Analysis.Candlesticks.Patterns.Spec as PatternsSpec


spec :: Spec
spec = do
    describe "candlesticks" $ do
        PatternsSpec.spec
        HelperSpec.spec
        