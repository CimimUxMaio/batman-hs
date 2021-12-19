module Candlesticks.Spec where 
import Test.Hspec (Spec, describe)

import qualified Candlesticks.HelpersSpec as HelperSpec
import qualified Candlesticks.Patterns.Spec as PatternsSpec


spec :: Spec
spec = do
    describe "Model.Analysis.Candlesticks" $ do
        PatternsSpec.spec
        HelperSpec.spec
        