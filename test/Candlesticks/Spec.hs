module Candlesticks.Spec where 
import Test.Hspec (Spec, describe)

import qualified Candlesticks.PatternsSpec as PatternsSpec
import qualified Candlesticks.UtilsSpec as UtilsSpec


spec :: Spec
spec = do
    describe "Model.Analysis.Candlesticks" $ do
        PatternsSpec.spec
        UtilsSpec.spec
        