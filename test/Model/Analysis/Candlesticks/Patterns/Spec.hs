module Model.Analysis.Candlesticks.Patterns.Spec where


import Test.Hspec (Spec, describe)
import qualified Model.Analysis.Candlesticks.Patterns.HammerSpec as HammerSpec


spec :: Spec
spec = do
    describe "patterns" $ do
        HammerSpec.spec
