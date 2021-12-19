module Candlesticks.Patterns.Spec where


import Test.Hspec (Spec, describe)
import qualified Candlesticks.Patterns.HammerSpec as HammerSpec


spec :: Spec
spec = do
    describe "patterns" $ do
        HammerSpec.spec
