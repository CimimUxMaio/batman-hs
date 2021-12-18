import Test.Hspec (hspec)
import qualified Candlesticks.Spec as CandlesticksSpec

main :: IO ()
main = hspec $ do
    CandlesticksSpec.spec
