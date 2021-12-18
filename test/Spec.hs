import Test.Hspec (hspec)
import qualified CandlesticksSpec

main :: IO ()
main = hspec $ do
    CandlesticksSpec.spec
