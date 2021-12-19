import Test.Hspec (hspec)
import qualified Model.Spec as ModelSpec

main :: IO ()
main = hspec $ do
    ModelSpec.spec
