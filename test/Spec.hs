
-- TODO: create tests with HSpec (specify the test dependency)
import Test.Hspec

import qualified EvaluatingSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Evaluating"      EvaluatingSpec.spec
