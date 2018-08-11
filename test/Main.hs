import Test.Tasty
import Test.Tasty.HUnit

import qualified Riak

main :: IO ()
main =
  Riak.withHandle "localhost" 8087
    (defaultMain . testGroup "Tests" . tests)

tests :: Riak.Handle -> [TestTree]
tests h =
  [ testCase "ping" $ do
      Riak.ping h >>= (@?= Right ())
  ]
