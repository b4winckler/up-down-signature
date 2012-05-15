import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "group" [
      testProperty "prop1" prop1
    , testProperty "prop2" prop2
    ]
  ]
