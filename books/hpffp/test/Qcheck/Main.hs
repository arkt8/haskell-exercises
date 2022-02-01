module Main where
import Test.Hspec
import Test.QuickCheck

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

-- Run tests using hspec via QuickCheck. Compare with the
-- output of the test/Hspec/Main.hs to see the difference.
testWithHspec :: IO ()
testWithHspec = hspec $ do
  it "x+1 is always gt than x" $ do
    property prop_additionGreater

-- Run tests using QuickCheck whithout the need of Test.Hspec
-- i.e. it works without importing Test.Hspec.
testWithQcheck :: IO ()
testWithQcheck =
  -- here the function name is shown correctly
  quickCheck prop_additionGreater





main :: IO()
main = do
--  testWithHspec
  testWithQcheck
-- See: the generator functions at Chap14.hs


