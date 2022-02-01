-- The book suggests the creation of a module called "Addition"
-- creating it via cabal file to, only then, use it via stack.
-- I prefered discover how to do it using package.yaml and
-- add it a name nearer to the lesson.

-- To run these tests, run `stack test` to perform all
-- correctly tests specified under packages.yaml file

module Main where
import Test.Hspec ( hspec, describe, it, shouldBe )
import Chap08

sayHello :: IO ()
sayHello = putStrLn "hello"

main :: IO()
main = hspec $ do
  describe "Addition" $ do
    it "1+1 is greather than 1" $ do
      (1+1) > 1 `shouldBe` True
    it "2+2 is equal to 4" $ do
      (2+2) `shouldBe` 4
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5,0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4,2)
