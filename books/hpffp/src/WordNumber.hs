-- 8.6 - Chapter Exercises
-- "Numbers into Words"

module WordNumber where
import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits n = foldr f [] (show n)
  where f a b = (read [a] :: Int) : b

-- >>> wordNumber 13567902468
-- "one-three-five-six-seven-nine-zero-two-four-six-eight"
wordNumber :: Int -> String
wordNumber n = intercalate "-"
  $ map digitToWord
  $ digits n


