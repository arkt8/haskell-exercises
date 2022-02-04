---------------------------------------
-- 14.4 Morse code tests
---------------------------------------
module Main where
import qualified Data.Map as M
import Morse
import Test.QuickCheck

-- Generation:
-- allow to use valid values for tests

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

-- Properties:
-- validation of the values
-- >>> :t charToMorse
-- >>> :t morseToChar
-- charToMorse :: Char -> Maybe Morse
-- morseToChar :: Morse -> Maybe Char
prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
  (\c -> (charToMorse c >>= morseToChar) == Just c)


main :: IO ()
main = do
  quickCheck prop_thereAndBackAgain

