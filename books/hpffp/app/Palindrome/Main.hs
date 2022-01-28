module Main where

import Control.Monad
import qualified Data.Char as Char

isPalindrome :: String -> Bool
isPalindrome line = line' == reverse line'
  where
    line'  = map Char.toLower alphas
    alphas = filter f line
    f a = Char.isAscii a && Char.isAlpha a

main :: IO ()
main = forever $ do
  line <- getLine
  if isPalindrome line
    then putStrLn "It's a palindrome!"
    else putStrLn "Nope!"
