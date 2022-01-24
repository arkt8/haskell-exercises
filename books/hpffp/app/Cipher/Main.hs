module Main where

import qualified Data.Char as Char
import Cipher
import System.IO

chooseCipher :: IO()
chooseCipher = undefined

runVigenere :: IO()
runVigenere = do
  putStrLn "\n\nVIGENERE CIPHER"
  putStr "Text: "
  txt <- getLine
  putStr "Key word: "
  key <- getLine

  putStrLn $ toVigenere txt key
  return ()

runCaesar :: IO ()
runCaesar = do
  putStrLn "\n\nCAESAR CIPHER"
  putStr "Text: "
  txt <- getLine
  putStr "Key number: "
  key <- getLine

  putStrLn $ toCaesar txt (read key :: Int)
  return ()

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStrLn "Choose the Cipher:"
  putStr "(C)aesar or (V)igenere? "
  opt <- getChar

  hSetBuffering stdin LineBuffering
  case opt of
    'v' -> runVigenere
    'c' -> runCaesar
    _   -> main
  return ()
