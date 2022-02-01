-----------------------------------------
-- Chap 14.5 - Morse Code
-----------------------------------------
module Main where
import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Traversable (traverse)
import Morse (stringToMorse, morseToChar)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (getLine, hIsEOF, stdin, isEOF)


convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess
  -- Otherwise, proceed
  line <- getLine
  convertLine line
  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        Just s  -> putStrLn (intercalate " " s)
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess
  -- Otherwise, proceed
  line <- getLine
  convertLine line
  where
    convertLine line = do
      let decoded :: Maybe String
          decoded = traverse morseToChar ( words line )
      case decoded of
        Just s  -> putStrLn s
        Nothing -> do
          putStrLn $ "Error: " ++ line
          exitFailure

argError :: IO ()
argError = do
  putStrLn "Arguments can be 'from' or 'to'"
  exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] -> case arg of
             "from" -> convertFromMorse
             "to"   -> convertToMorse
             _      -> argError
    _     -> argError
