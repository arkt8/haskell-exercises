module Main where
-- To make an infinite loop
import Control.Monad (forever)

-- To convert all chars to lowercase
import Data.Char     (toLower)

-- To hold info of discovered characters
import Data.Maybe    (isJust, fromMaybe)

-- To space guessed characters
import Data.List     (intersperse)

-- To exit successfully
import System.Exit   (exitSuccess)

-- For random word selection
import System.Random (randomRIO, Random (random))
import System.IO

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

-- give a shorter list of words
gameWords :: IO WordList
gameWords = do filter gameLength <$> allWords
  where
    gameLength w = l >= minWordLength
                && l < maxWordLength
      where l = length w

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO ( 0 , length wl - 1 )
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Integer

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle a _ _ _) b = b `elem` a

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ a _) b = b `elem` a

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

instance Show Puzzle where
  show (Puzzle _ found guessed errors) =
    intersperse ' ' (fmap renderPuzzleChar found)
    ++ " Guessed so far: " ++ guessed ++ " (Errors: " ++ show errors ++ ")"

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s e) c =
  Puzzle word newFilledInSoFar (c : s) e
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
    newFilledInSoFar =
      zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle@(Puzzle word found guessed errors) guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character,\
               \ pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word,\
               \ filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the\
               \ word, try again."
      return (fillInCharacter (Puzzle word found guessed (errors + 1)) guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed errors) =
  if errors > 9 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ word
       exitSuccess
  else
    return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _ _) =
  if all isJust filledInSoFar
  then do putStrLn "You win!"
          putStrLn $ "The word was: " ++ word
          exitSuccess
  else return()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must\
                   \ be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle


