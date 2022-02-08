module Main where

type Name = String
type Age = Integer

data Person = Person Name Age
  deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0   = Left AgeTooLow
  | otherwise = Left
                $ PersonInvalidUnknown
                $ "Name was: " ++ show name
                ++" Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Name: "
  name <- getLine

  putStrLn "Age:  "
  age  <- getLine

  print $ readPerson $ mkPerson name $ read age
  return ()

readPerson (Left a) = "Error: " ++ show a
readPerson (Right a) =
  "Yay! Successfully got a person: "
  ++ show a

main :: IO()
main = gimmePerson
