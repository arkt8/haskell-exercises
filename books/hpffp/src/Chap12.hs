module Chap12 where
import qualified Data.Either as Either

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a     = Just a

thePhrase :: String
thePhrase = "the cow loves us"

-- >>> replaceThe thePhrase
replaceThe :: String -> String
replaceThe text = foldr f "" mws  
  where
    f Nothing  [] = "a"
    f Nothing  b = "a" ++ " " ++ b
    f (Just a) [] = a
    f (Just a) b = a ++ " " ++ b
    ws = words text
    mws = map notThe ws

countVowels :: String -> Integer
countVowels = foldr f 0
  where f a b = if isVowel a then b + 1 else b

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

isConsonant :: Char -> Bool
isConsonant = flip elem "bcdfghjklmnpqrstvwxyz"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = f 0 . words
  where
    f total [] = total
    f total ("the":w@(c:_):ws) =
      if isVowel c
        then f (total + 1) (w:ws)
        else f  total      (w:ws)
    f total (w:ws) = f total ws

-- >>> countTheBeforeVowel "the apes and the gorillas are in the area"
-- >>> countTheBeforeVowel "the string"
-- >>> countTheBeforeVowel "some nothing"
countWordBeforeVowel :: String -> String -> Integer
countWordBeforeVowel word = f 0 word . words
  where
    f total word [] = total
    f total word (w1:w2:ws) =
      if (w1 == word) && (isVowel . head $ w2)
        then f (total+1) word (w2:ws)
        else f  total    word (w2:ws)

newtype Text' = Text' String
  deriving Show

-- A form of validation, where only consider valid phrases
-- where there is more vowels than consonants
-- >>> mkText "oi"
-- >>> mkText "um teste de mais palavras"
-- >>> mkText "sera que quanto maior a frase, menos vogais?"
-- Nothing
-- Just (Text' "um teste de mais palavras")
-- Nothing
mkText :: String -> Maybe Text'
mkText text =
  if uncurry (>) pair
    then Just (Text' text)
    else Nothing
  where
    pair = foldr f (0,0) text
    f a b@(b1,b2)
      | isVowel     a = (b1,   b2+1)
      | isConsonant a = (b1+1, b2  )
      | otherwise     = b


-- Creating a representation of natureal numbers
data Nat = Zero | Succ Nat
  deriving (Eq, Show)

-- >>> natToInteger Zero
-- >>> natToInteger (Succ Zero)
-- >>> natToInteger (Succ (Succ Zero))
-- 0
-- 1
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = 1 + natToInteger a



-- >>> integerToNat 3
-- >>> integerToNat 1
-- >>> integerToNat 0
-- >>> integerToNat (-2)
-- Just (Succ (Succ (Succ Zero)))
-- Just (Succ Zero)
-- Just Zero
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat num
  | num < 0   = Nothing
  | otherwise = Just (tonat num)
  where
    tonat :: Integer -> Nat
    tonat 0 = Zero
    tonat n = Succ (tonat (n - 1))



-- | -------------------------------------
-- | Small Maybe library
-- | -------------------------------------

-- >>> isJust (Just "hi")
-- >>> isJust Nothing
-- True
-- False
isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just a) = True

-- >>> isNothing (Just "hi")
-- >>> isNothing Nothing
-- False
-- True
isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just a) = False

-- >>> maybe' 0 (+1) Nothing
-- >>> maybe' 0 (+1) (Just 1)
-- 0
-- 2
maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' _ f (Just a) = f a
maybe' n _ Nothing = n

-- >>> listToMaybe [1,2,3]
-- >>> listToMaybe []
-- Just 1
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

-- >>> maybeToList (Just 1)
-- >>> maybeToList Nothing
-- [1]
-- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]


-- >>> dropNothing [Just 1, Nothing, Just 2]
-- >>> dropNothing $ take 3 $ repeat Nothing
-- [1,2]
-- []
dropNothing :: [Maybe a] -> [a]
dropNothing = foldr f []
  where f (Just a) b = a : b
        f Nothing  b = b

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Just [1,2,3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where
    f Nothing  _        = Nothing
    f _        Nothing  = Nothing
    f (Just a) (Just b) = Just (a:b)

-- | -------------------------------------
-- | Small Either library
-- | -------------------------------------

eitherList :: [Either Integer Char]
eitherList = [Left 3, Right 'c', Left 8, Right 'h']

-- >>> lefts' eitherList  
-- >>> rights' eitherList  
-- [3,8]
-- "ch"
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left a) b = a:b
    f _        b = b


rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right a) b = a:b
    f _         b = b

-- >>> partitionEithers' eitherList
-- ([3,8],"ch")
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' = foldr f ([],[])
  where f (Left  a) (l,r) =  (a:l,r)
        f (Right a) (l,r) =  (l,a:r)

-- >>> eitherMaybe' id (Left 10)
-- >>> eitherMaybe' id (Right 10)
-- Nothing
-- Just 10
eitherMaybe' :: (b->c) -> Either a b -> Maybe c
eitherMaybe' f (Right e) = Just (f e)
eitherMaybe' _ (Left _)  = Nothing

-- >>> either' (\a -> a^2) (\a -> a^3) (Right 3)
-- >>> either' (\a -> a^2) (\a -> a^3) (Left  2)
-- 27
-- 4
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ f (Right a) = f a

-- Same as eitherMaybe' but using either'
-- >>> eitherMaybe'' id (Left 10)
-- >>> eitherMaybe'' id (Right 10)
-- Nothing
-- Just 10
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

