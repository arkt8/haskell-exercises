module Chap10 where
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [ DatabaseItem ]
theDatabase = [ DbDate   ( UTCTime (fromGregorian 1911 5 1)
                                   (secondsToDiffTime 34123) )
              , DbNumber 9001
              , DbNumber 3002
              , DbString "Hello, world!"
              , DbDate   ( UTCTime (fromGregorian 1921 5 1)
                                   (secondsToDiffTime 34123) )
              ]

filterDbDate :: [ DatabaseItem ] -> [ UTCTime ]
filterDbDate = map g . filter f
  where
    f (DbDate a) = True
    f _          = False
    g (DbDate a) = a

-- >>> filterDbDate theDatabase
-- [1911-05-01 09:28:43 UTC,1921-05-01 09:28:43 UTC]


filterDbNumber :: [ DatabaseItem ] -> [ Integer ]
filterDbNumber = map g . filter f
  where
    f (DbNumber a) = True
    f _            = False
    g (DbNumber a) = a

-- >>> filterDbNumber theDatabase
-- [9001]

mostRecent :: [ DatabaseItem ] -> UTCTime
mostRecent = foldr max b . filterDbDate
  where b = UTCTime (fromGregorian 1500 1 1) (secondsToDiffTime 34123)
-- >>> mostRecent theDatabase
-- 1921-05-01 09:28:43 UTC

sumDb :: [ DatabaseItem ] -> Integer
sumDb = sum . filterDbNumber
-- >>> sumDb theDatabase
-- 12003

avgDb :: [ DatabaseItem ] -> Double
avgDb db = (fromIntegral . sum) dbnum / (fromIntegral . length) dbnum
  where dbnum = filterDbNumber db

-- >>> avgDb theDatabase
-- 6001.5

-- ################
-- # Scans Exercises

-- # 1. Fibonacci function to only return 20 first numbers
fst20fibo :: [Integer]
fst20fibo = take 20 $ 1 : scanl (+) 1 fst20fibo
-- >>> fst20fibo
-- [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]

-- # 2. Fibonacci function to return only numbers < 100
under100fibo :: [Integer]
under100fibo = takeWhile (\a -> a < 100) $ 1 : scanl (+) 1 under100fibo
-- >>> under100fibo
-- [1,1,2,3,5,8,13,21,34,55,89]

-- 3. Factorial function using scan
factorial :: [Integer]
factorial = 0 : scanl (*) 1 [2..]
-- >>> factorial !! 5
-- 120
--

-- ################
-- # 10.10 Exercises

stops = "pbtd"
vowels = "aeiou"

-- # 1. STOP WORDS
-- # a. triples of all possible stop-vowel-stop combos
-- # b. only return combos that begin with a "p"
-- # c. nouns and verbs instead of stops and vowels

stoptriples start mid end = [ (s,m,e) | s<-start, m<-mid, e<-end ]
-- >>> stoptriples stops vowels stops
-- [('p','a','p'),('p','a','b'),('p','a','t'),('p','a','d'),('p','e','p'),('p','e','b'),('p','e','t'),('p','e','d'),('p','i','p'),('p','i','b'),('p','i','t'),('p','i','d'),('p','o','p'),('p','o','b'),('p','o','t'),('p','o','d'),('p','u','p'),('p','u','b'),('p','u','t'),('p','u','d'),('b','a','p'),('b','a','b'),('b','a','t'),('b','a','d'),('b','e','p'),('b','e','b'),('b','e','t'),('b','e','d'),('b','i','p'),('b','i','b'),('b','i','t'),('b','i','d'),('b','o','p'),('b','o','b'),('b','o','t'),('b','o','d'),('b','u','p'),('b','u','b'),('b','u','t'),('b','u','d'),('t','a','p'),('t','a','b'),('t','a','t'),('t','a','d'),('t','e','p'),('t','e','b'),('t','e','t'),('t','e','d'),('t','i','p'),('t','i','b'),('t','i','t'),('t','i','d'),('t','o','p'),('t','o','b'),('t','o','t'),('t','o','d'),('t','u','p'),('t','u','b'),('t','u','t'),('t','u','d'),('d','a','p'),('d','a','b'),('d','a','t'),('d','a','d'),('d','e','p'),('d','e','b'),('d','e','t'),('d','e','d'),('d','i','p'),('d','i','b'),('d','i','t'),('d','i','d'),('d','o','p'),('d','o','b'),('d','o','t'),('d','o','d'),('d','u','p'),('d','u','b'),('d','u','t'),('d','u','d')]

-- >>> stoptriples "p" vowels stops
-- [('p','a','p'),('p','a','b'),('p','a','t'),('p','a','d'),('p','e','p'),('p','e','b'),('p','e','t'),('p','e','d'),('p','i','p'),('p','i','b'),('p','i','t'),('p','i','d'),('p','o','p'),('p','o','b'),('p','o','t'),('p','o','d'),('p','u','p'),('p','u','b'),('p','u','t'),('p','u','d')]

nouns = [ "red", "way", "sky" ]
verbs = [ "paint", "walk", "fly" ]

-- >>> stoptriples nouns verbs nouns
-- [("red","paint","red"),("red","paint","way"),("red","paint","sky"),("red","walk","red"),("red","walk","way"),("red","walk","sky"),("red","fly","red"),("red","fly","way"),("red","fly","sky"),("way","paint","red"),("way","paint","way"),("way","paint","sky"),("way","walk","red"),("way","walk","way"),("way","walk","sky"),("way","fly","red"),("way","fly","way"),("way","fly","sky"),("sky","paint","red"),("sky","paint","way"),("sky","paint","sky"),("sky","walk","red"),("sky","walk","way"),("sky","walk","sky"),("sky","fly","red"),("sky","fly","way"),("sky","fly","sky")]

-- # 2. Rewrite it using fractional division
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

seekritFunc' :: (Fractional a) => String -> a
seekritFunc' x =
  (/) ( sum $ map flen w )
      (flen w)
  where w = words x
        flen = fromIntegral . length

-- >>> phrase="Ola, tudo bem? Como vai? Aqui, tudo bem!"
-- >>> seekritFunc  phrase
-- >>> seekritFunc' phrase
-- 4
-- 4.125

-- >>> and [True, False, False]
-- False

-- ################
-- # Rewriting functions using fold

myAnd :: [ Bool ] -> Bool
myAnd = foldr (&&) True
-- >>> myAnd [False, False, True]
-- False

myOr :: [ Bool ] -> Bool
myOr = foldr (||) False
-- >>> myOr [False, True, False]
-- >>> myOr [False, False, False]
-- True
-- False

myAny :: ( a -> Bool ) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- >>> myAny even [1,3,5]
-- False

-- >>> myAny odd [2,1,8]
-- True

myElem :: Eq a => a -> [a] -> Bool
myElem n = foldr ((||) . (==) n) False
-- >>> myElem 1 [1..10] -- True
-- >>> myElem 1 [2..10] -- False
-- True
-- False

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []
-- >>> myReverse "blah"
-- "halb"

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []
-- >>> myMap (* 2) [1..5]
-- [2,4,6,8,10]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []
-- >>> myFilter odd [1..10]
-- >>> myFilter even [1..10]
-- [1,3,5,7,9]
-- [2,4,6,8,10]

squish :: [[a]] -> [a]
squish = foldr (++) []
-- >>> squish [ "oi", " tudo bem" ]
-- "oi tudo bem"

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []
-- >>> squishMap (\x->[1,x,3]) [2] -- [1,2,3]
-- >>> squishMap (\x->['W',x,'t',' ']) "aeiou"
-- [1,2,3]
-- "Wat Wet Wit Wot Wut "

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- Same as squish, but reusing squishMap
-- >>> squishAgain [ "oi ", "tudo bem" ]
-- "oi tudo bem"

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr ff (last xs) xs
  where ff a b | f a b == GT = a
               | otherwise = b

-- >>> myMaximumBy (\_ _ -> GT) [1..10] --  1
-- >>> myMaximumBy (\_ _ -> LT) [1..10] --  10
-- >>> myMaximumBy compare      [1..10] --  10
-- 1
-- 10
-- 10

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr ff (last xs) xs
  where ff a b | f a b == LT = a
               | otherwise = b
-- >>> myMinimumBy (\_ _ -> GT) [1..10] -- 10
-- >>> myMinimumBy (\_ _ -> LT) [1..10] -- 1
-- >>> myMinimumBy compare      [1..10] -- 1
-- 10
-- 1
-- 1


