{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import GHC.Real (Integral)
import Chap11 (capitalizeWord)
import Data.List as L
import Test.QuickCheck
-- For QuickCheck Ex. 8
import GHC.Generics

main :: IO()
main = do
  putStrLn "Ex. 1 - Recoveranble value from its part"
  quickCheck prop_recoverable

  putStrLn "Ex. 2 - Once sorted nothing changes"
  quickCheck $ prop_sortUniqueResult @[ String ]
  quickCheck $ prop_sortUniqueResult @[ Double ]
  quickCheck $ prop_sortUniqueResult @[ Integer ]
  quickCheck $ prop_sortUniqueResult @[ Char ]

  putStrLn "Ex. 3 - Sum Properties"
  -- Double and Floats not work!
  -- quickCheck $ prop_plusAssociative @Double
  -- quickCheck $ prop_plusAssociative @Float
  quickCheck $ prop_plusAssociative @Int
  quickCheck $ prop_plusAssociative @Integer
  quickCheck $ prop_plusAssociative @Word

  quickCheck $ prop_plusCommutative @Double
  quickCheck $ prop_plusCommutative @Float
  quickCheck $ prop_plusCommutative @Int
  quickCheck $ prop_plusCommutative @Integer
  quickCheck $ prop_plusCommutative @Word

  putStrLn "Ex. 4 - Multiplication properties"
  -- Double and Floats not work!
  -- quickCheck $ prop_plusAssociative @Double
  -- quickCheck $ prop_multiAssociative @Double
  -- quickCheck $ prop_multiAssociative @Float
  quickCheck $ prop_multiAssociative @Int
  quickCheck $ prop_multiAssociative @Integer
  quickCheck $ prop_multiAssociative @Word

  quickCheck $ prop_multiCommutative @Double
  quickCheck $ prop_multiCommutative @Float
  quickCheck $ prop_multiCommutative @Int
  quickCheck $ prop_multiCommutative @Integer
  quickCheck $ prop_multiCommutative @Word

  putStrLn "Ex. 5 - quot·rem vs. div·mod properties"
  quickCheck $ prop_quotientRemainder @Int
  quickCheck $ prop_quotientRemainder @Integer
  quickCheck $ prop_quotientRemainder @Word

  quickCheck $ prop_divisionModule @Int
  quickCheck $ prop_divisionModule @Integer
  quickCheck $ prop_divisionModule @Word

  putStrLn "Ex. 6 - (^) is neither associative or commutative"
  -- quickCheck $ prop_powerAssociative @Int
  -- quickCheck $ prop_powerAssociative @Integer
  -- quickCheck $ prop_powerAssociative @Word
  -- quickCheck $ prop_powerCommutative @Int
  -- quickCheck $ prop_powerCommutative @Integer
  -- quickCheck $ prop_powerCommutative @Word

  putStrLn "Ex. 7 - twice reversion is the same as id"
  quickCheck prop_doubleReverseIsId

  putStrLn "Ex. 8 - ($) property"
  quickCheck $ prop_delayedEvaluator @Int
  quickCheck $ prop_delayedEvaluator @Integer

  putStrLn "Ex. 9 - Foldr vs Concat (slow)"
  -- quickCheck $ prop_foldrConcat @[[Int]]
  -- quickCheck $ prop_foldrConcat @[String]

  putStrLn "Ex. 10 - take always return same length IS FALSE!"
  -- quickCheck $ prop_takeAlwaysSameLength @[String]
  -- quickCheck $ prop_takeAlwaysSameLength @[Int]

  putStrLn "Ex. 11 - Composing read and show is the identity"
  quickCheck $ prop_readShowId @Double
  quickCheck $ prop_readShowId @Float
  quickCheck $ prop_readShowId @String

  putStrLn "Ex. 12 - Square Identity -- False!"
  -- quickCheck prop_squareIdentity

  putStrLn "Ex. 13 - Idempotence"
  quickCheck prop_capitalizeIdp
  quickCheck $ prop_sortIdp @Int
  quickCheck $ prop_sortIdp @Char
  quickCheck $ prop_sortIdp @Double

  return ()




-- QuickCheck Ex. 1
half x = x/2

prop_recoverable :: Double -> Bool
prop_recoverable x = ((*2) . half) x == x




-- QuickCheck Ex. 2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_,False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t)  = (Just y, x >= y)

prop_sortUniqueResult :: (Ord a) => [a] -> Bool
prop_sortUniqueResult = listOrdered . L.sort



-- QuickCheck Ex. 3
prop_plusAssociative
  :: forall a. (Num a, Ord a)
  => a -> a -> a -> Bool
prop_plusAssociative x y z =
  x + (y + z) == (x + y) + z

prop_plusCommutative
  :: forall a. (Num a, Ord a)
  => a -> a -> Bool
prop_plusCommutative x y =
  x + y == y + x



-- QuickCheck Ex. 4
prop_multiAssociative
  :: forall a. (Num a, Ord a)
  => a -> a -> a -> Bool
prop_multiAssociative x y z =
  x * (y * z) == (x * y) * z

prop_multiCommutative
  :: forall a. (Num a, Ord a)
  => a -> a -> Bool
prop_multiCommutative x y =
  x * y == y * x



-- QuickCheck Ex. 5
prop_quotientRemainder
  :: forall a. (Ord a, Integral a)
  => a -> a -> Bool
prop_quotientRemainder = r
  where r 0 _ = True
        r _ 0 = True
        r x y = y * quot x y + rem x y == x

prop_divisionModule
  :: forall a. (Ord a, Integral a)
  => a -> a -> Bool
prop_divisionModule = r
  where r 0 _ = True
        r _ 0 = True
        r x y = y * div x y + mod x y == x



-- QuickCheck Ex. 6
prop_powerAssociative
  :: forall a. (Ord a, Integral a)
  => a -> a -> a -> Bool
prop_powerAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

prop_powerCommutative
  :: forall a. (Ord a, Integral a)
  => a -> a -> Bool
prop_powerCommutative x y =
  x ^ y == y ^ x



-- QuickCheck Ex. 7
prop_doubleReverseIsId :: [Integer] -> Bool
prop_doubleReverseIsId x =
  reverse (reverse x) == id x



-- QuickCheck Ex. 8
prop_delayedEvaluator
  :: forall a. (Ord a, Integral a)
  => a -> a -> Bool
prop_delayedEvaluator a b = x && y
  where
    a' = abs a
    b' = abs b
    x = (a * ( b + a )) == ((* a) $ b + a)
    y = ((a' ^ b') ^ a') == ((^ a') $ a' ^ b')



-- QuickCheck Ex. 9
-- foldr (:) a b == (++) a b = False
prop_foldrConsVsSimpleConcat
  :: forall a. Eq a
  => [a] -> [a] -> Bool
prop_foldrConsVsSimpleConcat a b = r1 == r2
  where r1 = foldr (:) a b
        r2 = (++) a b

--   the order of elements on resultant lists are different:
--   [b1, bn, a] vs [a, b1, bn]
prop_foldrConcat :: forall a. Eq a => [[a]] -> Bool
prop_foldrConcat a = r1 == r2
  where r1 = foldr (++) [] a
        r2 = concat a



-- QuickCheck Ex. 10
prop_takeAlwaysSameLength :: Int -> [a] -> Bool
prop_takeAlwaysSameLength n xs =
  length (take n xs) == n



-- QuickCheck Ex. 11
prop_readShowId :: (Eq a, Read a, Show a) => a -> Bool
prop_readShowId x = read (show x) == x



-- QuickCheck Ex. 12 (Failure)
-- Float/Double looses its precision on sqrt, do when
-- put on 2 power it can't have the same original value
square x = x * x
positiveGen :: (Num a, Enum a) => Gen a
positiveGen = elements [0 .. 100]

prop_squareIdentity :: Property
prop_squareIdentity =
  forAll positiveGen
  (\x -> (square . sqrt) x == abs x)



-- QuickCheck Ex. 13 (Idempotence)
run2x f = f . f
run4x = run2x . run2x

-- capitalizeWord is imported from Chap11
prop_capitalizeIdp :: [Char] -> Bool
prop_capitalizeIdp x =
  (capitalizeWord x == run2x capitalizeWord x)
  && (capitalizeWord x == run4x capitalizeWord x)

prop_sortIdp :: forall a. (Ord a, Eq a) => [a] -> Bool
prop_sortIdp x = 
  (sort x == run2x sort x) && (sort x == run4x sort x)

-- QuickCheck Ex.14 - Gen random

data Fool = Fulse | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = frequency [ (1, return Fulse),
                      (2, return Frue ) ]

instance Arbitrary Fool where
  arbitrary = foolGen

-- >>> x <- sample' foolGen
-- >>> take 5 x
-- [Fulse,Frue,Fulse,Frue,Frue]
