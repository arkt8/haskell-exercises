---------------------------------------
-- 14.6 - Arbitrary Instances
---------------------------------------
{-# LANGUAGE DeriveGeneric #-}
module Chap14_6 where

import Test.QuickCheck
import GHC.Generics

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

-- >>> sample' trivialGen
-- [Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial]
instance Arbitrary Trivial where
  arbitrary = trivialGen



-- Arbitrary identity
--
data Identity a = Identity a deriving (Eq,Show)

identityGen
  :: Arbitrary a
  => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a
  => Arbitrary (Identity a)
  where arbitrary = identityGen

-- >>> sample' identityGenInt
-- [Identity 0,Identity 1,Identity 0,Identity (-1),Identity (-5),Identity 8,Identity 1,Identity 5,Identity (-13),Identity 12,Identity 19]
identityGenInt
  :: Gen (Identity Int)
identityGenInt = identityGen


-- Arbitrary with Product Types
data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b)
  where arbitrary = pairGen

-- >>> sample' pairGenIntString
-- [Pair 0 "",Pair 2 "`K",Pair (-4) "k",Pair 1 "",Pair (-3) "\10956\"\1064164hC\SOH",Pair (-2) ":",Pair (-3) "M!o\1090723J~\32163\SI\v;\EOT",Pair (-1) ".\ENQolY}\n+\200434\59116O",Pair 8 "c\63112\GS\f",Pair 9 "\f\181115\1023821\&1L",Pair (-10) "\1098336C"]
pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

---------------------------------------
-- Arbitrary with Sum Types
---------------------------------------
data Sum a b = First a | Second b
  deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  -- oneof is used to choose between constructors
  oneof [return $ First a,
         return $ Second b]

-- >>> sample' sumGenCharInt
-- [First '\23066',Second 2,First 'R',Second 5,First 'p',First '2',First '\v',Second 4,Second 10,First '\1003538',Second (-8)]
sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

-- We can use different weighting for
-- constructors
sumGenUnequal :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenUnequal = do
  a <- arbitrary
  b <- arbitrary
  frequency [(2, return $ First a)
            ,(1,  return $ Second b)]

-- >>> sample' sumGenUnequalCharInt
-- [Second 0,First 'd',First 'd',Second 6,First '\1016969',Second 3,First 'T',Second 8,First 'O',Second (-17),First '(']
sumGenUnequalCharInt :: Gen (Sum Char Int)
sumGenUnequalCharInt = sumGenUnequal

-- Coarbitrary
-- Needs lang extension and import:
--   {-# LANGUAGE DeriveGeneric #-}
--   import GHC.Generics
data Bool' = True' | False'
  deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

-- >>> sample' trueGen
-- >>> :t trueGen
-- [0,0,0,3,-3,-8,12,4,4,3,4]
-- sample' trueGen :: IO [Int]
