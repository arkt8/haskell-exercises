module Main where

import MonoidAssoc
import MonoidOptional
import Test.QuickCheck
import Data.Monoid

main :: IO ()
main = do
  quickCheck ( monoidAssoc :: FirstMappend )
  quickCheck ( monoidLeftIdentity :: FstId )
  quickCheck ( monoidRightIdentity :: FstId )
  return ()

newtype First' a =
  First' { getFirst :: Optional a }
  deriving (Eq, Show)

instance Semigroup a => Semigroup (First' a)
  where (<>) (First' a) (First' b) = First' (a <> b)

instance Monoid a => Monoid (First' a)
  where mempty = First' Nada
        mappend (First' a) (First' b) = First' (a <> b)

instance (Monoid a, Arbitrary a) => Arbitrary (First' a)
  where arbitrary = genFirst'

-- >>> :t monoidAssoc
-- monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
type FirstMappend = First' String
                 -> First' String
                 -> First' String
                 -> Bool

-- >>> :t monoidLeftIdentity
-- >>> :t monoidRightIdentity
-- monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
-- monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
type FstId = First' String -> Bool

-- In the book have a `First' (Only 1)` but remember:
-- Numbers hasn't monoid associated directly to them, because
-- each number type has more than one associativity relationship!
-- >>> First' (Only (Sum 1)) `mappend` First' Nada
-- >>> First' (Only "hi") `mappend` First' (Only "hey")
-- >>> First' Nada        `mappend` First' (Only "hey")
-- >>> First' Nada        `mappend` First' Nada
-- First' {getFirst = Only (Sum {getSum = 1})}
-- First' {getFirst = Only "hihey"}
-- First' {getFirst = Only "hey"}
-- First' {getFirst = Nada}

-- See here how to generate for a parameterized type!!!!
genFirst' :: (Arbitrary a, Monoid a) => Gen (First' a)
genFirst' = do
  a <- arbitrary
  frequency [(1, return (First' Nada))
            ,(1, return (First' (Only a)))]

