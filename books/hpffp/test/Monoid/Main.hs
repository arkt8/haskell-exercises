module Main where

import MonoidAssoc
import Control.Monad
import Data.Monoid
import Test.QuickCheck


main :: IO()
main = do
  -- tests for functions from src/MonoidAssoc.hs
  monoidAssocTest
  monoidBullTest



-- Testing an invalid Monoid
-- The test below should fail.
--
-- The reason is that mempty is Fools and any value
-- passed for mappend converts into Fools.
--
-- What occurs is that mappend shouldn't always be
-- an identity or it will not be an indentity!
--
-- When you run the test the error occurs when trying
-- to use the Twoo data constructor

data Bull = Fools | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (2, return Twoo) ]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend =
  Bull -> Bull -> Bull -> Bool

monoidBullTest :: IO ()
monoidBullTest = do
  quickCheck ( monoidAssoc :: BullMappend )
  quickCheck ( monoidLeftIdentity :: Bull -> Bool )
  quickCheck ( monoidRightIdentity :: Bull -> Bool )


