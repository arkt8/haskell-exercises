module MonoidAssoc where
import Data.Monoid
import Test.QuickCheck

-- How would be a function test...
-- Observe that the function parameter name is an infix
associativity :: Eq a
              => (a -> a -> a)
              -> a -> a -> a
              -> Bool
associativity (<>) a b c =
   (a <> b) <> c == a <> (b <> c)

-- >>> associativity (+) 1 2 3
-- >>> associativity (*) 1 2 3
-- >>> associativity (-) 1 2 3
-- >>> associativity (^) 2 3 4
-- True
-- True
-- False
-- False

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> b) <> c == a <> (b <> c)

monoidLeftIdentity, monoidRightIdentity
  :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity  a = (mempty <> a) == a
monoidRightIdentity a = (a <> mempty) == a

-- Tests are performed under
-- hpffp:test-monoid (test/Monoid/Main.hs)
monoidAssocTest :: IO ()
monoidAssocTest = do
  quickCheck ( monoidAssoc :: String -> String -> String -> Bool )
  quickCheck ( monoidLeftIdentity :: String -> Bool )
  quickCheck ( monoidRightIdentity :: String -> Bool )

