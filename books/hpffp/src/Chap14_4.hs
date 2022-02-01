module Chap14_4 where
import Test.Hspec
import Test.QuickCheck

---------------------------------------
-- 14.4 - QuickCheck
-- Gen type, sample, sample', arbitrary
---------------------------------------
-- >>> sample' genBool
-- [True,True,False,False,True,True,True,False,False,False,True]
genBool :: Gen Bool
genBool = choose (False, True)

-- >>> sample' genBool'
-- [False,True,True,True,True,False,False,False,False,True,True]
genBool' :: Gen Bool
genBool' = elements [False, True]

-- >>> sample' genOrdering
-- [GT,EQ,EQ,LT,EQ,GT,EQ,GT,EQ,EQ,GT]
genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

-- >>> sample' genChar
-- "mnijjdaewad"
genChar :: Gen Char
genChar = elements ['a'..'z']

-- >>> x <- sample' (genTuple :: Gen (Int,Char))
-- >>> take 4 x
-- [(0,'2'),(2,'='),(-1,'\1019377'),(5,'\US')]
genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

-- >>> x <- sample' (genThreeple :: Gen (Word, String, Bool))
-- >>> take 4 x
-- [(0,"",False),(0,"\FS",True),(0,"\95142",True),(0,"\SI'\49868",True)]
genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c)
            => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

