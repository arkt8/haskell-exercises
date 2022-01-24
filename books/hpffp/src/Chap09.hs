module Chap09 where
-- >>> [ or [ False, False ], or [ False, True ] ]
-- [False,True]

squish :: [[a]] -> [a]
-- > squish [] = []
-- > squish (ls:lls) = ls ++ squish lls
-- equal to:
-- > squish = foldr (++) []
-- equal to:
squish = concat

-- >>> zip [1,2,3] [3,4,5]
-- [(1,3),(2,4),(3,5)]

squishList = [ [1,2,3], [3,4,5] ]
-- >>> squish squishList
-- [1,2,3,3,4,5]

-- >>> foldr (++) [] squishList
-- [1,2,3,3,4,5]

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- >>> squishMap (\x -> "A"++[x]++"B ") "123"
-- "A1B A2B A3B "

-- >>> squishMap (\x -> [1,x,3] ) [2,4,6]
-- [1,2,3,1,4,3,1,6,3]

htmlList [] _ = []
htmlList _ [] = []
htmlList (s:ls) (t:lt) = ("<"++t++">"++s++"</"++t++">") : htmlList ls lt

-- >>> htmlList ["item1", "item 2", "outro item"] ["li", "li","li"]
-- ["<li>item1</li>","<li>item 2</li>","<li>outro item</li>"]

