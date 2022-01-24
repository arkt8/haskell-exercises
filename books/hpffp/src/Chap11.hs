module Chap11 where
import Data.Char

data Nullary = MkNullaryA
data Unary a = MkUnaryA a

-- >>> :kind Nullary
-- >>> :kind Unary
-- Nullary :: *
-- Unary :: * -> *

-- >>> :type MkNullaryA
-- >>> :type MkUnaryA
-- MkNullaryA :: Nullary
-- MkUnaryA :: a -> Unary a

-- >>> :info Nullary
-- type Nullary :: *
-- data Nullary = MkNullaryA | MkNullaryB
--   	-- Defined at /home/thadeu/haskell/exercises/FromFirstPrinciples/chap11.hs:1:1

-- >>> :info Unary
-- type Unary :: * -> *
-- data Unary a = MkUnaryA a | MkUnaryB a
--   	-- Defined at /home/thadeu/haskell/exercises/FromFirstPrinciples/chap11.hs:2:1

data OperatingSystem = GnuLinux
                     | OpenBSD
                     | Mac
                     | Windows
                     deriving (Eq, Show, Enum)

data ProgLang = Agda
              | Haskell
              | Idris
              | PureScript
              deriving (Eq, Show, Enum)

data Programmer = Programmer
                { os   :: OperatingSystem
                , lang :: ProgLang }
                deriving (Eq,Show)


oses :: [OperatingSystem]
oses = [ GnuLinux .. Windows ]

pls :: [ProgLang]
pls = [Agda .. PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer x y | x <- oses, y <- pls ]

-- >>> allProgrammers
-- [Programmer {os = GnuLinux, lang = Agda},Programmer {os = GnuLinux, lang = Haskell},Programmer {os = GnuLinux, lang = Idris},Programmer {os = GnuLinux, lang = PureScript},Programmer {os = OpenBSD, lang = Agda},Programmer {os = OpenBSD, lang = Haskell},Programmer {os = OpenBSD, lang = Idris},Programmer {os = OpenBSD, lang = PureScript},Programmer {os = Mac, lang = Agda},Programmer {os = Mac, lang = Haskell},Programmer {os = Mac, lang = Idris},Programmer {os = Mac, lang = PureScript},Programmer {os = Windows, lang = Agda},Programmer {os = Windows, lang = Haskell},Programmer {os = Windows, lang = Idris},Programmer {os = Windows, lang = PureScript}]

-- >>> length allProgrammers
-- 16

-- |-----------------------------------------------------
-- | 11.17 Binary Tree
-- |-----------------------------------------------------

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

btins :: Ord a => a -> BinaryTree a -> BinaryTree a
btins b Leaf = Node Leaf b Leaf
btins b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (btins b left) a right
  | b > a = Node left a (btins b right)


mapTree :: ( a -> b ) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node (←) value (→)) = ( Node 
                                   (mapTree f (←))
                                   (f value)
                                   (mapTree f (→))
                                 )

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l a r) = inorder l ++ [a] ++ inorder r

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l a r) = [a] ++ preorder l ++ preorder r

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l a r) = postorder l ++ postorder r ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node l a r) = foldTree f ( f a (foldTree f b l) ) r

-- >>> bt1 = btins 1 Leaf
-- >>> bt2 = btins 2 bt1
-- >>> bt3 = btins 3 bt2
-- >>> bt3
-- >>> testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
-- >>> mapTree (* 10) bt3
-- >>> mapTree (* 10) testTree
-- >>> inorder   testTree
-- >>> preorder  testTree
-- >>> postorder testTree
-- >>> foldTree (-) 0 testTree
-- Node Leaf 1 (Node Leaf 2 (Node Leaf 3 Leaf))
-- Node Leaf 10 (Node Leaf 20 (Node Leaf 30 Leaf))
-- Node (Node Leaf 10 Leaf) 20 (Node Leaf 30 Leaf)
-- [1,2,3]
-- [2,1,3]
-- [1,3,2]
-- 2

-- |---------------------------------------------
-- | 11.18 Chapter Exercises
-- |---------------------------------------------

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf as@(a:at) (b:bt) =
  if b == a
  then isSubseqOf at bt
  else isSubseqOf as bt

-- >>> isSubseqOf "blah" "blahwoot" == True
-- >>> isSubseqOf "blah" "wootblah" == True
-- >>> isSubseqOf "blah" "wboloath" == True
-- >>> isSubseqOf "blah" "wootbla"  == False
-- >>> isSubseqOf "blah" "halbwoth" == False
-- >>> isSubseqOf "blah" "blawhoot" == True
-- True
-- True
-- True
-- True
-- True
-- True


-- Observe que estamos usando apenas ascii e apenas queria dar uma
-- simples ao exercício. O ideal seria verificar se o código retornado
-- por ord está no intervalo das minusculas e, só então retornar o valor
-- alterado para chr.
capitalizeWord :: String -> String
capitalizeWord (l:ls) = uc l : ls
  where uc = chr . (\n -> n - 32) . ord

capitalizeWords :: String -> [(String,String)]
capitalizeWords text = [ (w, capitalizeWord w) | w <- words text ]

capitalizeParagraph :: String -> String
capitalizeParagraph p = foldl f "" w
  where w = words p
        f "" a = capitalizeWord a
        f b  a = if   elem l ['.','?','!']
                 then b ++ " " ++ capitalizeWord a
                 else b ++ " " ++ a
                 where l = last b

-- >>> capitalizeWords "hello worlds"
-- >>> capitalizeParagraph "hello world. how are you? ok"
-- [("hello","Hello"),("worlds","Worlds")]
-- "Hello world. How are you? Ok"


