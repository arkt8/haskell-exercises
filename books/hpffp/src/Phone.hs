module Phone where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe

type Digit = Char
type Presses = Int

newtype Key = Key ( [Digit] )
newtype Phone = Phone [ Key ]


myPhone :: Phone
myPhone = Phone [ Key "1"    , Key "2abc", Key "3def"
                , Key "4ghi" , Key "5jkl", Key "6mno"
                , Key "7pqrs", Key "8tuv", Key "9wxyz"
                , Key "*"    , Key "0+ _", Key "#.,"
                ]

charToAction :: Char -> Phone -> [ Maybe (Digit, Presses) ]
charToAction k phone@(Phone p)
  | Char.isAsciiUpper k = Just ('*', 1) : charToAction (Char.toLower k) phone
  | otherwise = [ foldl f Nothing p ]
  where f Nothing (Key a) = x
          where idx = List.elemIndex k a
                x = if Maybe.isNothing idx
                    then Nothing
                    else Just (head a, Maybe.fromMaybe 0 idx)
        f b (Key a) = b

textToAction :: String -> Phone -> [ Maybe (Digit, Presses) ]
textToAction text phone = foldl f [] text
  where f b a = b ++ charToAction a phone

countActions :: [ Maybe (Digit, Presses) ] -> Presses
countActions = foldl f 0
  where f b (Just ( _, p)) = b + p
        f b Nothing = b


-- >>> charToAction '~' myPhone
-- >>> action = textToAction "Hello!, how are you" myPhone
-- >>> action
-- >>> countActions action
-- [Nothing]
-- [Just ('*',1),Just ('4',2),Just ('3',2),Just ('5',3),Just ('5',3),Just ('6',3),Nothing,Just ('#',2),Just ('0',2),Just ('4',2),Just ('6',3),Just ('9',1),Just ('0',2),Just ('2',1),Just ('7',3),Just ('3',2),Just ('0',2),Just ('9',3),Just ('6',3),Just ('8',2)]
-- 42


convo :: [ String ]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn" ]


data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i)   = i
eval (Add a b) = eval a + eval b

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b


-- >>> a1 = Add (Lit 9001) (Lit 1)
-- >>> a2 = Add a1 (Lit 20001)
-- >>> a3 = Add (Lit 1) a2
-- >>> eval a3
-- >>> printExpr a3
-- 29004
-- "1 + 9001 + 1 + 20001"

