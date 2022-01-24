module Cipher where
import qualified Data.Char as Char
import qualified Data.List as List


toLowerList :: [Char] -> [Char]
toLowerList = map Char.toLower

-- >>> (caesarShift 2 'a', caesarShift (-2) 'a')
-- ('c','y')
caesarShift :: Int -> Char -> Char
caesarShift n chr
  | isAlpha   = Char.chr neword
  | otherwise = chr
  where
    ord    = Char.ord . Char.toLower $ chr
    shift  = rem (ord - 97 + n) 26
    neword = if shift >= 0
             then 97 + shift
             else 97 + 26 + shift
    isAlpha = Char.isAscii chr && Char.isAlpha chr


-- >>> x = toCaesar "hello world" 1000000
-- >>> y = fromCaesar x 1000000
-- >>> (x , y)
-- ("VSZZC KCFZR","hello world")
toCaesar :: String -> Int -> String
toCaesar txt shift =
  map (Char.toUpper . caesarShift shift) txt

fromCaesar :: String -> Int -> String
fromCaesar txt shift =
  map (caesarShift $ shift * (-1)) txt

-- >>> x = toVigenere' "attackatdawn" "lemon"
-- >>> y = fromVigenere x "lemon"
-- >>> (x,y)
-- ("LXFOPVEFRNHR","attackatdawn")
--
toVigenere :: String -> String -> String
toVigenere txt key =
  zipWith f txt' $ List.cycle key'
  where
    txt'  = toLowerList txt
    key'  = cycle . toLowerList $ key
    f a b =
      Char.toUpper $ caesarShift (Char.ord b - 97) a

fromVigenere :: String -> String -> String
fromVigenere cipher key =
  zipWith f cipher' key'
  where cipher' = toLowerList cipher
        key'    = cycle . toLowerList $ key
        f a b   = caesarUnshift (Char.ord b) a
        caesarUnshift a b =
          caesarShift ((-1) * (a - 97)) b

