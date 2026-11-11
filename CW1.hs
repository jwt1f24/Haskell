import Data.Char (toUpper, ord, chr)
import Data.List (sortBy)

-- ASSESSMENT A1
filterChar :: String -> String
filterChar = filter (`elem` ['A'..'Z']) . map toUpper

shiftChar :: Int -> Char -> Char
shiftChar n c = chr (ord 'A' + ((ord c - ord 'A' + n) `mod` 26))

vigenere :: String -> (String -> String, String -> String)
vigenere key = (encrypt, decrypt)
 where
  procKey = if null (filterChar key) then "A" else filterChar key
  zipKey m = zip (filterChar m) (cycle procKey)
  encrypt m = map (\(p, k) -> shiftChar (ord k - ord 'A') p) (zipKey m)
  decrypt m = map (\(c, k) -> shiftChar (26 - (ord k - ord 'A')) c) (zipKey m)


-- ASSESSMENT A2
frequency :: Int -> String -> [[(Char, Int)]]
frequency n ct = map (sortBy comp . counts) (groups ct)
 where
  groups s = map (\k -> everyNth (drop k s)) [0..(n-1)]
  everyNth [] = []
  everyNth (x:xs) = x : everyNth (drop (n-1) xs)
  counts s = [(c, length (filter (== c) s)) | c <- ['A'..'Z'], c `elem` s]
  comp (c1, n1) (c2, n2) = compare n2 n1 <> compare c1 c2


-- ASSESSMENT A3





