-- ASSESSMENT A1
import Data.Char (toUpper, ord, chr)

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
