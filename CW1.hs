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
maze1 = []
maze2 = [ ((0,0), (3,3)) ]
maze3 = [ ((0,0), (0,3)), ((0,3), (3,3)), ((3,0), (3,3)) ]

renderMaze :: [((Integer, Integer), (Integer, Integer))] -> [String]
renderMaze [] = []
renderMaze m = [[if onAnyPath x y then '#' else ' ' | x <- [0..maxX]] | y <- [0..maxY]]
 where
  -- find min/max bounds
  xs = [x | ((x1, _), (x2, _)) <- m, x <- [x1, x2]]
  ys = [y | ((_, y1), (_, y2)) <- m, y <- [y1, y2]]
  maxX = maximum xs
  maxY = maximum ys
  
  -- check for (x, y) coords on the grid
  onAnyPath x y = any (checkPath x y) m
  checkPath x y ((x1, y1), (x2, y2)) =
   x >= min x1 x2 && x <= max x1 x2 &&
   y >= min y1 y2 && y <= max y1 y2









