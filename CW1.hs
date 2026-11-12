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


-- ASSESSMENT A4
mazeA = []
mazeB = [ ((0,0), (0,3)) ]
mazeC = [ ((0,0), (0,3)), ((0,2), (2,2)) ]
mazeD = [ ((0,0), (0,3)), ((3,5), (5,5)) ]
mazeE = [ ((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)), ((4,0),(4,2)), 
        ((4,2),(5,2)), ((2,1),(2,5)), ((1,5),(4,5)) ]
mazeF = [ ((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)), ((4,0),(4,2)), 
        ((4,2),(5,2)), ((2,1),(2,5)), ((3,5),(5,5)) ]

-- lines with 2 ends
type Path = ((Integer, Integer), (Integer, Integer))

-- check if 2 lines cross each other at any point
intersects :: Path -> Path -> Bool
intersects ((ax1, ay1), (ax2, ay2)) ((bx1, by1), (bx2, by2)) =
 let xOverlap = max (min ax1 ax2) (min bx1 bx2) <= min (max ax1 ax2) (max bx1 bx2)
     yOverlap = max (min ay1 ay2) (min by1 by2) <= min (max ay1 ay2) (max by1 by2)
 in xOverlap && yOverlap

-- check if maze is linked from any to other lines by hopping btwn connected lines
connected :: [Path] -> Bool
connected [] = True
connected (p:ps) = go [p] ps
 where
  go reached rest
   | null new = null rest
   | otherwise = go (reached ++ new) others
   where
    new = filter (\q -> any (intersects q) reached) rest
    others = filter (\q -> not (any (intersects q) reached)) rest




