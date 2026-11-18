import Data.List(nub)

-- data type keywords
type Pos = (Int, Int)
data Side = North | East | South | West deriving (Show, Eq, Ord)
type EdgePos = (Side, Int)
type Atoms = [Pos]
type Interactions = [(EdgePos, Marking)]
data Marking = Absorb | Reflect | Path EdgePos deriving (Show, Eq)

-- challenge 1
calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions n atoms = [interaction n atoms e | e <- entries] 
 where 
  -- return all edge entry points for a grid of size N
  entries = [(s, i) | s <- [North, East, South, West], i <- [1..n]]

-- initial position of ray
start :: Int -> EdgePos -> (Pos, Pos)
start _ (North, i) = ((i, 0), (0, 1))
start n (East, i) = ((n+1, i), (-1, 0))
start n (South, i) = ((i, n+1), (0, -1))
start _ (West, i) = ((0, i), (1, 0))

-- follow ray
trace :: Int -> Atoms -> Pos -> Pos -> Maybe EdgePos
trace n atoms (c, r) (dc, dr)
 | hit ahead = Nothing -- absorb
 | (left && right) || ((left || right) && outside (c, r)) = 
    if outside (c, r) then Just (exit n (c, r)) else move (-dc, -dr) -- reflection
 | left = move (-dr, -dc)
 | right = move (dr, dc)
 | otherwise = move (dc, dr)
 where
  hit x = x `elem` atoms
  ahead = (c + dc, r + dr)
  left = hit (fst ahead + dr, snd ahead + dc)
  right = hit (fst ahead - dr, snd ahead - dc)

  -- true if ray position is outside the grid
  outside (x, y) = x < 1 || y < 1 || x > n || y > n

  -- position movement of the ray
  move (a, b)
   | outside next = Just (exit n next)
   | otherwise = trace n atoms next (a, b)
   where next = (c + a, r + b)

-- convert position outside grid into the edge point where the ray left
exit :: Int -> Pos -> EdgePos
exit n (c, r)
 | r == 0 = (North, c)
 | c == n+1 = (East, r)
 | r == n+1 = (South, c)
 | otherwise = (West, r)

-- label result of ray
marking :: EdgePos -> Maybe EdgePos -> Marking
marking _ Nothing = Absorb
marking e (Just x) | x == e = Reflect
                   | otherwise = Path x

-- move ray from its starting point & pair with its result label
interaction :: Int -> Atoms -> EdgePos -> (EdgePos, Marking)
interaction n atoms e = (e, marking e result)
 where
  (p, d) = start n e
  result = trace n atoms p d


-- challenge 2
solveBB :: Int -> Interactions -> Atoms
solveBB n given = nub (concat [atoms | atoms <- subsets (cells n), agrees n atoms given])

-- check if result reproduces every interaction
agrees :: Int -> Atoms -> Interactions -> Bool
agrees n atoms given = all (\(e, m) -> interaction n atoms e == (e, m)) given

-- positions in grid of size N
cells :: Int -> [Pos]
cells n = [(c, r) | c <- [1..n], r <- [1..n]]

-- find every subset in a list
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:rest) = [x : g | g <- sr] ++ sr
 where sr = subsets rest



