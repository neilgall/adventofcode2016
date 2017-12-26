module Day1 where
  
import Data.List (foldl', scanl', concatMap, nub)
import Data.List.Split (splitOn)

data Dir = N | S | E | W deriving (Eq, Show)
data Turn = R | L | Z deriving (Eq, Show, Read)
type Step = (Turn, Int)

parse :: String -> [Step]
parse = map (\(c:cs) -> (read [c], read cs)) . splitOn ", "

turn :: Turn -> Dir -> Dir
turn L N = W
turn L S = E
turn L E = N
turn L W = S
turn R N = E
turn R S = W
turn R E = S
turn R W = N
turn Z d = d

type Pos = (Int, Int)

move :: Dir -> Int -> Pos -> Pos
move N d (x, y) = (x, y+d)
move S d (x, y) = (x, y-d)
move E d (x, y) = (x+d, y)
move W d (x, y) = (x-d, y)

-- part 1

start :: (Dir, Pos)
start = (N, (0, 0))

follow :: [Step] -> Pos
follow = snd . foldl' step start

step :: (Dir, Pos) -> Step -> (Dir, Pos)
step (dir, pos) (t, d) = (newDir, newPos)
  where
    newDir = turn t dir
    newPos = move newDir d pos
   
manhatten :: Pos -> Int
manhatten (x, y) = abs x + abs y

part1 :: String -> Int
part1 = manhatten . follow . parse

-- part 2

expand :: [Step] -> [Step]
expand = concatMap (\(t, d) -> (t, 1):replicate (d-1) (Z, 1))

trail :: [Step] -> [Pos]
trail = map snd . scanl' step start

duplicates :: [Pos] -> [Pos]
duplicates [] = []
duplicates (p:ps) = nub [q | q <- ps, q == p] ++ duplicates ps

part2 :: String -> Int
part2 = manhatten . head . duplicates . trail . expand . parse
