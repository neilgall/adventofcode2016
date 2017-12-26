module Day2 where
  
import Data.List (foldl', scanl')
import qualified Data.Map as M

-- part 1
  
move :: Int -> Char -> Int
move n 'U' = if n > 3 then n - 3 else n
move n 'D' = if n < 7 then n + 3 else n
move n 'L' = if n `elem` [1, 4, 7] then n else n - 1
move n 'R' = if n `elem` [3, 6, 9] then n else n + 1

code :: (a -> Char -> a) -> a -> [String] -> [a]
code f start = tail . scanl' (foldl' f) start

part1 :: String -> String
part1 = concat . map show . (code move 5) . lines

-- part 2

moves :: M.Map [Char] Char
moves = M.fromList $ map (\s -> (init s, last s))
        [               "1D3"
        ,               "2D6", "2R3"
        , "3U1", "3L2", "3D7", "3R4"
        ,        "4L3", "4D8"
        ,                      "5R6"
        , "6U2", "6L5", "6DA", "6R7"
        , "7U3", "7L6", "7DB", "7R8"
        , "8U4", "8L7", "8DC", "8R9"
        ,        "9L8"
        , "AU6",               "ARB"
        , "BU7", "BLA", "BDD", "BRC"
        , "CU8", "CLB"
        , "DUB"]

move2 :: Char -> Char -> Char
move2 c m = M.findWithDefault c (c:m:[]) moves

part2 = code move2 '5' . lines
