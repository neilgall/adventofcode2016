module Day3 where
    
import Data.List (permutations, transpose)
import Data.List.Split (chunksOf)

-- part 1

parse1 :: String -> [[Int]]
parse1 = map (map read . words) . lines

triangle :: [Int] -> Bool
triangle = all (\(a:b:c:_) -> a + b > c) . permutations

triangles :: [[Int]] -> Int
triangles = length . filter id . map triangle

part1 :: String -> Int
part1 = triangles . parse1

-- part 2

parse2 :: String -> [[Int]]
parse2 = concat . map transpose . chunksOf 3 . parse1

part2 :: String -> Int
part2 = triangles . parse2