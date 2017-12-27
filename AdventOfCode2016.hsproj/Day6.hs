module Day6 where
  
import Data.List (sort, sortBy, group, transpose)
  
pickBy :: (String -> String -> Ordering) -> String -> Char
pickBy f = head . head . sortBy f . group . sort

mostCommon b a = compare (length a) (length b)
    
part1 :: String -> String
part1 = map (pickBy mostCommon) . transpose . lines
  
leastCommon a b = compare (length a) (length b)

part2 :: String -> String
part2 = map (pickBy leastCommon) . transpose . lines
