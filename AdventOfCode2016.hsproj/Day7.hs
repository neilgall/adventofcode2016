module Day7 where
  
import Data.List
import Data.List.Split

-- part 1
  
tls :: String -> Bool
tls s = abba s && (not . any abba $ bracketed s)

abba :: String -> Bool
abba s
  | length s < 4 = False
  | otherwise = case s of
      (a:b:c:d:r) -> (a /= b && a == d && b == c) || abba (b:c:d:r)

bracketed :: String -> [String]
bracketed [] = []
bracketed (c:cs)
  | c == '[' = (takeWhile notClose cs) : (bracketed $ dropWhile notClose cs)
  | otherwise = bracketed cs
  where
    notClose = (/= ']')
    
part1 :: String -> Int
part1 = length . filter tls . lines

-- part 2

ssl :: String -> Bool
ssl s =
  let
    (outside, inside) = parts s
    abas = outside >>= aba
    babs = inside >>= aba
  in
    any (\a -> invert a `elem` babs) abas
   

parts :: String -> ([String], [String])
parts = pairUp . splitOneOf "[]"
  where
    pairUp [] = ([], [])
    pairUp (s:[]) = ([s], [])
    pairUp (s:t:rs) = let p = pairUp rs in (s:fst p, t:snd p)

aba :: String -> [String]
aba s
  | length s < 3 = []
  | otherwise = case s of
      (a:b:c:r) -> if (a /= b && a == c) then [a,b,c]:aba (b:c:r) else aba (b:c:r)

invert :: String -> String
invert (a:b:_) = [b,a,b]

part2 :: String -> Int
part2 = length . filter ssl . lines
