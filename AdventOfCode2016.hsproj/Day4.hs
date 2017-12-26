module Day4 where
    
import Control.Arrow ((&&&))
import Data.Char (isLetter, ord, chr)
import Data.Either (rights)
import Data.List (intersperse, sort, sortBy, group, isInfixOf)
import qualified Data.Map as M
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

type Name = String
type Sector = Int
type Checksum = String

data Room = Room {
    _name :: Name,
    _sector :: Sector,
    _checksum :: Checksum
  } deriving (Show)

parseRooms :: GenParser Char st [Room]
parseRooms = parseRoom `sepEndBy` endOfLine

parseRoom :: GenParser Char st Room
parseRoom = Room <$> parseName <*> parseSector <*> parseChecksum

parseName :: GenParser Char st Name
parseName = fmap init $ many (letter <|> char '-')

parseSector :: GenParser Char st Sector
parseSector = fmap read (many digit)

parseChecksum :: GenParser Char st Checksum
parseChecksum = do
    char '['
    c <- many letter
    char ']'
    return c
    
rooms :: String -> [Room]
rooms = head . rights . pure . parse parseRooms ""

-- part 1

realRoom :: Room -> Bool
realRoom = verify . (checksum . _name &&& _checksum)
  where
    verify = uncurry (==)

longer :: [a] -> [a] -> Ordering
longer b a = compare (length a) (length b)

checksum :: String -> String
checksum = take 5 . map head . sortBy longer . group . sort . filter isLetter 

part1 :: String -> Int
part1 = sum . map _sector . filter realRoom . rooms

-- part 2

shift :: Int -> Char -> Char
shift n c
  | isLetter c = chr $ ((ord c - ord 'a') + n) `mod` 26 + ord 'a'
  | otherwise = c

decipher :: Room -> Room
decipher (Room _name _sector _checksum) = Room name _sector _checksum
  where
    name = map (shift _sector) _name
    
isNorthPoleStorage :: Room -> Bool
isNorthPoleStorage (Room _name _ _) = "north" `isInfixOf` _name

part2 :: String -> Int
part2 = _sector . head . filter isNorthPoleStorage . map decipher . rooms
