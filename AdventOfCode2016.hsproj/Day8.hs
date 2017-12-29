module Day8 where
    
import Data.Either (rights)
import Data.List (foldl')
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- screen data structure

setCol :: Int -> V.Vector a -> M.Matrix a -> M.Matrix a
setCol c v m = V.foldl set m (V.indexed v)
  where
    set m (i, x) = M.setElem x (i+1, c) m
    
setRow :: Int -> V.Vector a -> M.Matrix a -> M.Matrix a
setRow r v m = V.foldl set m (V.indexed v)
  where
    set m (i, x) = M.setElem x (r, i+1) m

rotate :: Int -> V.Vector a -> V.Vector a
rotate n v = end V.++ start
  where
    len = V.length v
    end = V.take n $ V.drop (len - n) v
    start = V.take (len - n) v

type Screen = M.Matrix Int

newScreen :: Screen
newScreen = M.zero 6 50

rect :: Int -> Int -> Screen -> Screen
rect a b m = foldl' set m [(r,c) | r <- [1..b], c <- [1..a]]
  where
    set m p = M.setElem 1 p m

rotateRow :: Int -> Int -> Screen -> Screen
rotateRow r d m = setRow (r+1) newRow m
  where
    newRow = rotate d $ M.getRow (r+1) m
    
rotateCol :: Int -> Int -> Screen -> Screen
rotateCol c d m = setCol (c+1) newCol m
  where
     newCol = rotate d $ M.getCol (c+1) m

pixelsLit :: Screen -> Int
pixelsLit = length . filter (==1) . concat . M.toLists

-- parser

data Instruction
    = Rect Int Int
    | RotateCol Int Int
    | RotateRow Int Int
    deriving (Show)
    
parseInstructions :: GenParser Char st [Instruction]
parseInstructions = parseInstruction `sepEndBy` endOfLine

parseInstruction :: GenParser Char st Instruction
parseInstruction = char 'r' >> (parseRect <|> parseRotate)

parseRect :: GenParser Char st Instruction
parseRect = do
    string "ect "
    a <- parseInt
    char 'x'
    b <- parseInt
    return $ Rect a b

parseRotate :: GenParser Char st Instruction
parseRotate = do
    string "otate "
    dir <- string "row y=" <|> string "column x="
    a <- parseInt
    string " by "
    b <- parseInt
    return $ if dir == "row y="
        then RotateRow a b
        else RotateCol a b 
    
parseInt :: GenParser Char st Int
parseInt = fmap read (many digit)

load :: String -> IO [Instruction]
load f = do
    c <- readFile f
    return $ head . rights . pure $ parse parseInstructions f c
    
-- part 1

runInstruction :: Screen -> Instruction -> Screen
runInstruction s (Rect a b) = rect a b s
runInstruction s (RotateCol a b) = rotateCol a b s
runInstruction s (RotateRow a b) = rotateRow a b s

runInstructions :: [Instruction] -> Screen
runInstructions = foldl runInstruction newScreen

part1 :: [Instruction] -> Int
part1 = pixelsLit . runInstructions

-- part 2

part2 = runInstructions
