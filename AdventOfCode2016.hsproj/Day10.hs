{-# LANGUAGE TemplateHaskell #-}
module Day10 where
    
import Control.Lens
import Control.Monad.State
import Data.Either (rights)
import Data.List (delete, sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec hiding (State)

data Target 
  = Bot Int
  | Output Int
  deriving (Show)

data Instruction
  = Value Int Target
  | LoHi Int Target Target
  deriving (Show)

-- parser

parseInstructions :: GenParser Char st [Instruction]
parseInstructions = parseInstruction `sepEndBy` endOfLine

parseInstruction :: GenParser Char st Instruction
parseInstruction = valueInstruction <|> loHiInstruction

valueInstruction :: GenParser Char st Instruction
valueInstruction = do
    string "value "
    v <- integer
    string " goes to "
    t <- target
    pure $ Value v t
    
loHiInstruction :: GenParser Char st Instruction
loHiInstruction = do
    string "bot "
    b <- integer
    string " gives low to "
    lo <- target
    string " and high to "
    hi <- target
    pure $ LoHi b lo hi
 
target :: GenParser Char st Target
target = botTarget <|> outputTarget

botTarget = string "bot " >> (Bot <$> integer)
outputTarget = string "output " >> (Output <$> integer)

integer :: GenParser Char st Int
integer = fmap read (many digit)

loads :: String -> String -> [Instruction]
loads f c = head . rights . pure $ parse parseInstructions f c 

load :: String -> IO [Instruction]
load f = do
    c <- readFile f
    return $ loads f c
    
-- part 1

data Comparison = Comparison {
  _bot :: Int,
  _chips :: [Int]
} deriving (Show)
makeLenses ''Comparison

data Factory = Factory {
  _bots :: M.Map Int [Int],
  _outputs :: M.Map Int [Int],
  _comparisons :: [Comparison]
} deriving (Show)
makeLenses ''Factory

initial :: [Instruction] -> Factory
initial [] = Factory {
  _bots = M.empty,
  _outputs = M.empty,
  _comparisons = []
}

initial (Value v (Bot b):is) = input b v (initial is)
  where
    input b v = over bots (M.alter (give' v) b)

initial (_:is) = initial is

step :: [Instruction] -> State Factory ()
step is = mapM doInstruction is >> return ()

doInstruction :: Instruction -> State Factory ()
doInstruction (Value b t) = return ()
doInstruction (LoHi b lo hi) = doLoHiInstruction b lo hi

doLoHiInstruction :: Int -> Target -> Target -> State Factory ()
doLoHiInstruction b lo hi = do
    ts <- gets $ sort . fromMaybe [] . view (bots . at b)
    when (length ts == 2) $ do
      give (ts !! 0) lo
      give (ts !! 1) hi
      modify $ over comparisons (cons $ Comparison b ts)
      modify $ over bots (M.insert b [])

give :: Int -> Target -> State Factory ()
give v (Bot b) = modify $ over bots (M.alter (give' v) b)
give v (Output o) = modify $ over outputs (M.alter (give' v) o)

give' :: Int -> Maybe [Int] -> Maybe [Int]
give' v vs = case vs of
    Nothing -> Just [v]
    Just vs' -> Just (v:vs')
    
remove :: Int -> Int -> State Factory ()
remove v b = modify $ over bots (M.alter take' b)
  where
    take' ts = case ts of 
      Nothing -> Nothing 
      Just ts' -> Just $ delete v ts'

findComparison :: Int -> Int -> Factory -> Maybe Comparison
findComparison x y f = find' (view comparisons f)
  where
    find' [] = Nothing
    find' (c@(Comparison b ts):cs) =
      if x `elem` ts && y `elem` ts then Just c else find' cs

runUntil :: (Factory -> Bool) -> [Instruction] -> Factory
runUntil stop instructions = run' (initial instructions)
  where
    run' f = let f' = execState (step instructions) f in
      if (stop f') then f' else run' f'

part1 = do
    instructions <- load "day10.txt"
    let result = runUntil foundComparison instructions
    return (findComparison 61 17 result, result)
  where
    foundComparison f = case (findComparison 61 17 f) of
        Nothing -> False
        Just _ -> True

part2 :: IO Int
part2 = do
    instructions <- load "day10.txt"
    let result = runUntil outputsFilled instructions
    let os = map (getOutput result) [0..2]
    return $ product os
  where
    getOutput f n = head . fromJust $ view (outputs . at n) f
    outputsFilled f = all (filled f) [0..2]
    filled f n = case (view (outputs . at n) f) of
        Nothing -> False
        Just _ -> True
    
