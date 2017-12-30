{-# LANGUAGE TemplateHaskell #-}
module Day11 where
    
import Prelude hiding (floor)
import Control.Lens
import Control.Lens.Fold
import Control.Monad
import Data.List (subsequences, intercalate, (\\), nub, sort)

type Element = String

data Equipment
    = Microchip
    | Generator
    deriving (Eq, Ord, Show)

data Item = Item {
    _element :: Element,
    _equipment :: Equipment,
    _floor :: Int
} deriving (Eq)
makeLenses ''Item

data Building = Building {
   _elevator :: Int,
   _items :: [Item],
   _previous :: [[Item]]
} deriving (Eq)
makeLenses ''Building

instance Show Item where
    show (Item el eq _) = el ++ "." ++ show eq

instance Show Building where
    show b = "e" ++ (show $ _elevator b) ++ " " ++ items
      where
        items = intercalate " " $ map fItems floors
        fItems f = "f" ++ (show f) ++ ":" ++ show (itemsOnFloor f b)
    
floors = [1..4]

itemsOnFloor :: Int -> Building -> [Item]
itemsOnFloor n = filter (\i -> (_floor i) == n) . (view items)

elevatorMoves :: Int -> [Int]
elevatorMoves 1 = [2]
elevatorMoves 2 = [1,3]
elevatorMoves 3 = [2,4]
elevatorMoves 4 = [3]

moveItems :: [Item] -> Int -> Building -> Building
moveItems is f = set (items . traverse . filtered (`elem` is) . floor) f

listOf :: Equipment -> Int -> [Item] -> [Element]
listOf e f = map _element . filter match
    where
        match i = _equipment i == e && _floor i == f

friedMicrochips :: [Item] -> Bool
friedMicrochips is = or (map (fried' is) floors)

fried' is f = not (null ms) && not (null $ gs \\ ms)
    where
        ms = listOf Microchip f is  
        gs = listOf Generator f is

recordState :: Building -> Building
recordState b = b { _previous = (_items b):(_previous b) }

previousState :: Building -> Bool
previousState b = (_items b) `elem` (_previous b)

prevent b = guard (not b)

candidateMoves :: Building -> [Building]
candidateMoves b = let e = view elevator b in do
    e' <- elevatorMoves e    
    m <- subsequences $ itemsOnFloor e b
    guard $ length m == 1 || length m == 2
    let b' = moveItems m e' b
    prevent $ previousState b'
    prevent $ friedMicrochips (itemsOnFloor e b')
    prevent $ friedMicrochips (itemsOnFloor e' b')
    return $ set elevator e' (recordState b')

finished :: Building -> Bool
finished (Building _ is _) = all (\i -> (_floor i) == 4) is

assemble :: Building -> [Building]
assemble b = if finished b
             then pure b
             else candidateMoves b >>= assemble
      
