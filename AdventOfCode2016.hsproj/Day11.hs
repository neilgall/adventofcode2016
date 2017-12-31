{-# LANGUAGE TemplateHaskell #-}
module Day11 where
    
import Prelude hiding (floor)
import Control.Lens
import Control.Monad
import Data.List
import Data.Maybe (listToMaybe, catMaybes)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U

type Element = String

data Equipment
    = Microchip
    | Generator
    deriving (Eq, Ord, Show)

data Item = Item {
    _element :: !Element,
    _equipment :: !Equipment,
    _floor :: !Int
} deriving (Eq, Ord)
makeLenses ''Item

type Items = S.Set Item

data Building = Building {
   _elevator :: !Int,
   _items    :: !Items,
   _previous :: [Items],
   _steps    :: !Int
} deriving (Eq)
makeLenses ''Building

instance Show Item where
    show (Item el eq f) = el ++ "." ++ show eq ++ "." ++ (show f)

instance Show Building where
    show b = intercalate " " [elevator, items, steps] 
      where
        elevator = "e" ++ (show $ _elevator b)
        steps = "(" ++ (show $ _steps b) ++ ")"
        items = intercalate " " $ map fItems floors
        fItems f = "f" ++ (show f) ++ ":" ++ show (itemsOnFloor f b)
    
floors = [1..4]

itemsOnFloor :: Int -> Building -> Items
itemsOnFloor n = S.filter (\i -> (_floor i) == n) . _items

elevatorMoves :: Int -> [Int]
elevatorMoves 1 = [2]
elevatorMoves 2 = [1, 3]
elevatorMoves 3 = [2, 4]
elevatorMoves 4 = [3]

moveItems :: [Item] -> Int -> Building -> Building
moveItems is f = over items (S.fromList . setFloors . S.toList)
    where
        setFloors = set (traverse . filtered (`elem` is) . floor) f 

typesOf :: Equipment -> Items -> S.Set Element
typesOf e = S.map _element . S.filter match
    where
        match i = _equipment i == e

notnull = not . null

friedMicrochips :: Items -> Bool
friedMicrochips is = notnull (ms S.\\ gs) && notnull gs
    where
        ms = typesOf Microchip is  
        gs = typesOf Generator is
        
pairingScore :: [Item] -> Int
pairingScore is = sum scores
    where
        pairs = groupBy sameElement is
        sameElement a b = (_element a) == (_element b)
        scorePair (a:b:_) = let d = abs (_floor a - _floor b) in 9 - d * d
        scores = map scorePair pairs

floorScore :: [Item] -> Int
floorScore is = sum $ map _floor is

distanceToGoal :: Building -> Int
distanceToGoal = (0-) . dist . S.toList . _items
    where     
        dist is = pairingScore is + floorScore is
        
recordState :: Building -> Building
recordState b = over previous ((_items b):) b

previousState :: Building -> Bool
previousState b = (_items b) `elem` (_previous b)

oneOrTwoFrom :: S.Set a -> [[a]]
oneOrTwoFrom = filter oneOrTwo . subsequences . S.toList
    where
        oneOrTwo s = length s == 1 || length s == 2

prevent b = guard (not b)

candidateMoves :: Building -> [Building]
candidateMoves b = do
    let e = _elevator b
    e' <- elevatorMoves e    
    m <- oneOrTwoFrom $ itemsOnFloor e b
    let b' = moveItems m e' b
    prevent $ previousState b'
    prevent $ friedMicrochips (itemsOnFloor e b')
    prevent $ friedMicrochips (itemsOnFloor e' b')
    return $ set elevator e' . over steps (+1) $ recordState b'

finished :: Building -> Bool
finished = all (\i -> (_floor i) == 4) . _items

assemble :: Int -> Building -> Maybe Building
assemble limit = listToMaybe . assemble' limit . pure
                
assemble' :: Int -> [Building] -> [Building]
assemble' limit bs =
    let
        bs' = best $ concatMap candidateMoves bs
        best = take limit . sortOn distanceToGoal . nubBy same
        same a b = _items a == _items b
        f = sortOn ((0-) . _steps) $ filter finished bs'
    in
        if null bs' then []
        else if null f then assemble' limit bs'
        else f
      
testInitial = Building {
    _elevator = 1,
    _items = S.fromList [
        Item "Hydrogen" Microchip 1,
        Item "Lithium" Microchip 1,
        Item "Hydrogen" Generator 2,
        Item "Lithium" Generator 3 ],
    _previous = [],
    _steps = 0 }

initial = Building {
    _elevator = 1,
    _items = S.fromList [
        Item "Promethium" Microchip 1,
        Item "Promethium" Generator 1,
        Item "Cobalt" Generator 2,
        Item "Curium" Generator 2,
        Item "Ruthenium" Generator 2,
        Item "Plutonium" Generator 2,
        Item "Cobalt" Microchip 3,
        Item "Curium" Microchip 3,
        Item "Ruthenium" Microchip 3,
        Item "Plutonium" Microchip 3 ],
    _previous = [],
    _steps = 0 }
    
scan :: Building -> IO ()
scan b = do
    results <- forM [1..100] $
        \i -> case assemble i b of
            (Just r) -> do
                putStrLn $ (show i) ++ ".. " ++ (show $ _steps r)
                return $ Just (_steps r)
            Nothing ->
                return Nothing
    putStrLn . show . minimum . catMaybes $ results

--main = do
--    scan testInitial
--    scan initial

main = 
    putStrLn . show . fmap _steps $ assemble 100 initial
    
