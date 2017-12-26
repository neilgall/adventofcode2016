module Day5 where
    
import Control.Arrow ((&&&))
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as Hex
import Data.List (sortBy)

hash :: String -> Int -> BS.ByteString
hash d i = MD5.hash $ BS.pack (d ++ show i)

password :: (BS.ByteString -> a) -> String -> [a]
password f =
  map (f . BS.drop 5) . filter five0s . map Hex.encode . filter two0s . hashes
  where
    hashes door = map (hash door) [0..]
    two0s s = BS.take 2 s == BS.replicate 2 '\0'
    five0s s = BS.head (BS.drop 4 s) == '0'

part1 :: String -> String
part1 = take 8 . password BS.head

part2 :: String -> String
part2 = take 8 . map snd . arrange . password pick2
  where
    pick2 = BS.head &&& BS.head . BS.tail
    arrange = sortBy position . filter validPos
    validPos (p,_) = p `elem` "01234567"
    position (p,_) (q,_) = compare p q
    
