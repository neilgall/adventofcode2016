{-# LANGUAGE OverloadedStrings #-}
module Day5 where
    
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Array as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as Hex
import Data.Char (chr, digitToInt)
import Data.List (nubBy)

at :: Int -> BS.ByteString -> Char
at i s = BS.head $ BS.drop i s

hash :: BS.ByteString -> Int -> BS.ByteString
hash d i = MD5.hash (d `BS.append` BS.pack (show i))

password :: (BS.ByteString -> a) -> BS.ByteString -> [a]
password f = map (f . BS.drop 5) . map Hex.encode . filter five0s . hashes
  where
    hashes door = map (hash door) [0..]
    five0s s = at 0 s == (toEnum 0) && at 1 s == (toEnum 0) && at 2 s < (toEnum 16)

part1 :: BS.ByteString -> String
part1 = take 8 . password BS.head

part2 :: BS.ByteString -> String
part2 = order . take 8 . nubBy position . filter validPos . password pick2
  where
    pick2 s = (digitToInt $ BS.head s, BS.head $ BS.tail s)
    validPos p = 0 <= fst p && fst p <= 7
    position p q = fst p == fst q
    order = A.elems . A.array (0,7)
    
main = do
  let input = "ojvtpuvg"
  putStrLn (part1 input)
  putStrLn (part2 "abc")
  putStrLn (part2 input)
