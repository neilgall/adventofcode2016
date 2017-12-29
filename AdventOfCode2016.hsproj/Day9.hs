module Day9 where
    
import Data.Either (rights)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- part 1

compressed :: GenParser Char st String
compressed = fmap concat $ many $ do
    spaces
    r <- marker <|> text
    spaces
    return r

text :: GenParser Char st String
text = spaces >> pure <$> anyChar

marker :: GenParser Char st String
marker = do
    char '('
    len <- integer
    char 'x'
    rep <- integer
    char ')'
    s <- count len anyChar
    return $ concat . replicate rep $ s
    
integer :: GenParser Char st Int
integer = fmap read (many digit)

decompress :: String -> String
decompress = head . rights . pure . parse compressed ""

part1 :: String -> Int
part1 = length . decompress
