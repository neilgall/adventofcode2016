module Day9b where
    
import Data.Either (rights)
import Text.Parsec
import Text.Parsec.Char
import Text.ParserCombinators.Parsec hiding (runParser)

-- part 2

type Multiplier = (Integer, Integer)
type ParseState = [Multiplier]

consume :: Integer -> ParseState -> ParseState
consume _ [] = []
consume i ((l,r):ms)
    | i >= l = consume i ms
    | otherwise = (l-i,r):consume i ms
    
multiplier :: ParseState -> Integer
multiplier [] = 1
multiplier ((_,m):ms) = m * multiplier ms

compressed :: GenParser Char ParseState Integer
compressed = fmap sum $ many $ do
    spaces
    r <- marker <|> text
    spaces
    return r

text :: GenParser Char ParseState Integer
text = do
    anyChar
    s <- getState
    modifyState (consume 1)
    return (multiplier s)

marker :: GenParser Char ParseState Integer
marker = do
    char '('
    len <- integer
    char 'x'
    rep <- integer
    char ')'
    modifyState (consume 3)
    modifyState (\ms -> (len, rep):ms)
    return 0
    
integer :: GenParser Char ParseState Integer
integer = do
    d <- many digit
    modifyState (consume . toInteger $ length d)
    return $ read d

decompress :: String -> Integer
decompress = head . rights . pure . runParser compressed [] ""

part2 :: String -> Integer
part2 = decompress

