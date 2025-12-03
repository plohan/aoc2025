import qualified Data.Text as T
import qualified Data.Time as Time

doubleString :: String -> String
doubleString x = x ++ x

isDoubleString :: String -> Bool
isDoubleString x = do
    let (l, r) = splitAt (length x `div` 2) x
    l == r

parseRange :: String -> (Int, Int)
parseRange x = do
    let (l', r) = break (== '-') x
    let r' = dropWhile (== '-') r
    let l'' = read l' :: Int
    let r'' = read r' :: Int
    (l'', r'')

isRepeating :: String -> String -> Bool
isRepeating fullstr substr
    | fullstr == substr = True
    | otherwise = do
        let (l, r) = splitAt (length substr) fullstr
        l == substr && isRepeating r substr
  
isNumInvalid :: Int -> Bool
isNumInvalid = isDoubleString . show
  
isNumInvalid2 :: Int -> Bool
isNumInvalid2 x = do
    let x' = show x -- 12
    let l = length x' -- 2
    let ub = l `div` 2
    
    let ds = filter ((==0) . (l `mod`)) [1..ub] -- [2]

    any (\d -> isRepeating x' (take d x')) ds

solveOne :: (Int -> Bool) -> String -> [Int]
solveOne f x = do
    let (l, r) = parseRange x
    filter f [l..r]

solveMany :: (Int -> Bool) -> String -> [Int]
solveMany f x
    | x == "" = []
    | otherwise = do
        let (l, r) = break (== ',') x
        let r' = drop 1 r
        solveOne f l ++ solveMany f r'

p1 :: IO ()
p1 = do
    contents <- getContents
    let ans = solveMany isNumInvalid contents
    print ans

p2 :: IO ()
p2 = do
    contents <- getContents
    let ans = solveMany isNumInvalid2 contents
    print $ sum ans

main :: IO ()
main = p2

-- main = do
    -- print $ isNumInvalid2 113