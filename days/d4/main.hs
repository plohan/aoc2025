import Data.Bool (bool)

toInt :: Char -> Int
toInt = bool 0 1 . (== '@')

split :: String -> [String]
split x
  | x == "" = []
  | otherwise = do
    let (l, r) = break (=='\n') x
    let r' = drop 1 r
    [l] ++ split r'

isEnough :: (Int, Int) -> Int
isEnough (x, y)
  -- itself include so plus + 1
  | x == 1 && y < 5 = 1
  | otherwise = 0

printGrid :: [[Int]] -> IO ()
printGrid xs = do
  mapM_ print xs
  putStrLn ""

getSurvive :: [[Int]] -> [[Int]]
getSurvive xs = do
  let surrounding = getSurrounding xs
  let p = zipWith zip xs surrounding
  map (map isEnough) p

getSurrounding :: [[Int]] -> [[Int]]
getSurrounding xs = do
  let l = length xs
  let w = length (xs !! 0)
  map (\i -> map (\j -> getSurroundingOne xs i j) [0..w-1]) [0..l-1]

getSurroundingOne :: [[Int]] -> Int -> Int -> Int
getSurroundingOne xs i j = do
  let d = (,) <$> [i-1..i+1] <*> [j-1..j+1]
  let d' = sum $ map (\(i, j) -> getCheckBound xs i j) d
  d'

getCheckBound :: [[Int]] -> Int -> Int -> Int
getCheckBound xs i j
  | i >= 0 && i < length xs && j >= 0 && j < length (xs !! i) = xs !! i !! j
  | otherwise = 0

getConsistentState :: [[Int]] -> [[Int]]
getConsistentState xs = do
  let xs' = getSurvive xs
  let new = zipWith (zipWith subtract) xs' xs
  if new == xs then xs else getConsistentState new

p1 :: IO ()
p1 = do
  contents <- getContents
  print $ sum $ map sum $ getSurvive $ map (map toInt) $ split $ contents

p2 :: IO ()
p2 = do
  contents <- getContents
  let first = sum $ map sum $ map (map toInt) $ split $ contents
  let final = sum $ map sum $ getConsistentState $ map (map toInt) $ split $ contents
  print $ first - final

main :: IO ()
main = p2
