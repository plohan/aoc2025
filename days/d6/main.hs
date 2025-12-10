import Data.List (transpose)

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen cond = foldr f [[]]
    where f x acc@(y:ys)
            | cond x = [] : acc
            | otherwise = (x:y) : ys

solve1 :: [String] -> Int
solve1 x = do
    let op = last (x !! 0)
    case op of
        '+' -> sum $ map (parseInt . init) x
        '*' -> product $ map (parseInt . init) x

p2 :: IO ()
p2 = do
    contents <- getContents
    let lines = splitWhen (all (' ' ==)) $ transpose $ splitWhen (== '\n') contents
    print $ sum $ map solve1 lines

split :: (Char -> Bool) -> String -> [String]
split _ "" = []
split cond x = do
  let x' = dropWhile cond x
  let (y, ys) = break cond x'
  let z = dropWhile cond ys
  y : split cond z

parseInt :: String -> Int
parseInt x = read x :: Int

p1 :: IO ()
p1 = do
  contents <- getContents
  let lines = transpose $ map (split (== ' ')) $ split (== '\n') contents
  print $ sum $ map doOneP1 lines

doOneP1 :: [String] -> Int
doOneP1 xs = do
  let ys = map parseInt $ init xs
  let op = last xs
  case op of
    "+" -> sum ys
    "*" -> product ys
    _ -> error "Invalid operator"

padZero :: Int -> String -> String
padZero n x = replicate (n - length x) '0' ++ x

removeTrailingZero :: Int -> Int
removeTrailingZero n
  | n `mod` 10 == 0 = removeTrailingZero (n `div` 10)
  | otherwise = n

main :: IO ()
main = p1
