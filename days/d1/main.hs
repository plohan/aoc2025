import Text.Printf

toNum :: String -> Int
toNum x
    | ('L':xs) <- x = negate $ read xs :: Int
    | ('R':xs) <- x = read xs :: Int

-- convert -342 to (-42, 3)
-- convert 521 to (21, 5)
-- convert -21 to (-21, 0)
-- convert 54 to (54, 0)
toInRange :: Int -> (Int, Int)
toInRange x =
    let y = abs x
        z = y `div` 100 
    in (x + (if x < 0 then 1 else -1) * z * 100, z)

correct :: Int -> (Int, Int)
correct x
    | x == 0 = (0, 1)
    | x < 0 = (x + 100, 1)
    | x >= 100 = (x - 100, 1)
    | otherwise = (x, 0)

step :: (Int, Int) -> (Int, Int) -> (Int, Int)
step (s, ans) (diff, left)
    | 0 == s = (if diff > 0 then diff else 100 + diff, ans + left)
    | otherwise = let (s', ans') = correct (s + diff) in (s', ans' + ans + left)

p2 :: IO ()
p2 = do
    contents <- getContents
    let x = map (toInRange . toNum) $ lines contents
    let y = foldl step (50, 0) x

    print $ snd y

main :: IO ()
main = p2
