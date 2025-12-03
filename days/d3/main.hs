maxElem :: String -> (Char, Int)
maxElem [x] = (x, 0)
maxElem (x:xs) = let (m1, im) = maxElem xs
    in if x >= m1 then (x, 0) else (m1, im + 1)

maxElemInRange :: String -> Int -> Int -> (Char, Int)
maxElemInRange xs s e = do
    let xs' = take (e - s) $ drop s xs
    let (a, b) = maxElem xs'
    (a, b + s)

findNextChar :: Int -> String -> String -> String
findNextChar len acc y
    | length acc == len = acc
    | otherwise = do
        let numChoices = length y
        let curLength = length acc
        let reservedLength = len - curLength - 1
        let (v, i) = maxElemInRange y 0 (numChoices - reservedLength)

        findNextChar len (acc ++ [v]) (drop (i + 1) y)

parseInt :: String -> Int
parseInt x = read x :: Int

p1 :: IO ()
p1 = do
    contents <- getContents
    let c = lines contents
    let ans = sum $ parseInt . solveOneP1 <$> c
    print ans

solveOneP1 :: String -> String
solveOneP1 = findNextChar 2 "" 

solveOneP2 :: String -> String
solveOneP2 = findNextChar 12 ""

p2 :: IO ()
p2 = do
    contents <- getContents
    let c = lines contents
    let ans = sum $ parseInt . solveOneP2 <$> c
    print ans

main :: IO ()
main = p1
