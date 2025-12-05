import Data.Bool (bool)
import qualified Data.Text as T

split :: String -> (String, String)
split x = do
  let (l, r) = break (=='-') x
  let r' = drop 1 r
  (l, r')

parseInt :: String -> Int
parseInt x = read x :: Int

insertSorted :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
insertSorted [] (a, b) = [(a, b)]
insertSorted ((a', b'):xs) (a, b)
  | a < a' && b < a' = (a, b):(a', b'):xs
  | a < a' && b >= a' && b <= b' = (a, b'):xs
  | a < a' && b > b' = insertSorted xs (a, b)
  | a >= a' && a <= b' && b <= b' = (a', b'):xs
  | a >= a' && a <= b' && b > b' = insertSorted xs (a', b)
  | otherwise = (a', b'):(insertSorted xs (a, b))

checkInRange :: [(Int, Int)] -> Int -> Bool
checkInRange [] _ = False
checkInRange ((a, b):xs) y = do
  a <= y && y <= b || checkInRange xs y

parse1 :: String -> (Int, Int)
parse1 x = do
  let (l, r) = split x
  (parseInt l, parseInt r)

p1 :: IO ()
p1 = do
  contents <- getContents
  let [a, b] = T.splitOn (T.pack "\n\n") (T.pack contents)
  let a' = map T.unpack $ T.lines a
  let b' = map T.unpack $ T.lines b
  let a'' = parse1 <$> a'
  let b'' = parseInt <$> b'
  let x = foldl insertSorted [] a''
  let y = fromEnum <$> (checkInRange x) <$> b''
  let ans = sum y
  print ans

p2 :: IO ()
p2 = do
  contents <- getContents
  let [a, b] = T.splitOn (T.pack "\n\n") (T.pack contents)
  let a' = map T.unpack $ T.lines a
  let b' = map T.unpack $ T.lines b
  let a'' = parse1 <$> a'
  let b'' = parseInt <$> b'
  let x = foldl insertSorted [] a''
  let y = map (\(a, b) -> b - a + 1) x
  let ans = sum y
  print ans

main :: IO ()
main = p2
