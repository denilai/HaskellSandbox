module ChocolateBar where
import Data.List
import Control.Monad

main :: IO()
main = interact $ show . (\[bars,[d,m]] -> pieces d m bars) . map (map read . words) . tail . lines

pieces :: Int -> Int -> [Int] -> [[Int]]
pieces day month input = {-length . filter (/= []) . -}
  filter (\x -> length x == day) $ foldr f list [] where-- ??
    list = zipWith take [1..length input] $ repeat input
    f :: [Int] -> [[Int]] -> [[Int]]
    f x acc = if length x >= month then foldr g [] x : acc else acc
    g :: Int -> [Int] -> [Int]
    g x acc
        | length acc < month  = x:acc
        | length acc == month = if sum acc == day then acc else []
        | otherwise           = []



pieces' :: Int -> Int -> [Int] -> (Int, [Int])
pieces' day month = foldr f (0,[])  where
    f :: Int -> (Int,[Int]) -> (Int,[Int])
    f x (count, acc)
      | length acc + 1 < month   = (count, x:acc)
      | length acc + 1  == month =
          if sum (x:acc) == day
             then (count + 1 , [x])
          else (count, [x])
      | otherwise = (count, [x])

tuples ::  Int -> [Int] -> [[Int]]
tuples x input = zipWith (\a b -> take x $ drop (length input - x - a ) b) [0..(length input - x)] (repeat input)

answer :: Int -> Int -> [Int] -> Int
answer day month = foldr (\x acc -> if length x == month && sum x == day then acc + 1 else acc) 0 . tuples month

parseNum :: [Char] -> [Int]
parseNum = map (read . charToStr) . filter (/= ' ')

charToStr :: Char -> String
charToStr ch = [ch]



divPairs :: [Int] -> Int -> Int
divPairs ars k = length [(i,j) | i <- [0..length ars-2], j<- [0..length ars-1],
                                 i < j, ((ars !! i) + (ars !! j)) `mod` k == 0]
{-
main :: IO()
main = do
    [[_,k],bill,[b]] <- replicateM 3 getList
    putStr $ maybe "Bon Appetit" show $ bonAppetit k bill b
    -}

getList :: Read a => IO[a]
getList = map read . words <$> getLine

bonAppetit :: Int -> [Int] -> Int -> Maybe Int
bonAppetit  k bill b
   | b > actualPrice = Just (b - actualPrice)
   | otherwise = Nothing
   where actualPrice = sum (excludeNth k bill) `div` 2


excludeNth :: Int -> [a] -> [a]
excludeNth n xs = left ++ tail right where
  (left,right) = splitAt n xs