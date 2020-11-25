module CarNumbers where

import Data.Char

import Data.List
import Control.Monad (forM_)

similarLetAndDig ::  [String]
similarLetAndDig = [ [ch'] ++ trio ++ [ch',ch'] ++ duet | ch <- ['A'..'Z'],x <-[0..9],
                                                          let ch' = toUpper ch
                                                              trio = simDigit x 3
                                                              duet = simDigit x 2] 
    

simDigit :: Int -> Int -> String
simDigit _ 0 = []
simDigit x n = show x ++ simDigit x (n-1)



main :: IO()
main = getLine >>=
    (\n -> forM_ [1..read n] $ const $
        readList' >>=
            (\[a,b,c] -> putStrLn $ catAndMouse a b c ))


readList' :: Read a => IO [a]
readList' = map read . words <$> getLine



catAndMouse :: Int -> Int -> Int -> String
catAndMouse catA catB mouse = case compare (abs (catA - mouse)) (abs (catB - mouse)) of
    LT -> "Cat A"
    GT -> "Cat B"
    EQ -> "Mouse C"


pickingNumbers :: [Int] -> Int
pickingNumbers inp  =
   -- foldr (\x acc -> max acc $ length $ snd $ foldr g (x,[]) inp ) 0 inp
    foldr (\x acc -> max acc $ length $ snd $ t x inp ) 0 inp


t x inp = max high low where
  high = foldr (\x (n,acc) -> if x == n || x + 1 == n then (n,x:acc) else (n,acc)) (x,[]) inp
  low = foldr (\x (n,acc) -> if x == n || x - 1 == n then (n,x:acc) else (n,acc)) (x,[]) inp

--f x inp = max (foldr g (x,[]) inp 

gg x inp = max (foldr g (x,[]) inp) (foldr g' (x,[]) inp)    


g x (n,[]) = if abs (x - n) <=1 then (n,[x]) else (n,[])
g x (n,[y]) = if 1 `criteria` (x:[y]) && x<y then (n,x:[y]) else (n,[y])
g x (n,acc) = if 1 `criteria` (x:acc) then (n,x:acc) else (n,acc)

g' x (n,[]) = if abs (x - n) <=1 then (n,[x]) else (n,[])
g' x (n,[y]) = if 1 `criteria` (x:[y]) && x<y then (n,x:[y]) else (n,[y])
g' x (n,acc) = if 1 `criteria` (x:acc) then (n,x:acc) else (n,acc)

test1 = "9 6 13 16 5 18 4 10 3 19 4 5 8 1 13 10 20 17 15 10 6 10 13 20 18 17 7 10 6 5 16 18 13 20 19 7 16 13 20 17 4 17 8 19 12 7 17 1 18 3 16 4 5 3 15 17 6 17 14 11 11 7 11 6 15 15 12 6 17 19 8 6 13 9 10 19 14 18 7 9 11 16 11 20 4 20 10 7 8 4 2 12 11 8 12 13 19 8 8 5"

testL :: String -> [Int]
testL = map read . words

criteria :: Int -> [Int] -> Bool
criteria bound list = abs (minimum list - maximum list) <= bound

testL2 = testL test2

test2 = "14 18 17 10 9 20 4 13 19 19 8 15 15 17 6 5 15 12 18 2 18 7 20 8 2 8 11 2 16 2 12 9 3 6 9 9 13 7 4 6 19 7 2 4 3 4 14 3 4 9 17 9 4 20 10 16 12 1 16 4 15 15 9 13 6 3 8 4 7 14 16 18 20 11 20 14 20 12 15 4 5 10 10 20 11 18 5 20 13 4 18 1 14 3 20 19 14 2 5 13"