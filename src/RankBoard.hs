module RankBoard where

import Data.List
import Data.Maybe (fromMaybe)

type TourGrid = [(Int, Int)]



getList :: Read a => IO[a]
getList = map read . words <$> getLine

leaderBoard :: [Int] -> [Int]
leaderBoard  =  map head . group . sortBy (flip compare) 

--contest :: Int -> TourGrid -> TourGrid
--contest score grid = leaderBoard $ score : (map fst $ grid)

takenPlace :: Int -> [Int] -> Int -> Int
takenPlace acc [] _ = acc
takenPlace acc (p:ps) score = case compare score p of
    LT -> takenPlace (acc+1) ps score
    _  -> acc
--if score > fst p then acc else takenPlace (acc+1) ps score 

solve :: [Int] -> [Int] -> [Int]
solve ranks  = map (takenPlace 1 lb) where lb = leaderBoard ranks

ranksw = leaderBoard [10,100,100,50,40,40,20,10]