module Golf where


import Data.List

-- вывести список всех i-ый элементов списка,
-- где i <- [1,length списка]
skips :: [a] -> [[a]]
skips []     = []
skips inp = map (flip takeEvery inp) [1..c] where
   takeEvery :: Int -> [a] -> [a]
   takeEvery c inp = [
                       el | (el,pos) <- zip inp [1..],
                            pos `mod` c == 0
                      ]
   c = length inp

-- вывести элементы, которые были
-- бы больше своего левого и правого соседа
localMaxima :: [Integer] -> [Integer]
localMaxima input = [ el | (el,pos) <- zip input [0..],
                            pos > 0, pos < length input - 1,
                            el > (input !! (pos - 1)) &&
                            el > (input !! (pos+1))
                      ]

-- наглядно представить количество
-- цифр [0,9] в входном списке
histogram :: [Int] -> String
histogram list = case list of
     []     -> empty
     (x:xs) -> concat $ statistic list ++ [empty]
   where empty = "==========\n0123456789\n"

statistic :: [Int] -> [String]
statistic [] = []
statistic inp = stars inp : statistic inp' where
   inp' = deleteAll (frequent inp) $ inp

deleteAll :: (Eq a) => [a] -> [a] -> [a]
deleteAll [] from = from
--daleteAll _ [] = []
deleteAll what@(y:ys) from = deleteAll ys $ delete y from

stars :: [Int] -> String
stars []    = []
stars input =
   let xs = frequent input
       min = head $ frequent input
       f x (pos,line) = (x,line') where
          line' = "*" ++ take (pos - 1 - x) (repeat ' ') ++ line
   in take min (repeat ' ') ++ (snd $ foldr f (10,"") xs) ++ "\n"


-- самые частые цифры в массиве
frequent :: [Int] -> [Int]
frequent = sort . fst . foldr f ([],0) . group . sort
         where
           f x (digits, maxLength) =
              case (compare (length x) maxLength) of
                 GT -> ([head x], length x)
                 LT -> (digits, maxLength)
                 EQ -> (head x : digits, maxLength)