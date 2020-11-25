module Main where
import Data.List
import Lib
import Tree
import Data.Char
import Control.Monad
import Theory


main::IO()
main =  undefined
lister:: Int -> [[Int]]
lister x | x < 0  = error "lister: x is negative"
         | x == 0 = [[]]
         | otherwise = (member x): lister (x-2)
                   where member x = [z |z<-[0..x], z `mod` 2 == 1, z <= x]

sumMemb :: [[Int]] -> [Int]
sumMemb [] = []
sumMemb (x:xs) = (foldl1 (+) x) : sumMemb xs

solve = map even.sumMemb.reverse.lister

magicSquare :: Int -> Int -> [[Int]]
magicSquare x  y | x < 0  || y < 0 = error "magicSquare: some parameter are negative"
                 | x == 0          = [[]]
                 | otherwise       = take x $ (reverse.row $ y) : magicSquare (x-1) y
                           where row 0 = []
                                 row z = z : row (z-1)
cT :: [Int] -> [Int] -> [Int]
cT ann bob  | null ann || null bob = error "cT: some list is empty"
            | otherwise = scaler $ zipWith (\x y -> if x > y
                                                        then 1
                                                    else
                                                        if x == y
                                                           then 0
                                                        else -1 ) ann bob
                        where scaler []     = [0,0]
                              scaler (x:xs) | x == 1  = zipWith (+) [1,0].scaler $ xs
                                            | x == -1 = zipWith (+) [0,1].scaler $ xs
                                            | x == 0  = zipWith (+) [0,0].scaler $ xs

plusMinus :: [Int] -> [Double]
plusMinus [] = error "plusMinus: list is empty"
plusMinus arr =
                let a = filter (\x-> if x > 0 then True else False)  arr
                    b = filter (\x-> if x < 0 then True else False)  arr
                    c = filter (\x-> if x == 0 then True else False) arr
                in map (\x -> (fromIntegral . length $ x) / (fromIntegral . length $ arr)) [a, b, c]

birthdayCakeCandles :: [Int] -> Int
birthdayCakeCandles []  = error "birthdayCakeCandles: list is empty"
birthdayCakeCandles arr @ (x:xs) = length . filter  f $ arr
                                   where f x | x == foldl1 max arr = True
                                             | otherwise = False

timeConversion :: String -> String
timeConversion [] = error "timeConversion: input is empty"
timeConversion input @ (hFd:hSd:_:_:_:_:_:_:half) = output
                       where output | half == "AM" = filter (not.isLetter) input
                                    | half == "PM" = (show $ (12 + (read $ take 2 input)) `mod` 24) ++ (drop 2  $ filter (not.isLetter) input)
                                    | otherwise = error "timeConversion: invalid time format"


filterArray :: Ord a => a -> [a] -> [a]
filterArray _ [] = []
filterArray delimiter (x:xs) | delimiter > x = x : filterArray delimiter xs
                             | otherwise     = filterArray delimiter xs

oddPositionMemberAtList :: [a] -> [a]
oddPositionMemberAtList []     = []
oddPositionMemberAtList array = [members | (members,pos) <- zip array [0..] , odd pos ]

reverse' :: [a] -> [a]
reverse' array = foldl (flip (:)) [] array

{-
   foldl f init (x:xs) = foldl (f init x) xs
   левая свертка 
-}

-- main = interact $ show . f . map read . lines примерный вариант main

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = length' xs + 1

absoluteValue :: (Num a , Ord a) => [a] -> [a]
absoluteValue [] = []
absoluteValue (x:xs) | x < 0     = -x : absoluteValue xs
                     | otherwise =  x : absoluteValue xs

 -- main = interact $ unwords . map show . absoluteValue . map read . lines


pyphs :: Integer -> [(Integer, Integer, Integer)]
pyphs n  = [(x,y,z) | x <- [1..n],
                     let xx = x*x,
                     y <- [x+1..n],
                     gcd x y == 1,
                     let yy = y*y,
                     let isqrt = truncate . sqrt . fromInteger,
                     let z = isqrt $ xx + yy,
                     z*z == xx+yy
           ]

newtype Line = Line [Node]
    deriving (Show, Read, Eq, Ord)

--  позволяет сделать вот так:
--         (read "Line [4,2]")::Line
--          напечатать Line [...]

type Node = Int

linesL :: [Line]
linesL  = [   Line [1,2], 
              Line [2,3],
              Line [3,4],
              Line [4,5],
              Line [1,5], 
              Line [1,6,7,3],
              Line [2,7,8,4], 
              Line [3,8,9,5],
              Line [1,10,9,4],
              Line [2,6,10,5]
           ]

(/\) :: Line -> Line -> Maybe Node
(Line xs) /\ (Line ys) = 
                   let nodes = commonElem xs ys 
                       in case length nodes of
                             1         -> Just (head nodes)
                             otherwise -> Nothing
                                      

                             
commonElem xs = filter $ (flip elem) xs
commonElem' xs ys = [x | x <- xs, elem x ys] -- с помощью абстракции списков

-- список всех треугольников 
triangles :: [Line] -> [[Node]]
triangles lines =  map (map fromJust) $ [ tops | l1 <- lines, l2 <- lines, l3 <-lines,
                                                 l1 < l2, l2 < l3,
                                                 let tops @ [a,b,c] = zipWith (/\) [l1,l2,l3] [l2,l3,l1], 
                                                 and . map (Nothing /=) $ tops,
                                                 and $ zipWith (/=) [a,a,b] [b,c,c] 
                                         ]



-- задача на перестановки чисел [0..9]
-- unicNum :: Integer -> Integer -> [Int] -> [Integer]
unicNum state _ [] = [state]
unicNum state divisior divList = 
                 concat [unicNum state' (divisior + 1) divList' | d <- divList,
                                                                  let state' = state * 10 + d,
                                                                  state' `mod` divisior == 0,
                                                                  let divisior' = divisior + 1,
                                                                  let divList' = [x | x <- divList, x /= d]
                         ]



-- ------------------------------------------------------------------------------------------------------------------
-- задача про шахматы

{- позиции фигур ( белый король       -> wK
                   черный король      -> bK
                   первая белая ладья -> wR1
                   вторая белая ладья -> wR2
                   белый конь         -> wKn )
-}


-- функция, возвращающая список Place, которые бьет фигура
bitsPlace :: Chess -> [(Char, Int)]
bitsPlace figure 
 | rang figure == King = 
   filter inCb $ [(letter, digit) | letter <- [pred $ fst $ place figure .. succ $ fst $ place figure ],
                                    digit  <- [pred $ snd $ place figure .. succ $ snd $ place figure ], 
                                    (letter, digit) /= place figure ]
 | rang figure == Rook = 
   filter inCb $ [(letter, digit) | letter <- ['A' .. 'H'],
                                    let digit = snd $ place figure ] 
              ++ [(letter, digit) | digit <- [1..8],
                                    let letter = fst $ place figure,
                                    (/=) digit $ snd $ place figure ]
 | rang figure == Knight = 
   filter inCb $ [(letter, digit) | letter <- [applyN 2 pred $ fst $ place $ figure, applyN 2 succ $ fst $ place $ figure],
                                    digit  <- [pred $ snd $ place $ figure, succ $ snd $ place $ figure] ]
              ++ [(letter, digit) | letter <- [pred $ fst $ place $ figure, succ $ fst $ place $ figure],
                                    digit  <- [applyN 2 pred $ snd $ place $ figure, applyN 2 succ $ snd $ place $ figure] ]

-- предикат, показывающий, что мы не вышли за пределы доски 

inCb :: (Char, Int) -> Bool
inCb (letter, digit) = letter >= 'A' && letter <= 'H' && digit >= 1 && digit <= 8

wK = Chess White King ('A', 1)

bK = Chess Black King ('D', 4)

-- функция дающая ответ, может ли guarder безнаказанно срубить raider'а
-- P.S. даже если raider под ударом guarder'а, но если он переметстится
-- на позицию raider'a, срубив его, он все так же будет находиться под
-- ударом других фигур, список которых выступает вторым аргументом функции

safePlace ::     Chess 
             -> [Chess]  
             ->  Chess 
             ->  Bool
safePlace raider supporters guarder = 
   notElem (place raider) (bitsPlace guarder) ||                    -- находится ли raider под ударом guarder'а 
        elem (place raider) (concat $ map bitsPlace supporters)     -- защищают ли supporters raider'а


-- поставить мат черному королю на D5, разместив
-- на доске две белые ладьи и имея на A1 белого короля


-- notes: нужно перекрыть возмножность королю бежать куда либо,
--        а бежать он может на одну клетку как под диагонали,
--        так и под прямым углом. То есть нужно перекрыть 8 клеток

-- notes: на вход:  1) кого атаковать, 
--                  2) белый король
--        на выходе:  возможное положение [ладья, ладья, конь]


checkmateToBk :: Chess -> Chess -> [[Chess]] 
checkmateToBk wK bK = 
               [ [wR1, wR2, wKn] | placeWR1 <- desk,                               
                                   all ((placeWR1 /=) . place) [wK, bK],
                                   let wR1 = Chess White Rook placeWR1,

                                   placeWR2 <- desk,
                                   all ((placeWR2 /=) . place) [wK, bK, wR1],
                                   let wR2 = Chess White Rook placeWR2,

                                   placeWR1 < placeWR2,

                                   placeWKn <- desk,
                                   all ((placeWKn /=) . place) [wK, bK, wR1, wR2],
                                   let wKn = Chess White Knight placeWKn,
                                   
                                   safePlace wR1 [wR2, wKn] bK, -- все фигуры не могут быть срублены королем
                                   safePlace wR2 [wR1, wKn] bK,
                                   safePlace wKn [wR1, wR2] bK,
                                   place bK `elem` (concat $ map bitsPlace [wR1, wR2, wKn, wK]), -- условие шаха
                                   all (flip elem $ concat $ map bitsPlace [wR1, wR2, wKn, wK]) $ bitsPlace bK -- условие мата
              ]

desk :: [(Char, Int)]
desk = [(lett, digit) | lett <- ['A'..'H'],
                        digit <- [1..8] ]


-- задача решена, но тратится очень много времени и памяти, так как приходится использовать деструкторы для пар, списков
-- использованы пользовательские типы


-- генератор псевдослучайных чисел по Кнуту
-- можно использовать для создания бесконечного (или конечного из интервала) ефсписка случайных чисел

nextSeed :: Int -> Int
nextSeed n =  
   case test > 0 of
        True  -> test
        False -> test + 2147483647
   where
        test = 48271 * lo - 3399 * hi
        hi = n `div` 44488
        lo = n `mod` 44488 


-- ----------------------------------------------------------------------------------------------------------------

-- задача по Шкреду (Интуит выпуск 72)
-- описываем новый тип двумерного вектора на плоскости


-- неудачная попытка сделать Vector2 экземпляром класса Num
-- оказалось - это было не нужно, так как для векторов неоднозначно определено
-- понятие противоположного, знака вектора, результата функции fromInteger
{-
instance (Num a, Eq a, Ord a) => Num (Vector2 a) where
   Vector (a,b) + Vector (c,d) = Vector (a+c,b+d)
   Vector (a,b) * lambda       = Vector (a*lambda,b*lambda)
   abs (Vector (a,b))          = Vector (abs a, abs b)
   signum (Vector (a,b))
              | a > 0 && b > 0   =  1
              | a == 0 && b == 0 =  0
              | otherwise        = -1
   fromInteger x = Vector (fromInteger x,0)





import Data.List
main :: IO()
main = interact $ unlines . map parseBool . map (flip . uncurry $ isInfixOf) . constructPair. tail . lines

constructPair :: [String] -> [(String, String)]
constructPair [] = []
constructPair (x:y:rest) = (x,y) : constructPair rest

isSubstr :: String -> String -> Bool
isSubstr [] _ = False
isSubstr _ [] = True
isSubstr a @ (x:xs) b = case (and $ zipWith (==) a b) && length a >= length b of
  True  -> True
  False -> isSubstr xs b

parseBool :: Bool -> String
parseBool True  = "YES"
parseBool False = "NO"


import Control.Monad as M
import Data.ByteString as B

main :: IO ()
main = do
  n <- Prelude.read <$> Prelude.getLine
  replicateM_ n $ do
    s <- B.getLine
    w <- B.getLine
    Prelude.putStrLn $ if w `isInfixOf` s then "YES" else "NO"

-}
       
-- коллинеарность векторов через понятие о псевдоскалярном (косом) произведении
-- вектор (a',b') - перпендикулярен вектору (a,b)
(//) :: ( Num a, Eq a) => Vector2 a -> Vector2 a -> Bool
Vector (a,b) // Vector (c,d) =
    Vector (c,d) </> Vector (a', b') == 0 where
            a' = b 
            b' = -a

-- скалярное произведение
(</>) :: (Num a) => Vector2 a -> Vector2 a -> a
Vector (a,b) </> Vector (c,d) = a * c + b * d 

-- список всех точек, образующих множество треугольников
-- для их однозначного определения была введена косоугольная система координат,
-- единичными векторы котороый сонаправлены с двумя сторонами большого треугольника


-- список всех точек в косоугольной системе координат 
points :: [Point]
points = [(x,y) | x <- [0..4], y <- [0..x]]

-- список всех треугольников, образованных этими точками
triangles' :: [Triangle]
triangles' = [(x,y,z) | x <- points, 
                        y <- points, 
                        x > y,
                        z <- points,
                        z > x && z > y]

-- функция создает вектор с концом в (x,y) и началом в (x',y')
createVector :: Point -> Point -> Vector2 Int
createVector (x,y) (x',y') = Vector  (x-x', y-y')

-- функция, определяющая, не вырожден ли треугольник (существует ли?)
isExist :: Triangle -> Bool
isExist (x,y,z) = 
   not . (\[a,b,c] -> a//b || a//c || c//b) $
       zipWith (createVector) [x,x,y] [y,z,z] -- список всех трех векторов треугольника

-- список невырожденных треугольников
exTriangle = filter (isExist) triangles'

-- список вырожденных треугольников
unExTriangle = filter (not . isExist) triangles'


----------------------------------------------------------------------------
separator :: String -> [String]
separator [] = [[]]
separator [x] = [[x]]
separator (x:xs) = [[x]] ++ separator xs

ffd :: [[Int]] -> [Int]
ffd (x:_:rest) = concat rest

inmates :: [[Int]]
inmates = [[1,2],[2,3],[5,9],[9,12],[1,12],[7,18],[1,4],[11,17],[17,22],[22,4]]

-- тесные соседи
-- только того, кто передан в качестве первого параметра
closeNeibs :: [Int] -> [[Int]] -> [[Int]]
closeNeibs pair @ [f,s] =
  (pair :) . 
    filter (\otherPair ->
      (f `elem` otherPair || s `elem` otherPair) && pair /= otherPair)

myF = concat $ map (flip closeNeibs inmates) $ tail .  closeNeibs [1,2]$ inmates

myG = map (flip closeNeibs inmates) $ tail . closeNeibs [1,2] $ inmates


lagrange :: [(Float, Float)] -> Float -> Float
lagrange points x = foldl (\acc (xj,y) -> acc + (y * l xj)) 0 points where
  l :: Float -> Float
  l xj =
     foldl (\acc (xk,_) ->
               if xk == xj then acc
                 else acc * ((x-xk)/(xj-xk))) 1 points


data TreeT a = LeafT a | NodeT a [TreeT a] deriving Show

foldtree :: (b -> a -> b) -> b -> TreeT a -> b
foldtree f ini (LeafT x) = f ini x
foldtree f ini (NodeT x trees) = foldl' f' (f ini x) trees where
  f' ini tree = foldtree f ini tree

words' :: TreeT String
words' = NodeT "c" [NodeT "a" [LeafT "t", LeafT "p"], NodeT "o" [NodeT "o" [LeafT "l"]]]


isSubstr :: String -> String -> Bool
isSubstr [] _ = False
isSubstr a @ (x:xs) b = case and $ zipWith (==) a b of
  True  -> True
  False -> isSubstr xs b 


{-
import Control.Monad

main :: IO
main = do
    _    <- getLine
    list <- getLine
    _    <- getLine
    xs   <- getContents

    mapM (putStrLn . flip solve list) xs
-}

solveSum :: Int -> [Int] -> Int
solveSum s list = case subsetSum [] s (reverse . sort $ list) of
    Just x  -> x
    Nothing -> -1
  where
        subsetSum :: [Int] -> Int -> [Int] -> Maybe Int
        subsetSum accs s []
            | sum accs < s   = Nothing
            | otherwise      = Just (length accs)
        subsetSum accs s (x:xs)
            | sum accs< s  = subsetSum (x:accs) s xs
            | otherwise = Just (length accs)


kvaziMain = do
   n_temp <- getLine
   let n = map read . words $ n_temp :: [Int]
   mapM_ (putStrLn . show) n

socks :: [Int] -> [Int]
socks  = map (\xs -> length xs `div` 2 ) .
      group . sort


test12 = seeLvl "UUDUUDUUDDUDDDDUDDUDDDUUUUUU"

seeLvl :: String -> [Int]
seeLvl = reverse . foldl' (changeLvl) [0] where
    changeLvl :: [Int] -> Char -> [Int]
    changeLvl lvls @ (x:xs) step = case step of
        'U' -> x + 1 : lvls
        'D' -> x - 1 : lvls


{-
def countingValleys(n, s):
    level = valleys = 0
    for step in s:
        level += 1 if step == "U" else -1
        valleys += level == 0 and step == "U"
    return valleys
-}


valleys :: String -> Int -> Int
valleys [] _ = 0
valleys (x:xs) lvl = m + valleys xs lvl'
    where
        lvl'  = lvl + delta
        delta 
            | x == 'U'  = 1
            | otherwise = -1
        m
            | x == 'U' && lvl == -1 = 1
            | otherwise             = 0 
