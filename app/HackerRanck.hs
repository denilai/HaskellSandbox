{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}



module HackerRanck where
import Lib
import Data.List
import Control.Monad

mm = undefined

{-
Примеры кода с hackerrang:

Вывести n раз строку "Hello World"
main :: IO()
main = readLn >>= hello_worlds

hello_worlds n = mapM_ putStrLn $ replicate n "Hello World"

Каждый символ в списке Int повторить n раз 
f :: Int -> [Int] -> [Int]
f n arr =  concat [replicate n x | x <- arr] 

-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words


-- отфильтровать числа в списке
f :: [Int] -> [Int]
f (n:arr) = [ x | x <- arr, x < n]

-- The Input/Output section. You do not need to change or modify this part
main = getContents >>=
          mapM_   putStrLn . map show . f . map read . words

 или utput section. You do not need to change or modify this part
main = getContents >>=
          mapM_   print . (\(n:arr) -> f n arr) . map read . words


-- отфильтровать нечетные числа
f :: [Int] -> [Int]
f list  = [x | (x, pos) <- zip list [1..], not . odd $ pos ]

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main :: IO ()
main = getContents >>=
          mapM_ print . f . map read . words


-- взять модуль от всех чисел
f = map abs 

-- This section handles the Input/Output and can be used as it is. Do not modify it.
main = do
	inputdata <- getContents
	mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata


-- разложить эксп в ряд тейлора до 10 сумм
module Main where

exp_line x = sum [ (x**n)/fac n | n <- [0..9] ]



main :: IO()
main = getContents >>=
      mapM_ print . map exp_line . tail . map read. words


-- найти периметр фигуры 
(на вход строки с абсциссой и ординатой точки n-угольника)

main :: IO()
main  =  interact $ show . perimeter . adder . map (map read ) . map words . tail . lines

--perimeter :: (Num a, Floating a, Integral a, Show b) => [[a]] -> b
adder list = [last list] ++ list 
perimeter :: ( Integral a, Floating b) => [[a]] -> b
perimeter input = sum [ side (x,y) (x',y') | ([x,y],a) <- zip input [0..],                                                            ([x',y'],b) <- zip input [0..],                                                          a == b - 1 ] where
  side (x,y) (x',y') = sqrt (fromIntegral (x - x') ^ 2 + fromIntegral (y - y') ^ 2)


-- найти площадь фигуры
(на вход строки с абсциссой и ординатой точки n-угольника)
main :: IO()
main = interact $ show . area . prepanation . map (map read) .map words .  tail . lines

type Point = (Int, Int)
type Side  = (Point, Point)

closedFigure :: [[Int]] -> [[Int]] 
closedFigure lines = [last lines] ++ lines

-- подготовка входных данных
prepanation :: [[Int]] -> [Side]
prepanation = zipSides . constructPair . closedFigure

-- стоит знак минус у функции sum,
потому что я изменил направление обхода фигуры,
добавивв начало списка точек функцией closedFigure
последнюю точку. Обход изменился с против- на по-часовой стрелке
area :: [Side] -> Double
area input =
   - sum [ trapeze a b | (a,b) <- input ]

trapeze :: (Fractional a) => Point -> Point -> a
trapeze (x,y) (x',y') = rectangle + triangle where
    rectangle = fromIntegral y * fromIntegral (x' - x)
    triangle  = (/2) $ fromIntegral (y' - y) * fromIntegral (x' - x)

constructPair :: [[a]] -> [(a,a)]
constructPair = map (\[x,y] -> (x,y))

zipSides :: [Point] -> [Side]
zipSides list @ (x:xs) = zip list xs




main :: IO()
main = getContents >>=
    preparations . lines

newtype Grid  = Grid [String]
newtype Words = Words [String]

preparations :: [String] -> (Grid, Words)
preparations input = (grid, words) where

    grid  = Grid . sortBy lengthSort . take 10 $ input,
    words = Words . makeWords . last $ input

    makeWords :: String -> [String]
    makeWords [] = []
    makeWords line = map (takeWhile (/= ';')) $ word : makeWords tail where
        (word,tail) = splitAt posSep line
        posSep = case (find (==';') line) of
           Just _  -> head [ pos + 1 | (char, pos) <- zip line [0..],
                                        char == ';']
           Nothing -> length line

-- последовательность Фибоначчи
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- вывести треогульник Паскаля
main :: IO ()
main = getLine >>=
    mapM_ putStrLn . map prettyStr . pascalTr . (read :: String -> Int)


pascalTr :: Int -> [[Int]]
calTr k = [[el n r | r<-[0..n]] | n<-[0..k-1]] where
    el row col = (fac row) `div` ((fac col) * fac (row-col))

fac n = product [1..n]

prettyStr :: [Int] -> String
prettyStr  = 
    reverse . tail . reverse . concat .map (\x -> x++ " ") . map (show:: Int -> String)




import Text.Printf
import Data.List (sortBy)
import Data.Function (on)

type Point = (Int,Int)

main:: IO()
main = interact $
    ((printf "%.1f\n") :: Double -> String) . 
        perimetr . solve .  map (map (read :: String -> Double)) .   
            map words . tail . lines

solve = gScan . closedFigure . map constructPair

constructPair [a,b] = Pt (a,b)
constructPair [] = Pt (0,0)

perimetr :: [Pt] -> Double
perimetr [] = 0
perimetr (a:b:rest) = lengthSide a b + perimetr (b:rest)

lengthSide (Pt (x,y)) (Pt (x',y')) = sqrt ( (x - x') ^ 2 +  (y - y') ^ 2)

data Direction = LEFT | RIGHT | STRAIGHT
               deriving (Show, Eq)

data Pt = Pt (Double, Double)
        deriving (Show, Eq, Ord)

isTurned :: Pt -> Pt -> Pt -> Direction
isTurned (Pt (ax, ay)) (Pt (bx, by)) (Pt (cx, cy)) = case sign of
    EQ -> STRAIGHT
    LT -> RIGHT
    GT -> LEFT
    where sign = compare ((bx - ax) * (cy - ay)) ((cx - ax) * (by - ay))

closedFigure input = last input : input

gScan :: [Pt] -> [Pt]
gScan pts 
    | length pts >= 3 = scan [pt0] rests
    | otherwise       = pts
    where 
        -- Find the most bottom-left point pt0
        pt0 = foldr bottomLeft (Pt (1/0, 1/0)) pts where
            bottomLeft pa pb = case ord of
                               LT -> pa
                               GT -> pb
                               EQ -> pa
                       where ord = (compare `on` (\ (Pt (x, y)) -> (y, x))) pa pb

        -- Sort other points based on angle
        rests = tail (sortBy (compare `on` compkey pt0) pts) where
            compkey (Pt (x0, y0)) (Pt (x, y)) = (atan2 (y - y0) (x - x0),
                                       abs (x - x0))

        -- Scan the points to find out convex
        -- -- handle the case that all points are collinear
        scan [p0] (p1:ps)
            | isTurned pz p0 p1 == STRAIGHT = [pz, p0]
            where pz = last ps

        scan (x:xs) (y:z:rsts) = case isTurned x y z of
            RIGHT    -> scan xs (x:z:rsts)
            STRAIGHT -> scan (x:xs) (z:rsts) -- skip collinear points
            LEFT     -> scan (y:x:xs) (z:rsts)

        scan xs [z] = z : xs


-}

{-
solve l r a b = area l r a b  : volume l r a b : [] where
    area l r a b = sum [ f | x <- [l, l+0.001 .. r],
                            --let f = sum $ zipWith (*) a $ zipWith (^^) [x,x..] b
                             let f = sum $ zipWith (*) a (zipWith (^) [x,x..] b)
                       ]
    volume l r a b = sum [ pi* f * f | x <- [l, l+0.001 .. r],
                               let f = sum $ zipWith (*) a (zipWith (^) [x,x..] b)
                         ]

It works with most input format of HackerRank:

First read and parse an integer (n_temp and n)
For n times read another number from input called q
and read the next q lines with getMultipleLines.
It returns a list of q lines as strings.
To parse the numbers inside a single line, use
list comprehension to iterate on the lines and the function
"words" to split the values. This code works if every
line contains exactly two integers and put them in a list
of type [(Int, Int)]


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \boh  -> do
        q_temp <- getLine
        let q = read q_temp :: Int
        rawInput <- getMultipleLines q
        let input = [(read (words str !! 0) :: Int, read (words str !! 1) :: Int ) | str <- rawInput]
        -- here starts your code


getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret



-- незаконченный пример с convex hull

type Point = (Int,Int)

import Text.Printf

type Point = (Int,Int)

main:: IO()
main = interact $
    ((printf "%.1f\n") :: Double -> String) . 
        perimetr . solve .  map (map (read :: String -> Int)) .   
            map words . tail . lines

solve :: [[Int]] -> Double
solve = uncurry scanGraham . lowestXY . map constructPair

lowestXY :: [Point] -> ([Point], Point)
lowestXY input = (input, bottom) where
   bottom = (xMin,yMin)
   yMin = minAxis snd input
   xMin = minAxis fst ysMin
   ysMin = filter (\(x,y) -> y == yMin) input

minAxis :: (Point -> Int) -> [Point] -> Int
minAxis f [p] = f p
minAxis f (p:ps) = f p `min` minAxis f ps


constructPair :: [Int] -> Point
construstPair [x,y] = (x,y)

scanGraham :: [[Int]] -> [[Int]] -> [[Int]]
scanGraham acc input = 


-- посчиать разность сумм на диагоналях матрицы
main :: IO()
main = interact $ show . solve . map (map read :: [String] -> [Int]) . map words . tail . lines

solve :: [[Int]] -> Int
solve matrix = abs (sum mainDiagonal - sum secondaryDiagonal) where
    mainDiagonal = [matrix !! row !! col | row <- [0..length matrix - 1], let col = row]
    secondaryDiagonal = [matrix !! row !! col | row <- reverse [0..length matrix - 1],
                                                let col = length matrix - 1 - row]


-- вывести отношения количества положительных,
-- отрицательных и нулевых элементов к длинне списка

import Text.Printf (printf)
main :: IO()
main = getContents >>=
    mapM_ (putStrLn . printf "%.6f" ). solve .
        map (read:: String -> Int) . words . last . lines

solve :: [Int] -> [Double]
solve input = 
    [countPos / fracLength, countNeg / fracLength, countZero / fracLength] where 
        countPos  = fL . filter (> 0) $ input
        countNeg  = fL . filter (< 0) $ input
        countZero = fL . filter (== 0)$ input
        fracLength = fL input
        fL = fromIntegral . length

-- вывести лестницу из "#"
import Data.List
main :: IO()
main = interact $ unlines . sraircase . (read::String -> Int)

sraircase :: Int -> [String]
sraircase n = [hashLine row | row <- [1..n],
                              let hashLine row = (replicate (n - row) ' ') ++
                                    replicate row '#']



-- посчитать количество букв, которые нужно удалить
-- из двух слов, чтобы получить анаграмму
main :: IO()
main = interact $ show . (\[x,y] -> makeAnagram x y) . lines

makeAnagram :: String -> String -> Int
makeAnagram a b = deletedLetters where

    countOfCommon :: String -> String -> Int ->  Int
    countOfCommon [] _ c = c
    countOfCommon _ [] c = c
    countOfCommon a@(x:xs) b c = if x `elem` b then
        countOfCommon xs (x `delete` b) c+1 else
            countOfCommon xs b c

    deletedLetters = sum $ (flip (-) $ countOfCommon a b 0) <$> length <$> [a,b]


-- посчитать, сколько нужно удалить букв,
-- чтобы в входном слове не было идущих
-- друг за другом (последовательных)
-- букв
main :: IO()
main = do
    n <- readLn :: IO Int
    forM_ [1..n] $ const $ do
        curTest <- getLine
        putStrLn $ show $ countOfDeleted curTest 0


countOfDeleted :: String -> Int -> Int
countOfDeleted [x] c = c
countOfDeleted (x:y:rest) c = countOfDeleted (y:rest) c' where
    c' = if x == y then c+1 else c



-- конвертировать вермя
import Data.List

main:: IO()
main = interact $ convertTime


convertTime :: String -> String
convertTime (hf:hs:_:mf:ms:_:sf:ss:l1:l2:[]) =
    hour ++ ":" ++ minut ++ ":" ++ second where
        minut  = mf:ms:[]
        second = sf:ss:[]
        hour = case (l1:l2:[]) of
            "PM"      -> show . (+ 12) . read $ hf:hs:[]
            otherwise -> hf:hs:[]

-- посчитать количество раз, когда бился худший и лучший рекорд

import Data.List

main :: IO()
main = interact $ unwords . map show . solve . map (read :: String -> Int) . tail . words

solve :: [Int] -> [Int]
solve input = [brokeBest-1, brokeWorst-1] where
    brokeBest = length . group . map maximum . tail . inits $ input
    brokeWorst = length . group . map minimum . tail . inits $ input




findWays :: Int -> [Int] -> [Int] -> [[Int]]
--  n : remaining value
-- xs : candidates
-- ys : taken powers
findWays n xs ys  | n < 0  = []
                  | n == 0 = [ys]
                  | otherwise =
    concatMap helper . takeWhile (<= n) $ xs
    where
      helper :: Int -> [[Int]]
      helper k = findWays (n-k) (filter (>k) xs) (k:ys)

process n k = 
    length . findWays n (takeWhile (<= n) [x^k | x <- [1..]]) $ []

main = process <$> readLn <*> readLn >>= print


-- отфильтровать элементы, всречающиеся count раз rere

import Data.List

main :: IO()
main = interact $ write . map (uncurry solve) . parse
    where
        parse :: String -> [(Int, [Int])]
        parse = prettyInput . map (((map read):: [String] -> [Int]) . words) .
            tail . lines
        write :: [[Int]] -> String
        write = unlines . map unwords . map (map show)

prettyInput :: [[Int]] -> [(Int, [Int])]
prettyInput [] = []
prettyInput (servInf:seq:rest) = (last servInf, seq) : prettyInput rest


solve :: Int -> [Int] -> [Int]
solve count input = let 
    satisfying :: [Int] -> [Int]
    satisfying = map head . filter (\xs -> length xs >= count) . group . sort 
    in case (length $ satisfying input) of
    0 -> [-1]
    oherwise -> nub . filter (flip elem $ satisfying input) $ input
    

-- найти НОД массива чисел, предаставленных особым образом 
-) построчно
-) в виде "простое число" "степень" "прост. ч." "степ"...

main :: IO()
main = interact $ unwords . showPair . solveG . map constructPair . map (map (read :: String -> Int)) . map words . tail . lines

type ProdPrime = [(Int, Int)]

constructPair :: [Int] -> ProdPrime
constructPair [] = []
constructPair (digit:power:rest) = (digit, power) : constructPair rest

gcd' :: ProdPrime -> ProdPrime -> ProdPrime
gcd' x y = 
    [(div, pow) | div <- map fst x,
                  div `elem` map fst y,
                  let pow = min (fromJust $ lookup div x) (fromJust $ lookup div y) ]

solveG :: [ProdPrime] -> ProdPrime
solveG [x] = x
solveG [x,y] = gcd' x y
solveG (x:xs) = gcd' x  $ solveG xs

showPair :: ProdPrime -> [String]
showPair [] = []
showPair ((a,b):xs) = show a : show b : showPair xs

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "fromJust : `\"Nothing nor parse\"'"


-}

----------------------------------------------------------------------------

lowestXY :: [Point] -> Point
lowestXY input = bottom where
   bottom = (xMin,yMin)
   yMin = minAxis snd input
   xMin = minAxis fst ysMin
   ysMin = filter (\(x,y) -> y == yMin) input

minAxis :: (Point -> Int) -> [Point] -> Int
minAxis f [p] = f p
minAxis f (p:ps) = f p `min` minAxis f ps


data Side = OnLeft | OnRight | OnThe 
  deriving (Show, Read, Eq)

psevdoVecPr :: (Num a, Integral a) => Vector2 a -> Vector2 a -> a
psevdoVecPr (Vector (x,y)) (Vector (x',y')) = x * y' - x' * y

constructVec :: Num a => (a, a) -> (a, a) -> Vector2 a
constructVec (x,y) (x',y') = Vector (x' - x, y' - y)

angleBetween :: (Num a, Floating b, Integral a) => Vector2 a -> Vector2 a -> b
angleBetween a b = asin (fromIntegral (psevdoVecPr a b) / lengthV a / lengthV b)

lengthV :: (Num a, Integral a, Fractional b, Floating b) => Vector2 a -> b
lengthV (Vector (x,y)) = sqrt (fromIntegral x ^ 2 + fromIntegral y ^ 2)


-- косое (псевдовекторное) произведение двух векторов
-- положительно, если поворот против часовой стрелки
-- отрицательно, если поворот по часовой стрелке
-- равно нулю, если векторы коллинеарны
rotate :: Point -> Point -> Point -> Side
rotate (a,b) (c,d) (e,f)
    | psevdoVecPr v1 v2 > 0  = OnLeft
    | psevdoVecPr v1 v2 < 0  = OnRight
    | psevdoVecPr v1 v2 == 0 = OnThe where
        v1 = Vector (c-a,d-b)
        v2 = Vector (e-a,f-b)

{-
import Data.List
main :: IO ()
main = interact $ unlines . map unwords . solve . uncurry . constructPair . lines

constructPair = (\[x,y] -> (x,y))

solve :: String -> String -> [[String]]
solve a b = map (\x -> [show $ length x,x])
                    [maxPrefix, aWithoutPref, bWithoutPref] where
    maxPrefix = flip take a . flip (-) 1 . length . filter (== True) .
        map (flip isPrefixOf a) . inits $ b
    Just aWithoutPref = stripPrefix maxPrefix a
    Just bWithoutPref = stripPrefix maxPrefix b

-}
{-
angleVecOX :: (Num a, Integral a) => Point -> Point -> Ordering
angleVecOX a b = (\[a,b] -> compare a b) $ constructVec 
 map (angleBetween (Vector (1,0))) [a,b]
:m
test = [(1,1), (2,5), (3,3), (5,3), (3,2), (2,2)]
test2 = [(1,1), (1,2), (1,3), (1,4), (1,5)]
bottom = (-5,-5)
vectors = map (flip constructVec $ bottom) test2


solve'' :: String -> String -> [[String]]
solve'' a b = map (\x -> [show $ length x,x]) [maxPrefix, aWithoutPref, bWithoutPref] where
    maxPrefix = flip take a . flip (-) 1 . length . filter (== True) . map ((flip isPrefixOf) a) . inits $ b
    Just aWithoutPref = stripPrefix maxPrefix a
    Just bWithoutPref = stripPrefix maxPrefix b



myYY :: IO ()
myYY = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \_ -> do 
        x_temp <- getLine
        let x = read x_temp :: Int
        putStrLn $ isPrime x

myXX :: IO ()
myXX = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \_ -> do
         putStrLn $ isPrime 4

-}

myYY :: IO ()
myYY = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \_ -> do 
        x_temp <- getLine
        let x = read x_temp :: Int
        putStrLn $ isPrime x

    
isPrime :: Int -> String
isPrime x = case property x of
    True  -> "Prime"
    False -> "Not Prime"
    where
        property :: Int -> Bool
        property x
            | even x = False
            | otherwise = all (\y -> x `mod` y /= 0) [3 .. ceiling . sqrt . fromIntegral $ x]


separator :: String -> [String]
separator [] = []
separator (x:xs) = [x] : separator xs

solve' :: String -> String
solve' input = case length input of
    1 -> input
    _ -> solve' $ show $ foldr (\x acc -> acc + read x) 0 $ separator input





data SinglyLinkedListNode = SinglyLinkedListNode {
    nodeData :: Int,
    next :: SinglyLinkedListNode
} | SinglyLinkedListNodeNil

createSinglyLinkedList :: [Int] -> SinglyLinkedListNode
createSinglyLinkedList [] = SinglyLinkedListNodeNil
createSinglyLinkedList (x:xs) = SinglyLinkedListNode {
    nodeData = x,
    next = createSinglyLinkedList xs
}

instance {-# OVERLAPPING #-} Show (SinglyLinkedListNode, String) where
    show (SinglyLinkedListNodeNil, _) = ""
    show (SinglyLinkedListNode x SinglyLinkedListNodeNil, _) = show x
    show (SinglyLinkedListNode x xs, sep) = show x ++ sep ++ show(xs, sep)


insertNodeAtPosition :: SinglyLinkedListNode ->
                        Int ->
                        Int ->
                        SinglyLinkedListNode
                        
insertNodeAtPosition SinglyLinkedListNodeNil val _ = createSinglyLinkedList [val]
insertNodeAtPosition (SinglyLinkedListNode x xs) val 0 = SinglyLinkedListNode val (SinglyLinkedListNode x xs)
insertNodeAtPosition (SinglyLinkedListNode x xs) val pos = SinglyLinkedListNode x (insertNodeAtPosition xs val (pos-1))

-- вычисление даты Дня программиста

constructYearType :: Int -> Year
constructYearType y
    | y > 1918  = Grigorian $ typeG y
    | otherwise = Julian $ typeJ y where
        typeG x = case isLeapG x of
            True  -> Leap x
            False -> NonLeap x
        typeJ y = case isLeapJ y of
            True  -> Leap y
            False -> NonLeap y
        isLeapG :: Int -> Bool
        isLeapG = (\x -> (x `mod` 400 == 0) ||
                        ((x `mod` 4 == 0) && (x `mod` 100 /= 0))
                    )
        isLeapJ :: Int -> Bool
        isLeapJ = (\x -> (x `mod` 4 == 0))

dayOfTheProgrammer :: Year -> Date
dayOfTheProgrammer (Julian (NonLeap 1918)) = Date (26,9,1918)
dayOfTheProgrammer (Grigorian (Leap x))    = Date (12,9,x)
dayOfTheProgrammer (Julian(Leap x))        = Date (12,9,x)
dayOfTheProgrammer (Grigorian (NonLeap x)) = Date (13,9,x)
dayOfTheProgrammer (Julian (NonLeap x))    = Date (13,9,x)

showDate :: Date -> String
showDate (Date (d,m,y)) = show d ++ "." ++ "0" ++ show m ++ "." ++ show y

solveDayOfTheProgrammer =  showDate . dayOfTheProgrammer . constructYearType . read

--

{-
main:: IO()
main = do
    nAndI <- getLine
    let [_,i] = map read . words $ nAndI :: [Int]
    list_temp <- getLine
    let list = map read . words $ list_temp :: [Int]
    let c <- readLn :: IO Int
-}


bonAppetit :: [Int] -> [Int] -> Int -> String
bonAppetit list ps brianPay = case abs $ sum annPay `div` 2 - brianPay of 
    0 -> "Bon Appetit"
    x -> show x
  where annPay = [x | (x,p) <- zip list [0..], not $ p `elem` ps]

solveBA = bonAppetit [3,10,2,9] [1,2] 6


next' :: Fractional a => a -> a -> a
next' n x = (x + n/x) / 2




divs a = [x | x <- [1..a], a `mod` x ==0 ]
intersection a = filter (flip elem a)  

subF :: Char -> String -> Int
subF ch = foldr (\x acc -> if ch == x then acc + 1 else acc) 0

isValid :: String -> [Int]
isValid =  tail . 
    sortBy (reverseCompare). map length . group . sort

reverseCompare a b = case compare a b of
    GT -> LT
    LT -> GT
    EQ -> EQ

sub list = --all (==True) $
     zipWith (==) list ([last list] ++ list)

lazyFoldZipWith :: (a -> b -> Bool) -> [a] -> [b] -> Bool
lazyFoldZipWith _ [] _ = True
lazyFoldZipWith _ _ [] = True
lazyFoldZipWith f (x:xs) (y:ys) = case x `f` y of
    True -> lazyFoldZipWith f xs ys
    False -> False

solveIs list = case last list of
   1 -> True
   _ -> lazyFoldZipWith (==) list ([last list] ++ list)



newtype Vector = MkVec [Float] deriving (Eq, Show, Read, Ord)

lengthVec :: Vector -> Float
lengthVec (MkVec xs) = sqrt . foldr (\x acc -> acc + x*x) 0 $ xs

vecInprod :: Vector -> Vector -> Float
vecInprod (MkVec xs) (MkVec ys) = sum $ zipWith (*) xs ys

lengthVec' v = sqrt $ vecInprod v v

newtype Matrix = MkMat [[Float]] deriving (Eq, Show, Read, Ord)

transpose' :: Matrix -> Matrix
transpose' (MkMat xxs) = MkMat $ foldr (zipWith (:)) e xxs where
  e = repeat []