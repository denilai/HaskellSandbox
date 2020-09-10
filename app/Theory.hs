{-
  в этом модуле представлены конспекты по темам, которые я счиатаю 
  наиболее важными
  Содержание:
     1.1 Свертки
      foldr и foldl и их модификации

  
-}

-- uncurry :: (a -> b -> c) -> (a,b) -> c

-- intersperse ',' "abcde" = "a,b,c,d,e"

module Theory where
import Control.Applicative
import Text.Printf as T

myfoldr :: (a -> b -> b ) -> b -> [a] -> b
myfoldr f ini []     = ini
myfoldr f ini (x:xs) = f x (foldr f ini xs)
-- notes : foldr f ini p:q:r:[] = f p (f q (f r (f []))) = 
--                               = f p (f q (f r ini))

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f ini []     = ini
myfoldl f ini (x:xs) = myfoldl f (f ini x) xs
-- foldl, будучи примененной к своим аргументам: бинарному оператору, начальному значению (обычно 
-- левому аргументу из тождества оператора) и списку, сокращает список, используя
-- бинарный оператор слева направо:
-- foldl f ini p:q:r:[] = f (f (f ini p) q) r
-- наростание скобок идет как бы влево
-- foldl1 является вариантом предыдущей функции, она не имеет аргумента с начальным значением, и поэтому должна
-- применяться к непустым спискам.  scanl похожа на foldl, но возвращает
-- список успешно сокращенных значений слева:
--      scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- Обратите внимание, что last (scanl f z xs) == foldl f z xs.
-- scanl1 похожа на предыдущую функцию, но без начального элемента:
--      scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]

myscanl :: (b -> a -> b ) -> b -> [a] -> [b]
myscanl f p xs = p : (case xs of
                       []     -> []
                       (x:xs) -> myscanl f (f p x) xs)


myscanr :: (a -> b -> b) -> b -> [a] -> [b]
myscanr f q0 []     =  [q0]
myscanr f q0 (x:xs) =  f x q : qs
                     where qs@(q:_) = scanr f q0 xs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

rev' :: [a] -> [a]
rev' = foldl (flip (:)) []

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : map (x :) acc ) []

----------------------------------------------------------------------------
-- Моноиды 
{-
Моноид — полугруппа с нейтральным элементом. Более подробно,
моноидом называется множество M, на котором задана бинарная
ассоциативная операция, обычно именуемая умножением,
и в котором существует такой элемент e, что ex=x=xe для любого x в M.
Элемент e называется единицей и часто обозначается 1.
В любом моноиде имеется ровно одна единица.


class Monoid a where
  mempty  :: a               -- нейтральный элемент 
  mappend :: a -> a -> a     -- ассоциативная (замкнутая) операция

  mconcat :: [a] -> a        -- свертка
  mconcat = forlr mappend mempty
-}

-- Аппликативные функторы
ff x =  (zipWith (liftA2 x) (map Just [1..10]) (map Just [2..20]))


-- функции maM и mapM_ для вывода элементов массива. 
{-
  Аналог маp для монад. функция имеет сигнатуру (a -> m b). Контекст
  возвращаемого значения - монада. 

-}
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r as bs = [integral (* step), integral (\x -> pi * x ^ 2 * step)]
  where
    step = 0.001 :: Double
    
    f :: Double -> Double
    f x = sum [(fromIntegral a) * (x ^^ b) | (a, b) <- zip as bs]
    
    integral :: (Double -> Double) -> Double
    integral g = sum $ map (g . f) [fromIntegral l, fromIntegral l + step .. fromIntegral r]


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger where
   smaller = [a | a <- xs, a <= x ]
   larger  = [a | a <- xs, a >  x ] 