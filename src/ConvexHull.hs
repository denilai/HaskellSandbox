module ConvexHull where

import Data.List

newtype Point  = P (Int, Int) deriving (Show, Eq, Read)
newtype Vector = V (Int, Int) deriving (Show, Eq, Read)


-----------------------------TESTS---------------------------------
testP :: [Point]
testP = map P [(1,2),(4,9),(12,5),(5,4),(-2,-4),(-8,-7),
   (10,2),(4,17),(-1,-1),(0,4),(-8,-8), (5,5),(7,7)]

testP3 = map P [(1,1), (2,5), (3,3), (5,3), (3,2), (2,2)]

testP2 = map P [(0,0),(1,0),(13,13),(2,0),(2,2),(-2,1),
   (9,9),(7,7),(-2,3),(0,3),(2,3), (4,4)]

testV :: [Vector]
testV = map (constrVec (bottom testP2)) testP2
--------------------------------------------------------------------


-- дно конкретного списка в глобальной видимости
bot = bottom testP2


-- I)
-- найдем самую нижнюю точку в наборе - "дно" списка

bottom :: [Point] -> Point
bottom = foldr1 minAxis

-- вспомогательная функция для foldr1
minAxis :: Point -> Point -> Point
minAxis (P(x1,y1)) (P(x2,y2)) = case compare y1 y2 of
    GT -> P(x2,y2)
    LT -> P(x1,y1)
    EQ -> case compare x1 x2 of
       GT -> P(x2,y2)
       _  -> P(x1,y1)


-- II)
-- отсортируем точки по возрастанию угла, который образует
-- ось OX с вектором, направленным из "дна" списка
-- в данную точку (угол считаем против часовой стрелки)

sortByAngle = sortBy cosBetweenBot . constrPair

-- создаем список пар типа (Point, Angle),
-- где Angle - синус угла межу осью OX и вектором,
-- с началом в "дне" списка и концом в данной точке
constrPair :: [Point] -> [(Point, Double)]
constrPair = foldr f [] where
  f point acc
     | point  == bot  = acc
     | otherwise = (point , angle) : acc where
         angle = cosBetween oX fromBotToPoint
         oX = constrVec (P(0,0)) (P(1,0))
         fromBotToPoint = constrVec bot point 

-- фунция, упорядочивающая точки по
-- полярному углу между прямой, параллельной
-- OX, проведенной из точки, являющейся "дном"
-- в направлении против часовой стрелки
cosBetweenBot :: (Point, Double) -> (Point, Double) -> Ordering
cosBetweenBot (p1,cos1) (p2,cos2) = case compare cos1 cos2 of
    GT -> GT
    EQ -> lengthV a' `compare` lengthV b'
    _ -> LT
  where a' = constrVec bot p1
        b' = constrVec bot p2
 
-- III) удалим точки, образующие один угол
--      с осью OX. Оставим самую дальнюю

-- функция удаляет точки с равными углами,
-- оставляя ближайшие к "дну" списка
-- оставляет только точки
filterSameAngles :: [(Point,Double)] -> [Point]
filterSameAngles lst = map fst . filter snd $ raw where
   raw = zipWith (\(p1,a1) (p2,a2) -> (p1, abs(a1-a2) > eps))
      lst  ((drop 1 lst) ++ [head lst]) 
   -- точность
   eps = 0.001


-- IV) Рекурсивно построим оболочку

build :: [Point] -> [Point] -> [Point]
build hull [] = hull
build [h1] points @ (p:ps) = build [h1,p] ps
build hull @ (h2:h1:hs) points @ (p:ps) = hull'
  where
    v1  = constrVec h1 h2
    v2  = constrVec h1 p
    v1' = constrVec h2 h1
    v2' = constrVec h2 p
    rightTurn = psevdoSclMult  v1 v2 < 0
    collinear = psevdoSclMult  v1' v2' == 0
    hull' | rightTurn = build (h1:hs) points   -- Remove head and retry
          | collinear = build (p:h1:hs) ps     -- Replace the head with p
          | otherwise = build (p:hull) ps  -- Add p

---------------ТЕОРЕТИЧЕСКИЕ ПРИКЛАДНЫЕ ФУНКЦИИ-------------
-- понятие косого произведения (псевдоскалярного)
psevdoSclMult :: Vector -> Vector -> Int
psevdoSclMult (V(x1,y1)) (V(x2,y2)) = x1 * y2 - x2 * y1

-- понятие скалярного произведения
scalarMult :: Vector -> Vector -> Int
scalarMult (V(x1,y1)) (V(x2,y2)) = x1 * x2 + y1 * y2

-- длинна вектора
lengthV :: Vector -> Double
lengthV (V(x,y)) = sqrt(fromIntegral(x*x+y*y))

-- конструирование вектора
constrVec :: Point -> Point -> Vector
constrVec (P(a,b)) (P(c,d)) = V (c-a, d-b)

-- функция находит косинус угла между векторами
cosBetween :: Vector -> Vector -> Double
cosBetween a b
    | all (/= 0) [lengthV a, lengthV b] = 
           fromIntegral (scalarMult a b) / lengthV a / lengthV b
    | otherwise = 0
------------------------------------------------------------


-- отсортировать по углу между "дном"
-- в направлении против часовой стрелки



-- этапы фильтра грэма
-- 1) найти "дно" ->> findMinYPoint testP2
-- 2) отсортировать по углу между "дном"
--     в направлении против часовой
--     стрелки ->> sortP = sortBy sinBetweenBot
-- 3) исключить точки, с одинаковым углом между дном
--     оставить только дальнюю ->> filterSameAngles $ ...
-- 4) строим выпуклую оболочку, рекурсивно проверяя, 
--    попадет ли следующая точка в нее ->> hull = build [bot] $


a = V(-2,1)
b = V (2,1)



adder list = [last list] ++ list 
perimeter :: [Point] -> Double
perimeter input = sum [ side (x,y) (x',y') | ((P(x,y)),a) <- zip input [0..],
                                             ((P(x',y')),b) <- zip input [0..],                                                          a == b - 1 ] where
  side (x,y) (x',y') = sqrt (fromIntegral (x - x') ^ 2 + fromIntegral (y - y') ^ 2)

solve :: [Point] -> Double
solve input = perimeter . adder . build [bottom input] . filterSameAngles . sortByAngle $ input

   