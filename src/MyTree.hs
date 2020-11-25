----------------------------------------------------------------------------
-- рекурсивные типы данных на примере бинарных деревьев	
module MyTree where

-- по видеокурсу на ютубе (НОУ Интуит)
{-
data Tree a = Leaf a | Node a (Tree a) (Tree a)
      deriving (Show, Read, Ord, Eq)

-- функция сичтает количество узлов в дереве
nodeCount :: Tree a -> Int
nodeCount (Leaf _)            = 0
nodeCount (Node _ left right) = (+1) $ sum $ map nodeCount [left, right]

tree1 =  Node 1 
            (Node 2
                 (Leaf 1)
                 (Leaf 2)
             )
            (Node 3
                 (Node 4
                     (Leaf 3)
                     (Leaf 4)
                  )
                 (Node 5
                     (Leaf 5)
                     (Leaf 6)
                  )
             )

-- функция считает количество листьев 
leafCount :: Tree a -> Int
leafCount (Leaf _) = 1
leafCount (Node x left right) = leafCount left + leafCount right
-}

-- построение дерева поиска 

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show, Read, Ord, Eq)

-- добавление элемента в дерево
-- с сохранением ориетированности и сбалансированности
insertNode :: Ord a => a -> Tree a -> Tree a 
insertNode e Leaf          = Node e Leaf Leaf
insertNode e (Node x l r) 
    | e > x     = Node x  l (insertNode e r)
    | otherwise = Node x (insertNode e l) r

-- функция, которая находит проекции элементов дерева поиска. Плющит его
-- flatten

flatten :: Ord a => Tree a -> [a]
flatten Leaf         = []
flatten (Node x l r) = flatten l ++ [x] ++ flatten r

-- функция поиска елемента в бинарном дереве поиска

elemTree :: Ord a => a -> Tree a -> Bool
elemTree _ Leaf         = False
elemTree elem (Node x l r)
    | elem == x = True
    | elem < x = elemTree elem l
    | elem > x = elemTree elem r

-- функция, которая удаляет узел из бинарного дерева поиска

deleteNode :: Ord a => a -> Tree a -> Tree a
deleteNode _ Leaf            = Leaf
deleteNode elem (Node x l r)
   | elem < x  = Node x (deleteNode elem l) r
   | elem  == x = join l r -- Node (last flatten l) (deleteNode elem l) r
   | elem  > x  = Node x l (deleteNode elem r)

-- функция join, "склеивающая" два бинарных дерева поиска,
-- сохранив при этом все свойства дерева поиска

-- функция largestWithout возвращает пару, 
-- состоящую из максимального элемента
-- в дереве и исходного дерева, без этого элемента

join :: Ord a => Tree a -> Tree a -> Tree a
join Leaf rigth = rigth
join left right  = Node m left' right where
  (m,left') = largestWithout left
  largestWithout (Node x l Leaf) = (x,l)
  largestWithout (Node m l r) = (m',Node m l r') where
     (m',r') = largestWithout r

{-
--graft a tree - прививать дерево
graftTree :: Tree Int -> (Int, Int) -> Tree Int
graftTree (tree) (a,b) 
  | tree = Node _ Leaf Leaf
   
    let newLeft  = Node a Leaf Leaf
        newRigth = Node b Leaf Leaf in
-}
                            