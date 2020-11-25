module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
--parseMessage 
parseMessage str@(x:xs) = if validateString str then
      let input = preparing str :: [Either String (String, String)]
          mT = case head input of
             Right (a,b) -> Error $ read b
             Left "W" -> Warning
             Left "I" -> Info
          tS  = case input !! 1 of 
             Left x -> read x
             _ -> error "ERROR"
          mes = unwords $ map (\(Left x) -> x) $ drop 2 input 
          in LogMessage mT tS mes
              else Unknown str

-- проверка валидности (корректности) строки
-- для преобразования в LogMessage
validateString :: String -> Bool
validateString str = if first == "W" || first == "I" || first == "E" then 
       True else False where
             input = words str
             first = input !! 0

-- преготовления строки для обработки
preparing :: String -> [Either String (String, String)]
preparing str = case head w of
   "E" -> (Right $ (head w, w !! 1)) : (Left <$> drop 2 w)
   _   -> Left <$> w
   where  w = words str

-- считывает строки в LogMessage
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- вставляет одну вершину LogMessage в дерево
insert :: LogMessage -> MessageTree -> MessageTree 
insert (Unknown str) tree   = tree
insert message Leaf         = Node Leaf message Leaf
insert message (Node l x r)
   | stamp message <= stamp x = Node (insert message l) x r
   | stamp message >  stamp x = Node l x (insert message r)

-- строит дерево, поочередно накручивая Logmessage на Leaf
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


test1 = insert (parseMessage "W 123 all") (Node Leaf (parseMessage $ "I 1254 ggfdsgf") Leaf)

-- плющит дерево записей
flatten :: MessageTree -> [LogMessage]
flatten Leaf = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

inOrder = flatten

-- один из способов (не лучший) сортировки списка -
-- inOrder $ build tree


whatWentWrongNum :: Int -> [LogMessage] -> [String]
whatWentWrongNum x =
    map message . filter (\y -> stamp y > x && mType y /= Warning && mType y /= Info) . inOrder . build 

whatWentWrong = whatWentWrongNum 50

who = filter (\x -> elem "hacker" $ words x)