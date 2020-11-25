module Samples where



-- проверка карты на валидность номера
-- абстрактные операции с числами...

toDigits :: Integer -> [Integer]
toDigits x = if x `div` 10 > 0
    then toDigits (x `div` 10) ++ [x `mod` 10]
       else [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
   | x == 0 = []
   | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

-- число, необходимое для определения валидности номера карты
cardMagicNum :: Integer -> Integer
cardMagicNum =
      (\(a,b) -> g a + g b) . double 2 . toDigitsRev
        where g :: [Integer] -> Integer
              g = sum . concat . map toDigitsRev

validate :: Integer -> Bool
validate x = x `mod` 10 == 0



-- решил, что эта функция не должна контролировать входные данные (pos)
-- так как она является лишь инструментом, контролировать должна главная функция
double :: Int -> [Integer] -> ([Integer],[Integer])
double pos x = helper pos ([],[]) x where
    helper _ pair [] = pair
    helper c (a,b) y
       | length y >= c = helper c ( (y !! (c-1)) * 2 : a,  b ++ take (c-1) y) $ drop c y
       | otherwise     = (a,b)   



----------------------------------------------------------------------------


-- Ханойская башня

    