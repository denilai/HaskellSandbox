module TrapezeMethod where

import IntegralStuff


-- упрощенное представление метода
-- повторяющиеся члены вынесены, скомбинированы
trapezeMethod :: Range ->
                 CountOfPiece ->
                 (Double -> Double) ->
                 Double
trapezeMethod (a,b) n f =
    h / 2 * (f a + f b + 2 * sum internalTrapeze) where
      h = (b - a) / fromIntegral n
      internalTrapeze = [f $ a + fromIntegral(i) * h | i <- [1..n-1]]


-- прямое представление метода
-- буквальное сложение площадей трапеций
trapezeMethod' :: Range ->
                 CountOfPiece ->
                 (Double -> Double) ->
                 [Double]
trapezeMethod' (a,b) n f =
                [ (f x + f (x + h)) / 2 * h | let h = (b - a) / fromIntegral n,
                                                  x <- [a, a + h .. b - h] ]
                                                  