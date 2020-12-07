module Misha where

--solve::Double
solve = [ el | n <- [1..10], let el = (x n) /  5.0^n, el>0.001]

--x :: Double -> Double
x n = 4 * x (n-1) - 3 * x (n-2)
x 1 = 5
x 2 = 11