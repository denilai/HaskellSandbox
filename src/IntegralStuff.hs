module IntegralStuff where

myFunc :: Double -> Double -> Double -> Double
myFunc k l x = (x+l)/(x*x+x+k)

g :: Double -> Double
g x = 6.0*(x^5)

type Range = (Double,Double)

type CountOfPiece = Int

variant6 :: Double -> Double
variant6 = myFunc 2.2 1.2