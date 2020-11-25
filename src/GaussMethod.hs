module GaussMethod where

import IntegralStuff


gaussMethod :: Range ->
               CountOfPiece ->
               (Double -> Double) ->
               Double
gaussMethod (a,b) n f =
     sum $ map (* k) $ zipWith (*) weights  $ map (f . changeVar) abscesses  where
  k = (b-a)/2

  changeVar :: Double -> Double
  changeVar t =  (a+b)/2+(b-a)/2*t 
  
  weights ::  [Double]
  weights  = temp ++ reverse temp where
    temp = case n of
      2 -> [1]
      4 -> [0.347854, 0.652145]
      6 -> [0.171324, 0.360761, 0.467913]
      8 -> [0.101228, 0.222381, 0.313706, 0.362683]
      _ -> undefined
  
  abscesses :: [Double]
  abscesses = map negate temp ++ reverse temp where
    temp = case n of
      2 -> [0.57735]
      4 -> [0.861136, 0.339981]
      6 -> [0.932464, 0.661209, 0.238619]
      8 -> [0.960289, 0.796666, 0.525532, 0.183434]
      _ -> undefined
      
      
      
gaussMethod' :: Range ->
                CountOfPiece ->
                (Double -> Double) ->
                [Double]
gaussMethod' (a,b) n f =
     map (* k) $ zipWith (*) weights  $ map (f . changeVar) abscesses  where
  k = (b-a)/2

  changeVar :: Double -> Double
  changeVar t =  (a+b)/2+(b-a)/2*t 
  
  weights ::  [Double]
  weights  = temp ++ reverse temp where
    temp = case n of
      2 -> [1]
      4 -> [0.347854, 0.652145]
      6 -> [0.171324, 0.360761, 0.467913]
      8 -> [0.101228, 0.222381, 0.313706, 0.362683]
      _ -> undefined
  
  abscesses :: [Double]
  abscesses = map negate temp ++ reverse temp where
    temp = case n of
      2 -> [0.57735]
      4 -> [0.861136, 0.339981]
      6 -> [0.932464, 0.661209, 0.238619]
      8 -> [0.960289, 0.796666, 0.525532, 0.183434]
      _ -> undefined


-- sum $ map (* koef) $ zipWith (*) weights  $ map (f . changeVar) abscesses
--test = foldr1 (\x acc -> acc + x * koef) $ zipWith (*) weights  $ map (f . changeVar) abscesses