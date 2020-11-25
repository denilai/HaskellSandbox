module MatrixOrtogonal where
import MatrixStuff

riF :: Floating a => Matrix a -> Int -> [a]
riF (M matrix) 1 = matrix !! 0
riF (M matrix) k =
    ak /-/ (foldr1 (/+/) $ list ak (M matrix) k)
    where ak = matrix !! (k-1)


siF :: Floating a => Matrix a -> Int -> [a]
siF m k =   (lengthVec rk) $:/ rk where rk = riF m k

ortMethod :: Floating a => Matrix a -> [a]
ortMethod (M matrix) = [ri / rn_1 | let r = riF (M matrix) $ length matrix,
                                          i <- [0..length r - 1],
                                          let rn_1 = last r,
                                          let ri = r !! i]


list :: Floating a => [a] -> Matrix a -> Int -> [[a]]
list ak m k = [(ak /*/ si) $*/ si | si <- map (siF m) [1..k-1]]

