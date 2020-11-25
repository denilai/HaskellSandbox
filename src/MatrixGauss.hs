
module MatrixGauss where
import MatrixStuff

import Text.Printf (printf)



-- одна итерация прямого хода метода Гаусса
straightDir' :: (Fractional a) => Int -> Matrix a -> Matrix a
straightDir' n (M matrix) =
  let n'     = pred n
      proj j = matrix !! n' !! j
      lider  = matrix !! n' !! n'
       in M [ row' | (row,i) <- zip matrix [0..],
                        let row'
                             | i > n'  = map (\(x,j) -> x - (row !! n' * proj j / lider) ) $ zip row [0..]
                             | i == n' = map (/lider) row
                             | i < n'  = row
             ]


straightDir :: Fractional a => Int -> Matrix a -> Matrix a
straightDir 0 matrix = matrix
straightDir n matrix = straightDir' n $ straightDir (n-1) matrix

showSteps ::  Matrix Double -> IO()
showSteps ( matrix) = mapM_ (putStrLn . (++"\n") . show . ((\x -> straightDir x matrix) )) [0..3]


xN :: Fractional a => Int ->Int -> [[a]] -> [a]
xN n k matrix
      | n < k  = values !! n' - (sum $ zipWith (*) (drop n row') $ pre) : pre
      | n == k = [last values]
   where
      values = map last matrix
      n' = n-1
      row' = matrix !! n'
      pre = xN (n+1) k matrix


applyXn :: Fractional a => Int -> Matrix a -> [a]
applyXn n (M m) = xN n 3 m


gaussMethod :: (Fractional a) => Matrix a -> [a]
gaussMethod m =  applyXn 1 $ triangleMtrx m where
      triangleMtrx = straightDir 3


