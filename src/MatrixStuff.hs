module MatrixStuff where
import Data.List



scalarProd :: Floating a => [a] -> [a] -> a
scalarProd a = sum . ((*) /#/) a

(/*/) :: Floating a => [a] -> [a] -> a
(/*/) = scalarProd

($*/) :: Floating a => a -> [a] -> [a]
($*/) k vec = (*k) <$> vec

($:/) :: Floating a => a -> [a] -> [a]
($:/) k vec = (/k) <$> vec

(/-/) :: Floating a => [a] -> [a] -> [a]
(/-/) = ((-) /#/)

(/+/) :: Floating a => [a] -> [a] -> [a]
(/+/) = ((+) /#/)

(/:/) :: Floating a => [a] -> [a] -> [a]
(/:/) = ((/) /#/)

(/#/) :: Floating a => (a -> a -> a) -> [a] -> [a] -> [a]
(/#/) f a b = zipWith f a b


lengthVec :: Floating a => [a] -> a
lengthVec a = sqrt $ a /*/ a



variant6 :: Fractional a => Matrix a
variant6 = M [[4,1,1,-1],[2,1,4,4],[1,1,5,6]]

testOrt2 :: Fractional a => Matrix a
--1,957   3,126    3,803
testOrt2 = M [[3,0.15,-0.09,-6],[0.08,4,-0.16,-12],[0.05,0.3,5,-20],[0,0,0,1]]


variant7Ort :: Fractional a => Matrix a
--1,957   3,126    3,803
variant7Ort = M [[1,4,-3,-3],[4,2,1,-11],[1,-1,0,-1],[0,0,0,1]]


variant6Ort :: Fractional a => Matrix a
variant6Ort = M [[4,1,1,1],[2,1,4,-4],[1,1,5,-6],[0,0,0,1]]

test :: Fractional a => Matrix a
test = M $ map (map fromInteger)[[1,1,-1,0],[2,1,1,7],[1,-1,1,2]]

testOrt :: Matrix Float
testOrt = M([[1,2,-1],[3,1,-8],[0,0,1]])

newtype Matrix a = M [[a]]
  deriving (Read, Eq)

instance Show a => Show (Matrix a) where
  show (M rows) = concat $ intersperse "\n" $ show <$> rows