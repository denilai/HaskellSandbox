module Lib where

newtype Vector2 a = Vector (a,a) deriving (Show, Read, Eq)

instance (Floating a, Ord a) => Ord (Vector2 a) where
-- сравнение векторов по длинне
 Vector (a,b) `compare` Vector (c,d) 
       = compare (sqrt (a^2 + b^2)) (sqrt (c^2 + d^2))


type Point = (Int, Int)
type Triangle = (Point, Point, Point)

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "fromJust: can't convert `Nothing`"


data Color    = Black | White 
                     deriving Show

data ChessR   = King   -- король
              | Queen  -- ферзь
              | Bishop -- слон
              | Knight -- конь
              | Rook   -- ладья
              | Pawn   -- пешка 
                        deriving Eq
                         
instance Show ChessR 
   where
       show King = "King"
       show Queen  = "Queen"
       show Bishop = "Bishop"
       show Knight = "Knight"
       show Rook   = "Rook"
       show Pawn   = "Pawn"  


-- Chess Pieces - шахматные фигуры
data Chess  = Chess {  color    :: Color,
                       rang     :: ChessR,
                       place    :: (Char, Int)
                     } 
                       deriving Show

applyN :: Int -> (a -> a) -> a -> a
applyN n f x
    | n > 0  = iterate f x !! n
    | n == 0 = x
    | otherwise = error "Main.applyN: invalid Int value"



data Year = Julian TypeY | Grigorian TypeY

data TypeY = NonLeap Int | Leap Int

newtype Date = Date (Int, Int, Int)