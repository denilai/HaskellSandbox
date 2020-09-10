module DenParse (module DenParse, module Control.Applicative) where

import Control.Applicative
import Data.Char

-- парсер типа а  - это функция,
-- принимающая строку, и возвращающая
-- список пар типа (а, String), где 
-- snd - нераспарсенная строка


newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp


-- простой парсер, 
-- который возвращает первый аргумент
-- в независимости от входной строки
-- v - value
-- return (8:: Int) :: Parser Int
return' :: a -> Parser a
return' v = P (\inp -> [(v, inp)])

-- простой парсер, который возвращает
-- пустой список в независимости от входных
--данных
failure :: Parser a
failure = P (\inp -> [])

-- простой парсер, который в зависимости
-- от входной строки возрващает либо пустой
-- список, либо список, где отщипывает первый
-- символ от стоки
item :: Parser Char
item = P (\inp -> case inp of 
           []     -> []
           (x:xs) -> [(x,xs)])


-- для организации цепочек (последовательностей)
-- парсеров, сделаем тип Parser
-- экземплярами следующийх классов...

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Оператор выбора парсера

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Переопределим примитивы ...

-- satisfy - удовлетворяет
sat :: (Char -> Bool) -> Parser Char
sat p = do -- т.к. Parser  - уже монада, можем использовать do-нотацию
       x <- item
       if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

upper :: Parser Char
upper = sat isUpper

lower :: Parser Char
lower = sat isLower

letter :: Parser Char
letter = sat isLetter

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat


-- Интервал обработки

-- убирает все пробелы в начале строки
space :: Parser ()
space = do many (sat isSpace)
           return ()


token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)