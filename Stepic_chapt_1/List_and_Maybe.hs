import Data.Char
import Data.List

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken str | str == "+" = Just Plus
            | str == "-" = Just Minus
            | str == ")" = Just RightBrace
            | str == "(" = Just LeftBrace
            | all (isDigit) str = Just (Number (read str::Int))
            | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize str = sequence tokenList where
  tokenList = map asToken (words str)

test = tokenize "1 + ( 7 - 2 )"

-------------------------------------
{-Пусть имеется тип данных, который описывает конфигурацию шахматной доски:

data Board = ...
Кроме того, пусть задана функция
nextPositions :: Board -> [Board]
которая получает на вход некоторую конфигурацию доски и возвращает все возможные конфигурации,
которые могут получиться, если какая-либо фигура сделает один ход. Напишите функцию:
nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
которая принимает конфигурацию доски, число ходов n, предикат и возвращает все возможные конфигурации досок,
которые могут получиться, если фигуры сделают n ходов и которые удовлетворяют заданному предикату.
При n < 0 функция возвращает пустой список.-}

data Board = Board Int deriving Show

nextPositions :: Board -> [Board]
nextPositions (Board i) = [Board (i * 10 + 1), Board (i * 10 + 2)]

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred
  | n < 0 = []
  | n == 0 = filter pred [b]
  | otherwise = do
    move      <- nextPositions b
    restMoves <- nextPositionsN move (n - 1) pred
    return restMoves

-------------------------------------------------------
{-Используя монаду списка и do-нотацию, реализуйте функцию

pythagoreanTriple :: Int -> [(Int, Int, Int)]

которая принимает на вход некоторое число xx и возвращает список троек (a, b, c)(a,b,c),
 таких что выполняется теорема пифагора.

Число xx может быть \leq 0≤0 , на таком входе должен возвращаться пустой список.

GHCi> pythagoreanTriple 5
[(3,4,5)]

GHCi> pythagoreanTriple 0
[]

GHCi> pythagoreanTriple 10
[(3,4,5),(6,8,10)]-}

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x | x <= 0 = []
                    | otherwise = do
                      a <- [1 .. x - 2]
                      b <- [1.. x - 1]
                      c <- [1.. x]
                      if (c^2 == a^2 + b^2 && a < b) then "1" else []
                      return (a, b, c)


