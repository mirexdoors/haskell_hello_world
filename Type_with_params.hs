import Data.Char
import Data.List
{-
Реализуйте функции distance, считающую расстояние между двумя точками с вещественными координатами,
 и manhDistance, считающую манхэттенское расстояние между двумя точками с целочисленными координатами.-}

data Coord a = Coord {x :: a, y :: a} deriving Show

distance :: Coord Double -> Coord Double -> Double
distance c1 c2 = sqrt $ (x c2 - x c1)^2 + (y c2 - y c1)^2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = (abs (x1-x2)) + (abs (y1-y2))

coord1 = Coord {x = 0.0, y = 3.0}
coord2 = Coord {x = 0.0, y = 4.0}

manhCoord1 = Coord {x = 1, y = 3}
manhCoord2 = Coord {x = 1, y = 4}

--------------------------------------------
{-
Реализуйте функцию, которая ищет в строке первое вхождение символа, который является цифрой, и возвращает Nothing, если в строке нет цифр.
-}

findDigit :: [Char] -> Maybe Char
findDigit (x:xs) = if (isDigit x) then Just x  else findDigit xs
findDigit [] =  Nothing
----------------
{-Реализуйте функцию findDigitOrX, использующую функцию findDigit (последнюю реализовывать не нужно).
findDigitOrX должна находить цифру в строке, а если в строке цифр нет, то она должна возвращать символ 'X'.
 Используйте конструкцию case.-}

findDigitOrX :: [Char] -> Char
findDigitOrX str = case findDigit str of
                                     Just x  ->  x
                                     Nothing -> 'X'

-----------------------------------------
{-Maybe можно рассматривать как простой контейнер, например, как список длины 0 или 1.
 Реализовать функции maybeToList и listToMaybe, преобразующие Maybe a в [a] и наоборот (вторая функция отбрасывает все элементы списка, кроме первого).-}

maybeToList :: Maybe a -> [a]
maybeToList x  = case x of
						Just x -> [x]
						Nothing -> []

listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe [] = Nothing

----------------------------------------------
{-

Реализуйте функцию parsePerson, которая разбирает строки вида firstName = John\nlastName = Connor\nage = 30 и возвращает либо результат типа Person, либо ошибку типа Error.

Строка, которая подается на вход, должна разбивать по символу '\n' на список строк, каждая из которых имеет вид X = Y. Если входная строка не имеет указанный вид, то функция должна возвращать ParsingError.
Если указаны не все поля, то возвращается IncompleteDataError.
Если в поле age указано не число, то возвращается IncorrectDataError str, где str — содержимое поля age.
Если в строке присутствуют лишние поля, то они игнорируются.-}
