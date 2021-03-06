import Data.List
import Data.Char

oddsOnly =  filter (odd)

isPalindrom :: Eq a => [a] -> Bool
isPalindrom list = list == reverse list where
  reverse tmpList = rev tmpList [] where
    rev [] b = b
    rev (x:xs) b = rev xs (x:b)


-----------------------------------------------
--Составьте список сумм соответствующих элементов трех заданных списков. Длина результирующего списка должна быть равна длине самого длинного из заданных списков, при этом «закончившиеся» списки не должны давать вклада в суммы.
--
--GHCi> sum3 [1,2,3] [4,5] [6]
--[11,7,3]
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 a b c = map sum (transpose [a,b,c])

-----------------------------------------------
--Напишите функцию readDigits, принимающую строку и возвращающую пару строк.
--Первый элемент пары содержит цифровой префикс исходной строки, а второй - ее оставшуюся часть.
--GHCi> readDigits "365ads"
--("365","ads")
--GHCi> readDigits "365"
--("365","")

readDigits :: String -> (String, String)
readDigits = span $ isDigit

----------------------------------------------------------
{-Реализуйте функцию filterDisj, принимающую два унарных предиката и список,
 и возвращающую список элементов, удовлетворяющих хотя бы одному из предикатов.

GHCi> filterDisj (< 10) odd [7,8,10,11,12]
[7,8,11]
-}
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj pred1 pred2 list= filter (\x -> pred1 x || pred2 x) list

----------------------------------------------------------
{-Напишите реализацию функции qsort.
Функция qsort должная принимать на вход список элементов и сортировать его в порядке возрастания с помощью сортировки Хоара:
 для какого-то элемента x изначального списка (обычно выбирают первый) делить список на элементы меньше и не меньше x,
 и потом запускаться рекурсивно на обеих частях. -}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = (qsort ys) ++ [x] ++ (qsort zs) where
    ys = filter (< x) (x:xs)
    zs = filter (> x) (x:xs)

qsort1 :: Ord a => [a] -> [a]
qsort1 [] = []
qsort1 [x] = [x]
qsort1 (x:xs) = (qsort1 lessPart) ++ [x] ++ (qsort1 morePart) where
   (lessPart, morePart) = partition (<x) xs

{-
Напишите функцию squares'n'cubes, принимающую список чисел,
и возвращающую список квадратов и кубов элементов исходного списка.
GHCi> squares'n'cubes [3,4,5]
[9,27,16,64,25,125]
-}
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

{-
Воспользовавшись функциями map и concatMap, определите функцию perms,
которая возвращает все перестановки, которые можно получить из данного списка, в любом порядке.

GHCi> perms [1,2,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-}

perms :: [a] -> [[a]]
perms []     = [[]]
perms [x]    = [[x]]
perms (x:xs) = ins x `concatMap` perms xs
  where
    ins :: a -> [a] -> [[a]]
    ins x y = map (\p -> (take p y) ++ [x] ++ (drop p y)) [0..(length y)]

---------------------------------------------------------------

{-
Реализуйте функцию delAllUpper, удаляющую из текста все слова, целиком состоящие из символов в верхнем регистре. Предполагается, что текст состоит только из символов алфавита и пробелов, знаки пунктуации, цифры и т.п. отсутствуют.

GHCi> delAllUpper "Abc IS not ABC"
"Abc not"
Постарайтесь реализовать эту функцию как цепочку композиций, аналогично revWords из предыдущего видео.
-}
delAllUpper :: String -> String
delAllUpper list = unwords $ filter (not . null) (filter (any isLower)  . words $ list)

------------------------------------------------
{-
Напишите функцию max3, которой передаются три списка одинаковой длины и которая возвращает список той же длины,
содержащий на k-ой позиции наибольшее значение из величин на этой позиции в списках-аргументах.

GHCi> max3 [7,2,9] [3,6,8] [1,8,10]
[7,8,10]
GHCi> max3 "AXZ" "YDW" "MLK"
"YXZ"
-}

largest a b c = max a (max b c)
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 a b c = zipWith3 (largest)  a b c
--------------------------------------------------------------------------------------
{-
Реализуйте c использованием функции zipWith функцию fibStream, возвращающую бесконечный список чисел Фибоначчи.

GHCi> take 10 $ fibStream
[0,1,1,2,3,5,8,13,21,34]
  [0,1]


-}
fibStream :: [Integer]
fibStream = 0 : 1  :  zipWith (+) fibStream (tail fibStream)

--------------------------------------
{-
Предположим, что функция repeat, была бы определена следующим образом:
repeat = iterate repeatHelper
определите, как должна выглядеть функция repeatHelper.
-}
repeat1 :: a -> [a]
repeat1 = iterate repeatHelper


repeatHelper = \x -> x

-----------------------------------------------------------------------------------------

data Odd = Odd Integer
  deriving (Eq, Show)

instance Enum Odd where
 succ (Odd x) = Odd $ x + 2
 pred (Odd x) = Odd $ x - 2
 toEnum x = Odd $ toInteger x * 2 + 1
 fromEnum (Odd x) = quot (fromInteger x - 1) 2
 enumFrom = iterate succ
 enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]
 enumFromTo (Odd x) (Odd y) = map Odd [x, x + 2 .. y]
 enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x , y .. z]

-------------------------------------------------------------------------------

{-
Пусть есть список положительных достоинств монет coins, отсортированный по возрастанию.
Воспользовавшись механизмом генераторов списков, напишите функцию change, которая разбивает переданную ей положительную сумму денег на монеты достоинств из списка coins всеми возможными способами.
Например, если coins = [2, 3, 7]:

--------------------------------
{-Реализуйте функцию sumOdd, которая суммирует элементы списка целых чисел, имеющие нечетные значения:

GHCi> change 7
[[2,2,3],[2,3,2],[3,2,2],[7]]

Примечание. Порядок монет в каждом разбиении имеет значение, то есть наборы [2,2,3] и [2,3,2] — различаются.
Список coins определять не надо.
-}
coins ::(Ord a, Num a) => [a]
coins = [5,9,13]

change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change amount = [ c:cs |c<-coins, amount>=c, cs<-change (amount - c) ]

GHCi> sumOdd [2,5,30,37]
42-}
------------------------------------------
{-
Напишите реализацию функции concatList через foldr

GHCi> concatList [[1,2],[],[3]]
[1,2,3]-}
concatList :: [[a]] -> [a]
concatList = foldr (++) []

------------------------------------------
{-
Используя функцию foldr, напишите реализацию функции lengthList, вычисляющей количество элементов в списке.

GHCi> lengthList [7,6,5]
3-}

lengthList :: [a] -> Int
lengthList = foldr (\_ s -> s + 1) 0

---------------------------------------
{-
Реализуйте функцию sumOdd, которая суммирует элементы списка целых чисел, имеющие нечетные значения:

GHCi> sumOdd [2,5,30,37]
42-}

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if (odd x) then s + x else s) 0

---------------------------------------------------------------
{-Реализуйте функцию meanList, которая находит среднее значение элементов списка, используя однократный вызов функции свертки.

GHCi> meanList [1,2,3,4]
2.5
Постобработка считается допустимой, то есть предполагаемая реализация функции meanList имеет вид

meanList = someFun . foldr someFoldingFun someIni-}

meanList :: [Double] -> Double
meanList list = foldr (\x s -> (x /  fromIntegral ((length list))) + s) 0 list

------------------------------------------------
{-Используя однократный вызов свертки, реализуйте функцию evenOnly, которая выбрасывает из списка элементы, стоящие на нечетных местах, оставляя только четные.

GHCi> evenOnly [1..10]
[2,4,6,8,10]
GHCi> evenOnly ['a'..'z']
"bdfhjlnprtvxz"-}

getOddPairs l= filter (odd . fst) (zip [0..] l)

evenOnly :: [a] -> [a]
evenOnly l = foldr (\x s -> snd x : s) [] (getOddPairs l)

------------------------------------------------------
{-
Напишите реализацию функции, возвращающей последний элемент списка, через foldl1.

lastElem :: [a] -> a
lastElem = foldl1 undefined-}

lastElem :: [a] -> a
lastElem = foldl1 (flip const)

-----------------------------------------------------
{-
Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном порядке список символов, попадающих в заданный парой диапазон.
 Попадание символа x в диапазон пары (a,b) означает, что x >= a и x <= b.
revRange :: (Char,Char) -> [Char]
revRange = unfoldr g
  where g = undefined
GHCi> revRange ('a','z')
"zyxwvutsrqponmlkjihgfedcba"-}

evRange :: (Char,Char) -> [Char]
evRange (a,b) = unfoldr (\x -> if x < a then Nothing else Just (x, pred x)) b

