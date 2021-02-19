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
