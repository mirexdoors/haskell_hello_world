import Data.List

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
