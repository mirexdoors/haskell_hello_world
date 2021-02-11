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