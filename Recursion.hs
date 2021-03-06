doubleFact :: Integer -> Integer
doubleFact n = if n < 1 then 1 else n * doubleFact (n - 2)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n == 0 = 1
			| n > 0 = n * factorial (n - 1)
			| otherwise = error "arg must be >= 0"

factorial' n  | n >= 0 = helper 1 n
			  |	otherwise = error "arg must be >= 0"

helper acc 0 = acc
helper acc n = helper (acc * n)	(n - 1)


fibonacci :: Int -> Int
fibonacci n | n == 0 = 0
			| n == 1 = 1
			| n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
			| n == (- 1) = 1
			| otherwise = fibonacci (n + 2) - fibonacci (n + 1)

--Реализация функции для вычисления числа Фибоначчи, основанная на прямом рекурсивном определении, крайне неэффективна -
--количество вызовов функции растет экспоненциально с ростом значения аргумента.
--GHCi позволяет отслеживать использование памяти и затраты времени на вычисление выражения, для этого следует
--выполнить команду :set +s:
--GHCi> :set +s
--GHCi> fibonacci 30
--832040
--(8.36 secs, 298293400 bytes)
--С помощью механизма аккумуляторов попробуйте написать более эффективную реализацию, имеющую линейную сложность
--(по числу рекурсивных вызовов). Как и в предыдущем задании, функция должна быть определена для всех целых чисел.

fibonacci2 :: Integer -> Integer
fibonacci2 n = helper2 0 1 n

helper2 :: Integer -> Integer -> Integer -> Integer
helper2 acc1 acc2 n  | n == 0 = acc1
                    | n > 0 = helper2 (acc1 + acc2) acc1 (n - 1)
                    | n < 0 = helper2 acc2 (acc1 - acc2) (n + 1)


seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | n > 2  =  let
                     recurrently acc1 acc2 acc3 0 = acc1
                     recurrently acc1 acc2 acc3 n = recurrently acc2 acc3 ((acc3 + acc2) - 2*acc1) (n-1)
                   in recurrently 1 2 3 n


--Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = if x > 0 then helper x 0 0 else helper (-x) 0 0
    where
        helper 0 0 0 = (0, 1)
        helper 0 sum count = (sum, count)
        helper x sum count = helper (x `div` 10) (sum + x `mod` 10) (count + 1)


--Реализуйте функцию, находящую значение определённого интеграла от заданной функции ff на заданном интервале [a,b][a,b] методом трапеций.
-- (Используйте равномерную сетку; достаточно 1000 элементарных отрезков.)
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = integ f a b 1000 where
	integ f a b 0 = 0
	integ f a b n = h * (f (a) + f (a + h)) / 2 + integ f (a + h) b (n - 1) where h = (b - a) / n
