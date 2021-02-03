--Can you find the needle in the haystack?
--
--Write a function findNeedle() that takes an array full of junk but containing one "needle"
--
--After your function finds the needle it should return a message (as a string) that says:
--
--"found the needle at position " plus the index it found the needle, so:
--
--findNeedle ["hay", "junk", "hay", "hay", "moreJunk", "needle", "randomJunk"]
--should return "found the needle at position 5"
import Data.Maybe
import Data.List

findNeedle :: [String] -> String
findNeedle list= "found the needle at position " ++ show (fromJust $ elemIndex "needle" list)

----------------------------------------------------------------------------------------------------------------
--Write a program that finds the summation of every number from 1 to num.
-- The number will always be a positive integer greater than 0.
summation :: Integer -> Integer
summation n = div (n*(n + 1)) 2

summation' :: Integer -> Integer
summation' n  =  summHelper 0 n

summHelper acc 0 = acc
summHelper acc n =  summHelper (acc + n) (n -1)

----------------------------------------------------------------------------------------------------------------

--The museum of incredible dull things
--The museum of incredible dull things wants to get rid of some exhibitions. Miriam, the interior architect, comes up with a plan to remove the most boring exhibitions. She gives them a rating, and then removes the one with the lowest rating.
--
--However, just as she finished rating all exhibitions, she's off to an important fair, so she asks you to write a program that tells her the ratings of the items after one removed the lowest one. Fair enough.
--
--Task
--Given an array of integers, remove the smallest value. Do not mutate the original array/list. If there are multiple elements with the same value, remove the one with a lower index. If you get an empty array/list, return an empty array/list.
--
--Don't change the order of the elements that are left.
removeSmallest :: [Int] -> [Int]
removeSmallest [] = []
removeSmallest [x] = [x]
removeSmallest list  = delete (minimum list) list

----------------------------------------------------------------------------------------------------------------
--Write a function called repeat_str which repeats the given string src exactly count times.


repeatStr :: Int -> String -> String
repeatStr n str | n == 1 = str
                | n > 1  = str ++ repeatStr (n - 1) str
