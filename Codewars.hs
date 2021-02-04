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
import qualified Data.Char as Char

findNeedle :: [String] -> String
findNeedle list= "found the needle at position " ++ show (fromJust $ elemIndex "needle" list)

----------------------------------------------------------------------------------------------------------------

--
--Given an array of integers, return a new array with each value doubled.
--
--For example:
--
--[1, 2, 3] --> [2, 4, 6]
--
--For the beginner, try to use the map method - it comes in very handy quite a lot so is a good one to know.

maps :: [Int] -> [Int]
maps = map (2*)
----------------------------------------------------------------------------------------------------------------------

--Complete the solution so that it reverses the string passed into it.
solution :: String -> String
solution = reverse

----------------------------------------------------------------------------------------------------------------------

--Description
--We need a function that can transform a string into a number. What ways of achieving this do you know?
--
--Note: Don't worry, all inputs will be strings, and every string is a perfectly valid representation of an integral number.
--
--Examples
--stringToNumber "1234" == 1234
--stringToNumber "605"  == 605
--stringToNumber "1405" == 1405
--stringToNumber "-7"   == -7

stringToNumber :: String -> Integer
stringToNumber str = read str :: Integer

--------------------------------------------------------------------------------------------------------------------------

--Trolls are attacking your comment section!
--
--A common way to deal with this situation is to remove all of the vowels from the trolls' comments, neutralizing the threat.
--
--Your task is to write a function that takes a string and return a new string with all vowels removed.
--
--For example, the string "This website is for losers LOL!" would become "Ths wbst s fr lsrs LL!".
--
--Note: for this kata y isn't considered a vowel.

disemvowel :: String -> String
disemvowel [] = []
disemvowel (x:xs) |not( x `elem` "aeiouAEIOU") = x: disemvowel xs
                  |otherwise = disemvowel xs

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

----------------------------------------------------------------------------------------------------------------
--Jaden Smith, the son of Will Smith, is the star of films such as The Karate Kid (2010) and After Earth (2013).
--Jaden is also known for some of his philosophy that he delivers via Twitter.
--When writing on Twitter, he is known for almost always capitalizing every word. For simplicity, you'll have to capitalize each word, check out how contractions are expected to be in the example below.
--
--Your task is to convert strings to how they would be written by Jaden Smith.
-- The strings are actual quotes from Jaden Smith, but they are not capitalized in the same way he originally typed them.


toJadenCase :: String -> String
toJadenCase [] = []
toJadenCase js = unwords $ map capWord $ words js

capWord word = [Char.toUpper $ head word] ++ (map Char.toLower $ tail word)
