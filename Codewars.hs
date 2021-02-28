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

----------------------------------------------------------------------------------------------------------------

--In this little assignment you are given a string of space separated numbers, and have to return the highest and lowest number.
--
--Example:
--
--highAndLow "1 2 3 4 5")  # return "5 1"
--highAndLow "1 2 -3 4 5") # return "5 -3"
--highAndLow "1 9 3 4 -5") # return "9 -5"
--Notes:
--
--All numbers are valid Int32, no need to validate them.
--There will always be at least one number in the input string.
--Output string must be two numbers separated by a single space, and highest number is first.

highAndLow :: String -> String
highAndLow input =(show::Int->String) (maximum (getIntList input)) ++ " " ++ (show::Int->String) (minimum (getIntList input))

getIntList :: [Char] -> [Int]
getIntList input = map (read::String->Int) (words input)

-----------------------------------------------------------------------------------------------------------
--Create a function named divisors/Divisors that takes an integer n > 1 and returns an array with all of the integer's divisors
--(except for 1 and the number itself), from smallest to largest.
--If the number is prime return the string '(integer) is prime' (null in C#) (use Either String a in Haskell and Result<Vec<u32>, String> in Rust).

--Example:
--divisors 12   -- should return Right [2,3,4,6]
--divisors 25   -- should return Right [5]
--divisors 13   -- should return Left "13 is prime"

divisors :: (Show a, Integral a) => a -> Either String [a]
divisors a | a < 2 = Left "arg must be > 1"
           | a == 2 = Left "2 is prime"
           | length (getDivisors a) == 0 = Left $ show a ++ " is prime"
           | otherwise = Right $ getDivisors a


getDivisors a = [x | x <- [2..a`div`2], a `rem` x == 0]
-----------------------------------------------------------------------------------------------------------
--There is an array with some numbers. All numbers are equal except for one. Try to find it!
--
--getUnique [1, 1, 1, 2, 1, 1] -- Result is 2
--getUnique [0, 0, 0.55, 0, 0] -- Result is 0.55
--It’s guaranteed that array contains at least 3 numbers.
--
--The tests contain some very huge arrays, so think about performance.


isMember x [] = False
isMember x (y : ys)
   | x == y = True
   | otherwise = isMember x ys

isMemberTwice x [] = False
isMemberTwice x (y : ys)
   | x == y =  True && isMember x ys
   | otherwise = isMemberTwice x ys


getUnique :: [Int] -> Int
getUnique list = head $ filter (\x -> not (isMemberTwice x list)) list

---------------------------------------------------------------------------------------------------------------
--Digital root is the recursive sum of all the digits in a number.
--
--Given n, take the sum of the digits of n.
--If that value has more than one digit, continue reducing in this way until a single-digit number is produced. The input will be a non-negative integer.


digs :: Integral a => a -> [a]
digs 0 = []
digs n = digs (n `div` 10) ++ [n `mod` 10]

--digitalRoot :: Integral a => a -> a
digitalRoot x  | length ((show::Integer->String) (sum (digs x))) < 2 = sum (digs x)
               | otherwise = digitalRoot (sum (digs x))

{-
Let us consider this example (array written in general format):

ls = [0, 1, 3, 6, 10]

Its following parts:

ls = [0, 1, 3, 6, 10]
ls = [1, 3, 6, 10]
ls = [3, 6, 10]
ls = [6, 10]
ls = [10]
ls = []
The corresponding sums are (put together in a list): [20, 20, 19, 16, 10, 0]

The function parts_sums (or its variants in other languages) will take as parameter a list ls and return a list of the sums of its parts as defined above.
-}
partsSum :: [Integer] -> [Integer]
partsSum [] = [0]
partsSum list = scanr (+) 0 list

-- bad solution with lazy evals
--partsSum list (head list + (sum (tail list))) : partsSum  (tail list)
---------------------------------------------------------------------------------
{-
Write a function toWeirdCase (weirdcase in Ruby) that accepts a string, and returns the same string with all even indexed characters in each word upper cased,
and all odd indexed characters in each word lower cased. The indexing just explained is zero based, so the zero-ith index is even, therefore that character should be upper cased.

The passed in string will only consist of alphabetical characters and spaces(' ').
 Spaces will only be present if there are multiple words. Words will be separated by a single space(' ').

Examples:
toWeirdCase "String"            `shouldBe` "StRiNg"
toWeirdCase "Weird string case" `shouldBe` "WeIrD StRiNg CaSe"
-}

toWeirdCase :: String -> String
toWeirdCase [] = []
toWeirdCase str = unwords (map (\x -> toWeirdCaseHelper x) (words str))

toWeirdCaseHelper :: String -> String
toWeirdCaseHelper [] = []
toWeirdCaseHelper [x] = [Char.toUpper x]
toWeirdCaseHelper (x:xs) = Char.toUpper x : Char.toLower (head xs ) : toWeirdCaseHelper (tail xs)
