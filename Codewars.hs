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

--------------------------------------------------------------------------------
{-
Count the number of Duplicates
Write a function that will return the count of distinct case-insensitive alphabetic characters and numeric digits that occur more than once in the input string.
The input string can be assumed to contain only alphabets (both uppercase and lowercase) and numeric digits.
-}
--duplicateCount :: String -> Int
--duplicateCount = length . filter (>1) . map length . group . sort . map toLower
duplicateCount =length . filter (>1) .  map length . group . sort . map Char.toLower

------------------------------------------------
{-
Write a small function that returns the values of an array that are not odd.

All values in the array will be integers. Return the good values in the order they are given.

noOdds :: Integral n => [n] -> [n]
-}
noOdds :: Integral n => [n] -> [n]
noOdds = filter (even)

---------------------------------------------------
{-
A simple kata, my first.
simply tranform an array into a string, like so:

  transform [ 5, 7, 8, 9, 0, 5 ] -> "578905"
-}

transform :: Show a => [a] -> String
transform [] = ""
transform (x:xs) = show x ++ transform xs

---------------------------------------------------
{-
#Find the missing letter

Write a method that takes an array of consecutive (increasing) letters as input and that returns the missing letter in the array.

You will always get an valid array. And it will be always exactly one letter be missing. The length of the array will always be at least 2.
The array will always contain letters in only one case.

Example:

['a','b','c','d','f'] -> 'e' ['O','Q','R','S'] -> 'P'

["a","b","c","d","f"] -> "e"
["O","Q","R","S"] -> "P"
(Use the English alphabet with 26 letters!)

Have fun coding it and please don't forget to vote and rank this kata! :-)

I have also created other katas. Take a look if you enjoyed this kata!
-}

getNextLetter :: Char -> Char
getNextLetter c = Char.chr (Char.ord c + 1)

findMissingLetter :: [Char] -> Char
findMissingLetter (c:cs) = if (head cs == getNextLetter c) then findMissingLetter cs else getNextLetter c

--------------------------------

{-
Your task is to construct a building which will be a pile of n cubes.
 The cube at the bottom will have a volume of n^3, the cube above will have volume of (n-1)^3 and so on until the top which will have a volume of 1^3.

You are given the total volume m of the building. Being given m can you find the number n of cubes you will have to build?

The parameter of the function findNb (find_nb, find-nb, findNb) will be an integer m and you have to return the integer n such as n^3 + (n-1)^3 + ... + 1^3 = m if such a n exists or -1 if there is no such n.

Examples:
findNb(1071225) --> 45

findNb(91716553919377) --> -1
-}

findNb :: Integer -> Integer
findNb m
    | div (root * (root + 1)) 2 ^ 2 == m = root
    | otherwise = -1
    where
        intSqrt = floor . sqrt . fromIntegral
        root = intSqrt (intSqrt m * 2)

-----------------------------------------------------

{-
Imagine a triangle of numbers which follows this pattern:

Starting with the number "1", "1" is positioned at the top of the triangle. As this is the 1st row, it can only support a single number.
The 2nd row can support the next 2 numbers: "2" and "3"
Likewise, the 3rd row, can only support the next 3 numbers: "4", "5", "6"
And so on; this pattern continues.
        1
       2 3
      4 5 6
     7 8 9 10
   11 12 13 14 15
 16
...
Given N, return the sum of all numbers on the Nth Row:

1 - (1,1) 0
2-2 (2,3) 1
3- (4,6) 2
4   (7,10) 3
5 - (11,15) 4
6 - 16,21  5

1 <= N <= 10,000-}

cumulativeTriangle :: Integer -> Integer
cumulativeTriangle n = sumRange (getStartSequence n)

--сначала напишем функцию, считающую сумму в промежутке
sumRange :: (Integer, Integer) -> Integer
sumRange (a,b) =  if a < b then a + sumRange ((a + 1), b) else b

getStartSequence :: Integer -> (Integer, Integer)
getStartSequence 1 = (1,1)
getStartSequence 2 = (2,3)
getStartSequence a = (,) ((fst (getStartSequence (a - 1))) + (a - 1)) (snd (getStartSequence (a - 1)) + a)

------------------------------------------------------------------
{-
We need prime numbers and we need them now!

Write a method that takes a maximum bound and returns all primes up to and including the maximum bound.

For example,

11 => [2, 3, 5, 7, 11]-}

prime :: Int -> [Int]
prime n = [x | x <- [2..n], length (factors x) < 3 ]

-- найдем все делители числа
factors n = [x | x <- [1..n], mod n x == 0]
--------------------------------------------
{-
Write a function that takes a string of parentheses, and determines if the order of the parentheses is valid.
 The function should return true if the string is valid, and false if it's invalid.-}

validParentheses :: String -> Bool
validParentheses = error "todo: validParentheses"


----------------------------------------
{-
Given a number, find the permutation with the smallest absolute value (no leading zeros).

-20 => -20
-32 => -23
0 => 0
10 => 10
29394 => 23499
The input will always be an integer.-}

minPermutation :: Int -> Int
minPermutation = scanString . moveZero . sort . show

moveZero :: [Char] -> [Char]
moveZero [x] = [x]
moveZero ('-': xs) = '-' : moveZero xs
moveZero (x:xs) = moveZ 0  (x:xs)

moveZ :: Int -> [Char] -> [Char]
moveZ n (x:xs) = if x == '0' then  moveZ (n + 1) xs else ((x : (take  n (repeat '0'))) ++ xs)
moveZ 0 [x] = [x]


scanChar :: Char -> Int
scanChar c | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
           | otherwise = -1

scanString :: String -> Int
scanString = go 0
    where go a [] = a
          go a (x:xs) | 0 <= sc && sc <= 9 = go (10*a + sc) xs
                      | otherwise = (-1) * go a (xs)
              where sc = scanChar x
-------------------------------------------------

{-fix is a nice little function:

fix :: (a -> a) -> a
fix f = let x = f x in x
But let's make it nicer!

Code length:
27 characters or less (module line included)-}

fix :: (a -> a) -> a
fix = fix id
