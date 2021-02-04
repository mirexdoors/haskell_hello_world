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