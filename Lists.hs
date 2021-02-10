oddsOnly =  filter (odd)

isPalindrom :: Eq a => [a] -> Bool
isPalindrom list = list == reverse list where
  reverse tmpList = rev tmpList [] where
    rev [] b = b
    rev (x:xs) b = rev xs (x:b)