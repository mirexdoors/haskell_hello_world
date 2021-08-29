{-Определение четырёхсимвольного алфавита-}
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

{-Функция rotateN для работы с произвольным алфавитом-}
rotateN :: (Bounded a, Enum a) => Int -> a -> a
rotateN alphabetSize char = toEnum rotation
  where halfAlphabet = alphabetSize `div` 2
        offset = fromEnum char + halfAlphabet
        rotation = offset `mod` alphabetSize

{-Определение кода наибольшего начения Char-}
largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

{-Поворот для Char-}
rotChar :: Char -> Char
rotChar charToEncrypt = rotateN sizeOfAlphabet charToEncrypt
  where sizeOfAlphabet = 1 + largestCharNumber

{-Сообщение  в четырёзсимвольном алфавите-}
message :: [FourLetterAlphabet]
message = [L1, L2, L3, L1, L1, L4]

{-Определение fourLetterEncoder с помощью map-}
fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
  where
    alphaSize = 1 + fromEnum(maxBound :: FourLetterAlphabet)
    rot4l = rotateN alphaSize

{-
Кодирование в трёхсимволном алфавите
(показывает проблему с декодеированием для алфавитов нечетной длины из-за округления при вычислении половины алфавита)
-}
data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
  where
    alphaSize = 1 + fromEnum(maxBound :: ThreeLetterAlphabet)
    rot3l = rotateN alphaSize


{-Декодер для алфавитов нечётной длины-}
rotateNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotateNdecoder n c = toEnum rotation
  where halfN = n `div` 2
        offset = if even n
                 then fromEnum c + halfN
                 else 1 + fromEnum c + halfN
        rotation = offset `mod` n

{-Декодер для трёхбуквенного алфавита-}
threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3ldecoder vals
  where
    alphaSize = 1 + fromEnum(maxBound :: ThreeLetterAlphabet)
    rot3ldecoder = rotateNdecoder alphaSize

{-Функции для кодирования и декодирования строк-}
rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where alphaSize = 1 + largestCharNumber
        rotChar = rotateN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where alphaSize = 1 + largestCharNumber
        rotCharDecoder = rotateNdecoder alphaSize
