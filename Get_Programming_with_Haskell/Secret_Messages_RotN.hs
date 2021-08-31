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



xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

--применение к паре Bool
xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

-- Нужно для работы на списках булевых значений
xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

-- создадим синоним типа  Bits для потока битов
type Bits = [Bool]

-- Вспомогательная функция для первода Int в Bool
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (reminder == 0)
               then False : intToBits' nextVal
               else True : intToBits' nextVal
  where reminder = n `mod` 2
        nextVal = n `div` 2

{-Проблемы intToBits' в том, что:
   а) Мы получаем первернутый список
   б) списки разной длины
Значения maxBits и итоговая intToBits:
-}

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where reversedBits = reverse (intToBits' n)
        missingBits = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])

-- Перевод Char в Bits
charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

-- Обратный перевод от Bits к Int
bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
  where size = length bits
        indices = [size-1, size-2 .. 0]
        trueLocations = filter (\x -> fst x == True) (zip bits indices)


bitsToChar :: Bits -> Char
bitsToChar = toEnum . bitsToInt


{-Реализация одноразвого блокнота-}

-- простой блокнот
myPad :: String
myPad = "Shhhhhhhhhh"

myPlainText :: String
myPlainText = "Haskell"

--Применение блокнота к строке
applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = map (\pair -> (fst pair) `xor` (snd pair)) (zip padBits plainTextBits)
  where padBits = map charToBits pad
        plainTextBits = map charToBits plainText

--кодирование строки блокнота
applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar bitList
  where bitList = applyOTP' pad plainText

--Функция  encider-decoder и частичное применение
encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad


{-Класс Cipher -  позволяет создать общий интерфейс для новых шифров, а также упростить работу с существующими-}
class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data ROT = Rot

instance Cipher ROT where
  encode Rot  = rotEncoder
  decode Rot  = rotDecoder


data OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

-- Линейный конгруэнтный генератор псевдослучайных чисел
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a*seed + b) `mod` maxNumber
