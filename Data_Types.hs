{-
Тип данных Color определен следующим образом

data Color = Red | Green | Blue
Определите экземпляр класса Show для типа Color, сопоставляющий каждому из трех цветов его текстовое представление.

GHCi> show Red
"Red"-}


data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"


data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Warning = GT
cmp Error Info = GT


cmp Warning Error = LT
cmp Warning Info  = GT

cmp Info Warning = LT
cmp Info Error = LT

cmp Info Info = EQ
cmp Error Error = EQ
cmp Warning Warning = EQ

-----------------------------------------------------
{-
Пусть объявлен следующий тип данных:

data Result = Fail | Success


И допустим определен некоторый тип данных SomeData и некоторая функция
doSomeWork :: SomeData -> (Result,Int)
возвращающая результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха.
Определите функцию processData, которая вызывает doSomeWork и возвращает строку "Success" в случае ее успешного завершения,
 либо строку "Fail: N" в случае неудачи, где N — код ошибки.-}

data Result = Fail | Success

data SomeData

doSomeWork :: SomeData -> (Result,Int)
doSomeWork = undefined

processData :: SomeData -> String
processData d = case doSomeWork d of
                        (Success, _) -> "Success"
                        (Fail, errNum) -> "Fail: " ++ show errNum


------------------------------------------------------------

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point a b) (Point x y) = sqrt ((a - x)**2 + (b - y)**2)

-------------------------------------------------------------
{-Определим тип фигур Shape:

data Shape = Circle Double | Rectangle Double Double
У него два конструктора: Circle r — окружность радиуса r, и Rectangle a b — прямоугольник с размерами сторон a и b.
 Реализуйте функцию area, возвращающую площадь фигуры.-}

--data Shape = Circle Double | Rectangle Double Double deriving Show
--
--area :: Shape -> Double
--area (Circle r) = pi * (r ** 2)
--area (Rectangle a b) = a * b

----------------------------------------------------------------

{-
Функция doSomeWork возвращала результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха.
Такое определение функции не является наилучшим, так как в случае успеха мы вынуждены возвращать некоторое значение, которое не несет никакой смысловой нагрузки.

Используя функцию doSomeWork, определите функцию doSomeWork' так, чтобы она возвращала код ошибки только в случае неудачи.
 Для этого необходимо определить тип Result'.
 Кроме того, определите instance Show для Result' так, чтобы show возвращал "Success" в случае успеха и "Fail: N" в случае неудачи, где N — код ошибки.-}

--data Result = Fail | Success
--data Result' = Result Int
--
--instance Show Result' where
--    show (Result n) = case n of
--                       0 -> "Success"
--                       otherwise -> "Fail: " ++ show n
--
--doSomeWork' :: SomeData -> Result'
--doSomeWork' someData = case doSomeWork someData of
--                          (Success, 0)  -> (Result 0)
--                          (Fail, n )  -> (Result n)

----------------------------------------------------------------------------------------------

--Реализуйте функцию isSquare, проверяющую является ли фигура квадратом.

data Shape = Circle Double | Rectangle Double Double
  deriving Show

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Circle a) = False
isSquare (Rectangle a b) | a == b = True
						 | otherwise = False
